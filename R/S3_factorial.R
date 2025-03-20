## checkdata:       (required) this prepares all the info required to estimate the power parameters

.checkdata.facmeans <- function(obj) {
    jinfo("PAMLj: Checkdata factorial facmeans")

    obj$ok <- FALSE

    obj$info$letter <- letter_peta2
    obj$info$esmax <- .999999
    obj$info$esmin <- .001
    obj$info$eslbound <- 0
    obj$info$alternative <- "two.sided"
    obj$info$r <- obj$options$r
    obj$info$toaes <- function(value) value / (1 - value)
    obj$info$fromaes <- function(value) value / (1 + value)
    means <- obj$options$means
    sds <- obj$options$sds
    factors <- obj$options$factors
    ## derive terms
    obj$data <- data.frame(
        power = obj$options$power,
        sig.level = obj$options$sig.level
    )

    needed <- c("Factors", "Means", "Standard deviations")
    needed <- needed[c(is.null(factors), is.null(means), is.null(sds))]
    if (length(needed)) {
        text <- "<p>Please fill in the required input:</p> <ul>"
        for (ned in needed) text <- text %+% "<li>" %+% ned %+% "</li>"
        text <- text %+% "</ul>"
        obj$warning <- list(topic = "issues", message = text, head = "info")
    }
    if (is.null(means)) {
        return()
    }
    if (is.null(sds)) {
        return()
    }
    if (is.null(factors)) {
        return()
    }

    exdata <- obj$analysis$data
    obj$ok <- TRUE
    form <- paste("means~", paste(factors, collapse = "*"))
    obj$info$terms <- attr(terms(as.formula(form)), "term.labels")


    if (nrow(stats::na.omit(exdata)) != nrow(obj$analysis$data)) {
        obj$stop("Dataset cannot contain missing values")
    }

    obj$extradata <- exdata

    within <- unlist(obj$options$within)
    between <- setdiff(factors, within)

    if (!is.something(within)) obj$info$r <- 0

    if (nrow(exdata) > 0) {
        nlevbet <- 1
        nlevwit <- 1
        for (f in factors) {
            exdata[[f]] <- factor(exdata[[f]])
            contrasts(exdata[[f]]) <- contr.sum(nlevels(exdata[[f]]))
            if (f %in% between) {
                nlevbet <- nlevbet * nlevels(exdata[[f]])
            } else {
                nlevwit <- nlevwit * nlevels(exdata[[f]])
            }
        }

        ## we need to be sure that jamovi does not pass the data as factors
        ## like when the sd are a computed variable with one integer
        exdata[[means]] <- as.numeric(as.character(exdata[[means]]))
        exdata[[sds]] <- as.numeric(as.character(exdata[[sds]]))

        # now we start computing the SS
        form <- paste(means, "~", paste(factors, collapse = "*"))
        aa <- stats::aov(as.formula(form), data = exdata)
        sumr <- summary(aa)[[1]]
        res <- as.data.frame(sumr)
        res$source <- trimws(rownames(res))
        res <- res[res$source != "Residuals", ]
        res$type <- unlist(lapply(res$source, function(x) {
            terms <- stringr::str_split(x, ":", simplify = T)
            terms <- trimws(terms)
            test <- length(intersect(terms, within)) > 0
            if (test) {
                return("w")
            } else {
                return("b")
            }
        }))
        res$edfw <- 0
        res$edfb <- 0
        res$cell <- 0
        for (i in seq_len(nrow(res))) {
            type <- res$type[i]
            x <- res$source[i]
            terms <- stringr::str_split(x, ":", simplify = T)
            terms <- trimws(terms)
            wits <- intersect(terms, within)
            val <- 1
            for (f in wits) val <- val * (nlevels(exdata[[f]]) - 1)
            res$edfw[i] <- val
            bets <- intersect(terms, between)
            val <- 1
            for (f in bets) val <- val * (nlevels(exdata[[f]]) - 1)
            res$cell[i] <- val
            res$edfb[i] <- nlevbet - 1
        }

        ### this formulas are equivalent to standard computation of SS for mixed anova
        ### they are slightly different in order to reflect the computation
        ### of SS in car::Anova() dividing everything by the error DF.
        ### They lead to the correct partial eta-square anyway.

        for (i in seq_len(nrow(res))) {
            if (res$type[i] == "b") {
                res$ss[i] <- res$`Sum Sq`[i] / nlevbet
            } else {
                res$ss[i] <- res$cell[i] * res$`Sum Sq`[i] / (nlevbet * res$Df[i])
            }
        }
        form <- paste(sds, "~", paste(factors, collapse = "*"))
        aa <- stats::aov(as.formula(form), data = exdata)
        msds <- as.data.frame(emmeans::emmeans(aa, specs = factors))

        mse <- mean(msds$emmean^2)
        for (i in 1:nrow(res)) {
            if (res$type[i] == "w") {
                res$sigma2[i] <- mse * (1 - obj$info$r)
            } else {
                res$sigma2[i] <- mse * (1 + (nlevwit - 1) * obj$info$r)
            }
        }

        res$es <- res$ss / (res$ss + res$sigma2)
        res$n <- obj$options$n
        res$sig.level <- obj$options$sig.level
        res$power <- obj$options$power
        res$df_effect <- res$Df
        res$df_model <- sum(res$df_effect)
        obj$extradata <- res
        obj$extradata[[obj$aim]] <- NULL
        obj$extradata$id <- 1:nrow(obj$extradata)

        pwr <- powervector(obj, obj$extradata)
        ## we select the effect to focus on
        if (obj$aim == "n") {
            w <- which.max(pwr$n)
        } else {
            w <- which.min(pwr$es)
        }
        w <- w[1]
        if (length(obj$info$terms) > 1) {
            obj$warning <- list(topic = "powerbyes", message = "Sensitivity analysis is done on the smallest effect (" %+% obj$info$terms[w] %+% ")")
        }
        obj$data <- subset(obj$extradata, obj$extradata$id == w)
        obj$data[[obj$aim]] <- NULL
        obj$info$nmin <- obj$data$df_model + 10
        # at least one parameter should be empty for parameters estimation
        obj$ok <- TRUE
    } else {
        form <- as.formula(paste(means, "~", paste(factors, collapse = "*")))
        .names <- attr(terms(form), "term.labels")
        obj$data <- data.frame(
            source = .names,
            power = obj$options$power,
            sig.level = obj$options$sig.level
        )
    }
}


.checkdata.facpeta <- function(obj) {
    jinfo("PAMLj: Checkdata factorial facpeta")
    obj$info$letter <- letter_peta2
    obj$info$esmax <- .999999
    obj$info$esmin <- .001
    obj$info$eslbound <- 0
    obj$info$alternative <- "two.sided"
    obj$info$toaes <- function(value) value / (1 - value)
    obj$info$fromaes <- function(value) value / (1 + value)
    obj$data <- data.frame(
        power = obj$options$power,
        sig.level = obj$options$sig.level,
        n = obj$options$n
    )

    obj$data$es <- obj$options$peta
    design <- obj$options$effect_type
    type <- obj$options$repeated_type

    if (design == "within") {
        obj$data$type <- "w"
        obj$data$edfb <- obj$options$design_groups - 1
        # if (obj$data$edfb==0) obj$data$edfb<-1
        obj$data$edfw <- obj$options$df_effect
        obj$data$df_effect <- obj$data$edfw
        obj$data$df_model <- obj$options$design_groups - 1
        obj$data$source <- "Within"
    } else {
        obj$data$type <- "b"
        obj$data$edfb <- obj$options$design_groups - 1
        obj$data$edfw <- 1
        obj$data$source <- "Between"
        obj$data$df_model <- obj$options$design_groups - 1
        obj$data$df_effect <- obj$options$df_effect
    }


    obj$data[[obj$aim]] <- NULL
}

## powervector:     (required) pass the data, with adjutment, to the lowerlevel power functions

.powervector.factorial <- function(obj, data) {
    jinfo("PAMLj: Factorial power function")
    if (is.something(data$es)) {
        data$f2 <- obj$info$toaes(data$es)
        data$es <- NULL
    } else {
        data$f2 <- NULL
    }
    if (!is.something(data$n)) {
        data$v <- NULL
    } else {
        data[["v"]] <- data$edfw * (data$n - data$edfb - 1)
    }
    results <- lapply(1:nrow(data), function(i) {
        one <- data[i, ]
        if (one$type == "w" && obj$options$ncp_type == "model") {
            ncp <- "strict"
        } else {
            ncp <- obj$options$ncp_type
        }

        pamlj.glm(
            u = one$df_effect,
            v = one$v,
            f2 = one$f2,
            power = one$power,
            sig.level = one$sig.level,
            df_model = one$df_model,
            ncp_type = ncp,
            alternative = as.character(obj$info$alternative)
        )
    })

    results <- as.data.frame(do.call("rbind", results))
    for (i in seq_len(ncol(results))) results[[i]] <- unlist(results[[i]])
    results$es <- obj$info$fromaes(results$f2)
    odata <- data[, !(names(data) %in% names(results))]
    results <- cbind(odata, results)
    results$df1 <- results$df_effect
    results$df2 <- ceiling(results$v)
    results$n <- round((results$df2 / data$edfw) + data$edfb + 1)
    n <- results$n
    k <- data$edfb + 1
    n <- n + k / 2
    results$bn <- (n - (n %% k)) / k
    return(results)
}

## powertab_init:   (not required) this function produces or format the main table, powertab, before running
## NO NEED

## powertab:    (not required) this function produces or format the main table, powertab, after running

.powertab.facpeta <- function(obj) {
    if (any(obj$data$n != obj$data$nb)) {
        warning("N per group (N-group) is adjusted to obtain a balanced design.")
    }

    return(obj$data)
}

.powertab.factorial <- function(obj) {
    tab <- powervector(obj, obj$extradata)
    attr(tab, "titles") <- list(es = letter_peta2)
    if (any(tab$n != tab$nb)) {
        warning("N per group (N-group) is adjusted to obtain a balanced design.")
    }
    warning("Model df=", tab$df_model[1])
    return(tab)
}


## powerbyes:       this function produces or format the powerbyes table , after the estimation is done
## NO NEED
## powerbyen:       this function produces or format the powerbyen table , after the estimation is done
## NO NEED

## effectsize_init: (not required) some sub.module requires additional effect size to be computed. This function inits the table
## NO NEED

## effectsize_run:  (not required) some sub.module requires additional effect size to be computed. This function fills the table
## NO NEED

## extrainfo:       (better if is there) this function add some extra info to be given to the user when the option "explain" is selected
## TO BE IMPLEMENTED
