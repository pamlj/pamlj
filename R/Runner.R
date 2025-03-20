## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
    inherit = Initer,
    cloneable = FALSE,
    class = TRUE,
    public = list(
        run = function() {
            # this is run before any table or plot is filled.
            # it produces the basic estimation required for all tables and plots
            # it fills self$data with all power parameters
            jinfo("PAMLj: Runner: checking data")

            checkdata(self)
            commonchecks(self)
            if (!self$filled) {
                self$ok <- FALSE
            }
            if (!self$ok) {
                return()
            }
            jinfo("PAMLj: Runner: first estimation")

            resobj <- try_hard(powervector(self, self$data))
            if (!isFALSE(resobj$warning)) {
                warning(resobj$warning)
            }
            if (!isFALSE(resobj$error)) {
                checkfailure(self, resobj)
                self$ok <- FALSE
                return(NULL)
            }
            # everything went well, so fill self$data
            self$data <- resobj$obj
            #   postchecks(self)
            extrainfo(self)
        },
        run_powertab = function() {
            jinfo("PAMLj: Runner: powertab")
            tab <- powertab(self)
            return(tab)
        },
        run_effectsize = function() {
            return(effectsize_run(self))
        },
        run_powerbyes = function() {
            jinfo("PAMLj: Runner: powerbyes")

            tab <- powerbyes(self)
            warning("Estimated for N=", round(self$data$n))
            return(tab)
        },
        run_powerbyn = function() {
            jinfo("PAMLj: Runner: powerbyn")

            tab <- powerbyn(self)
            warning("Estimated for ES=", format(self$data$es, digits = 5))
            return(tab)
        },
        run_powerxy = function() {
            jinfo("PAMLj: Runner: powerxy")
            r <- self$info$rxy
            f2 <- r^2 / (1 - r^2)

            tab <- pamlj.glm(
                u = 1,
                v = self$data$n - 2,
                f2 = f2,
                sig.level = self$data$sig.level,
                alternative = self$data$alternative,
                df_model = 1
            )
            tab$beta <- r
            self$info$ryxpower <- tab$power
            text <- "Power represents the attainable power in a simple regression with only X and Y, given the input parameters."
            text <- text %+% " The N is set based on the smallest mediated effect."
            warning(text)
            return(list(tab))
        },
        run_means = function() {
            exdata <- self$analysis$data
            factors <- self$options$factors
            means <- self$options$means
            sds <- self$options$sds
            if (is.null(means)) {
                return()
            }
            if (is.null(sds)) {
                return()
            }
            if (is.null(factors)) {
                return()
            }


            for (f in factors) {
                exdata[[f]] <- factor(exdata[[f]])
                contrasts(exdata[[f]]) <- contr.sum(nlevels(exdata[[f]]))
            }

            form1 <- paste(means, "~", paste(factors, collapse = "*"))
            model1 <- lm(form1, exdata)
            form2 <- paste(sds, "~", paste(factors, collapse = "*"))
            model2 <- lm(form2, exdata)

            effects <- self$info$terms
            suppressWarnings({
                tabs <- lapply(effects, function(e) {
                    form <- as.formula(paste("~", e))
                    em <- as.data.frame(emmeans::emmeans(model1, specs = form))
                    es <- as.data.frame(emmeans::emmeans(model2, specs = form))
                    for (n in names(em)) if (is.factor(em[[n]])) em[[n]] <- as.character(em[[n]])
                    w <- which(names(em) == "emmean")
                    em <- em[, 1:w]
                    em$sd <- es$emmean
                    for (i in w:1) em <- em[order(em[[i]]), ]
                    em
                })
            })
            return(tabs)
        },
        run_customtable = function() {
            ## this is filled by plotter$prepateCustom
            ## here we simply pass it to the table
            state <- self$analysis$results$powerCustom$state
            if (is.null(state)) {
                return()
            }
            return(state$data)
        },
        run_implied_covs = function() {
            mark("PAMLj SEM: implied covs run")
            model <- lavaan::sem(self$data$modelPop)
            tab <- lavaan::inspect(model, "implied")
            return(as.data.frame(tab$cov))
        },
        run_implied_lvcovs = function() {
            mark("PAMLj SEM: implied latent covs run")
            if (length(self$info$lvnames) == 0) {
                self$warning <- list(topic = "implied_lvcovs", message = "No latent variables in the model")
                return()
            }
            model <- lavaan::sem(self$data$modelPop)
            tab <- lavaan::inspect(model, "cov.lv")
            tab <- round(tab, digits = 2)
            return(as.data.frame(tab))
        },
        run_implied_betas = function() {
            mark("PAMLj SEM: implied std betas run")
            model <- lavaan::sem(self$data$modelPop)
            res <- lavaan::inspect(model, "std")
            if (!("beta" %in% names(res))) {
                self$warning <- list(topic = "implied_betas", message = "No regression coefficients in the model")
                return()
            }
            test <- apply(res$beta, 1, function(x) length(x[x == 0]))
            .order <- names(test)[order(test, decreasing = TRUE)]
            tab <- as.data.frame(res$beta)
            tab <- tab[.order, .order]
            tab <- round(tab, digits = 2)
            tab$variable <- names(tab)
            tab
        },
        endrun = function() {
            self$analysis$results$initnotes$setContent(" ")
            self$analysis$results$initnotes$setVisible(FALSE)
        }
    ), # end of public function estimate

    private = list(
        # do private stuff
    ) # end of private
) # end of class
