### text manipulation

syntax_digest<-function(s) {
  warning<-list(unique_symb=TRUE)
  # first, get the regression lines
  regressions<-.get_regression_lines(s)
  if (is.null(regressions))
      stop("Please specify a model of the form `y~1*1+1*x+(1*1|cluster)`")
  
  alist<-lapply(regressions, function(line) {
    .re    <- .findbars(line)
    if (length(.re)==0) {
      results<-decompose_formula(.nobars(line))
    } else {
      results<-decompose_formula(.nobars(line))
      results$random<-lapply(.re,function(x) {
        r<-decompose_formula(x)
        r
      })
      results$original<-s
      results$original_fixed<-results$original_rhs
      results$original_rhs<-stringr::str_split(s,"\\~")[[1]][2] 
      results$clusters<-unique(unlist(stringr::str_split(names(.re),"\\:")))
      .random<-unlist(lapply(results$random,function(x) x$rhs))
      .random<-paste(.random,names(.random),sep="|")
      .random<-paste0("(",.random,")",collapse = "+")   
      results$formula<-paste0(results$formula,"+",.random)
    }
    results
  })
  if (length(alist)==1) alist<-alist[[1]]
  alist$commands<-digest_other_lines(get_other_lines(s))
  alist
}

decompose_formula<-function(line) {
  breaks<-stringr::str_split(line,"\\~")[[1]]
  if (length(breaks)==1) {
    lhs<-NULL
    rhs<-breaks
  } else {
    lhs<-breaks[[1]]
    rhs<-breaks[[2]]
  }
  termsigns<-.get_terms_signs(rhs)
  clean<-attr(termsigns,"clean")
  attr(termsigns,"clean")<-NULL
  
  names<-get_terms_names(clean)
  termlist<-as.list(stringr::str_split(clean,stringr::fixed("+"))[[1]])
  lapply(termlist,.has_bad_terms)
  resultobj<-list(
    termlist=termlist,
    termsigns=termsigns,
    terms=names)
  resultobj$coef_symbs<-get_coefs_symb(rhs)
  resultobj$rhs<-paste0(names,collapse = "+")
  resultobj$lhs<-lhs
  resultobj$formula<-paste0(lhs,"~",paste(names,collapse = "+"))
  resultobj$coefs<-get_coefs_num(resultobj)
  resultobj$varnames<-extract_vars(resultobj$terms)
  resultobj$original<-line
  resultobj$original_rhs<-rhs
  resultobj$original_lhs<-lhs
  structure(resultobj,class="syntax_formula")
}

fix_intercept<-function(obj,avalue=NULL) {
  
  if (! "syntax_formula" %in% class(obj)) stop("Object is not syntax_class") 
  attr(obj,"fix_intercept")<-FALSE
  test<-any(sapply(obj$terms,function(x) (x=="1" || x=="0")))  
  if (!test) {
    old_commands <- obj$commands
    coef<-ifelse(is.null(avalue),"",paste0(avalue,"*"))
    line<-paste0(obj$lhs,"~",coef,"1+",obj$original_rhs)
    obj<-syntax_digest(line)
    obj$commands <- old_commands
    attr(obj,"fix_intercept")<-TRUE
  }
  if (!is.null(obj$random))
    for (i in seq_along(obj$random)) {
      obj$random[[i]]<-fix_intercept(obj$random[[i]],avalue)
    } 
  obj
}


### helper functions - internal

.split_syntax_text <- function(syntax) {
  # split on either any line break OR semicolon
  parts <- trimws(unlist(strsplit(syntax, "(?:\\R|;)", perl = TRUE)))
  parts <- sub("#.*$", "", parts)   # remove inline comments
  parts[sapply(parts,stringr::str_length)!=0]

}

.get_regression_lines <- function(syntax) {

  parts <- .split_syntax_text(syntax)
  parts<-  gsub("\\s+", "", parts, perl = TRUE)
  # detect tilde that's not part of ~~ or :=
  formula_tilde <- "(?<![~:])\\s*~\\s*(?![~=])"
  keep <- grepl(formula_tilde, parts, perl = TRUE)
  parts[keep]
}

## check that no crazy grammar is used for categorical variables
.has_bad_terms <- function(term) {
  # bad: word*[...] with nothing after ] (e.g. x*[1,2])
  # valid: word*[...]*varname (e.g. b*[1,2]*q) or [1,2]*varname
  if (grepl("\\b[A-Za-z]\\w*\\s*\\*\\s*\\[[^\\]]*\\](?!\\s*\\*\\s*[A-Za-z])", term, perl = TRUE))
    stop("Syntax for categorical variables not correct")
}


.signed_terms<-function(rhs) {
  if (!grepl("^[+-]", rhs)) rhs <- paste0("+", rhs)
  
  m <- gregexpr("\\[[^]]*\\]", rhs)
  # get the matched substrings
  blocks <- regmatches(rhs, m)
  # in each block, replace "-" with "$"
  blocks <- lapply(blocks, function(z) gsub("-", "\\$", z))
  # put modified blocks back into the string
  regmatches(rhs, m) <- blocks
  regmatches(rhs, gregexpr("[+-][^+-]*", rhs))[[1]]
}


raw_terms<-function(rhs) {
  
  gsub("[+-]","",.signed_terms(rhs))
  
}

.get_terms_signs<-function(rhs, named=FALSE) {
  
  ## first, get the signs of each term
  sign_terms<-.signed_terms(rhs)
  signs <- substr(sign_terms,1,1)
  new_form<-paste0(gsub("[-+]","",sign_terms),collapse = "+")
  new_form<-gsub("$","-",new_form,fixed = T)
  attr(signs,"clean")<-new_form
  if (named) names(signs)<-get_terms_names(rhs)
  signs
}


get_terms_names<-function(rhs, named=FALSE) {
  
  warns<-list(unique=TRUE, error=NULL)
  pat <- "(?<![[:alpha:]])[0-9]+(?:\\.[0-9]+)?(?=\\*)"
  terms<-raw_terms(rhs)
  terms<-gsub("^\\*","",gsub(pat, "", terms, perl = TRUE))
  terms<-gsub("^[^*]*\\*","",terms)
  terms<-gsub("^[^*]*\\*","",terms)
  if (named) names(terms)<-terms
  if (length(unique(terms))!=length(terms)) warns$unique<-FALSE
  if (any(terms=="")) warns$error="Some terms are not well-defined"
  ## .attr returns the object with the warns as attributes
  .attr(terms,warns)
  
}

get_coefs_num<-function(obj) {
  
  ### function to use 
  to_num <- function(x) {
    parts <- strsplit(x, ",", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    as.numeric(parts)
  }
  # traverse the list of terms
  coefs<-lapply(obj$termlist, function(x) {
    rx <- "\\[\\s*[0-9eE+\\-.,\\s]+\\s*\\]"  # bracketed numeric list(s)
    m <- gregexpr(rx, x, perl = TRUE)
    hits <- regmatches(x, m)[[1]]
    if (length(hits)>0) {
      strip_brackets <- function(x) sub("^\\s*\\[(.*)\\]\\s*$", "\\1", x, perl = TRUE)
      blocks_raw <- trimws(vapply(hits, strip_brackets, "", USE.NAMES = FALSE)) 
      coef <- to_num(blocks_raw)
    } else {
      coef<-.get_coefs_num(x)[[1]]
    }
    coef
  })
  
  signs<-as.numeric(paste0(obj$termsigns,1))
  for (i in seq_along(coefs)) coefs[[i]]<-coefs[[i]]*signs[[i]]
  names(coefs)<-obj$termnames
  if (any(is.na(coefs))) attr(coefs,"error")<-"All terms should have a coefficient"
  coefs
}


.get_coefs_num <- function(s,named=FALSE) {
  
  warns<-list(error=TRUE)
  # keep only RHS and remove spaces
  rhs <- gsub("\\s+", "", sub("^.*?~", "", s))
  terms <- strsplit(rhs, "\\+", fixed = FALSE)[[1]]
  
  # extract number before *
  coefs <- lapply(terms, function(t) {
    m <- regmatches(t, regexpr("[0-9]*\\.?[0-9]+(?=\\*)", t, perl = TRUE))
    if (length(m) && m != "") as.numeric(m) else NA_real_
  })
  if (named) names(coefs)<-get_terms_names(rhs)
  if (any(is.na(coefs))) warns$error<-TRUE
  .attr(coefs,warns)
  coefs
}




get_coefs_symb<-function(rhs,named=FALSE) {
  
      warns<-list(unique=TRUE)
      pat <- "(?<![[:alpha:]])[0-9]+(?:\\.[0-9]+)?(?=\\*)"
      terms<-raw_terms(rhs)
      terms<-gsub("^\\*","",gsub(pat, "", terms, perl = TRUE))
      pat <- "[[:alpha:]](?=\\*)"
      res<-regmatches(terms, gregexpr(pat, terms, perl = TRUE))
      res[unlist(!as.logical(sapply(res,length)))]<-NA
      symbs<-unlist(res)
      if (named) names(symbs)<-get_terms_names(rhs)
      u<-symbs[!unlist(sapply(symbs,is.na))]
      if (length(unique(u))!=length(u)) warns$unique<-FALSE
      .attr(symbs,warns)
}

### this is taken from jmvcore


extract_vars <- function(terms) {
  # split interaction terms
  parts <- strsplit(terms, ":")
  
  # flatten
  vars <- unlist(parts, use.names = FALSE)
  
  # trim spaces just in case
  vars <- gsub("^\\s+|\\s+$", "", vars)
  
  # drop empty
  vars <- vars[nzchar(vars)]
  
  # drop pure numeric tokens (including "1")
  vars <- vars[!grepl("^\\d+(\\.\\d+)?$", vars)]
  
  # unique
  unique(vars)
}






.findbars <- function(s) {
  
  if (grepl("\\|\\|", s))
    stop("The '||' syntax (uncorrelated random effects) is not supported. Please use '|' instead.")
  
  rx <- "\\([^()]*\\)"
  hits <- regmatches(s, gregexpr(rx, s, perl = TRUE))[[1]]
  if (length(hits) == 0)
    return(setNames(character(0), character(0)))
  
  # remove outer parens
  contents <- trimws(sub("^\\(|\\)$", "", hits, perl = TRUE))
  
  # keep only blocks that contain a '|'
  has_bar <- grepl("\\|", contents, perl = TRUE)
  contents <- contents[has_bar]
  if (!length(contents))
    return(setNames(character(0), character(0)))
  
  # split into left (before |) and right (after |)
  left  <- trimws(sub("\\|.*$", "", contents, perl = TRUE))
  right <- trimws(sub("^.*?\\|", "", contents, perl = TRUE))
  
  # sanitize names: drop any parentheses and surrounding spaces
  names_clean <- trimws(gsub("[()]", "", right, perl = TRUE))
  
  ## ---- expand grouping factors with "/" (nesting) ----
  out_left  <- character(0)
  out_names <- character(0)
  
  for (i in seq_along(names_clean)) {
    g <- names_clean[i]   # e.g. "cluster1/cluster2"
    l <- left[i]          # e.g. "1" or "1 + x"
    
    if (grepl("/", g, fixed = TRUE)) {
      # split by "/", remove empty pieces, trim
      parts <- strsplit(g, "/", fixed = TRUE)[[1]]
      parts <- trimws(parts[nzchar(parts)])
      
      if (length(parts) >= 1) {
        # first-level grouping factor: "cluster1"
        out_left  <- c(out_left,  l)
        out_names <- c(out_names, parts[1])
      }
      if (length(parts) >= 2) {
        # cumulative interactions: "cluster1:cluster2", "cluster1:cluster2:cluster3", ...
        for (k in 2:length(parts)) {
          gf_name  <- paste(parts[1:k], collapse = ":")
          out_left  <- c(out_left,  l)
          out_names <- c(out_names, gf_name)
        }
      }
    } else {
      # no nesting: keep as is
      out_left  <- c(out_left,  l)
      out_names <- c(out_names, g)
    }
  }
  
  out <- out_left
  names(out) <- make.unique(out_names, sep = "..")
  out
}

.extract_cluster_vars_from_re <- function(re_vec) {
  # re_vec is the output of .findbars(s), i.e. named char vector
  gfs <- names(re_vec)
  if (length(gfs) == 0)
    return(character(0))
  
  # split on ":" or "/" to get primitive variables
  parts <- strsplit(gfs, "[/:]", perl = TRUE)
  vars  <- trimws(unlist(parts))
  vars  <- vars[nzchar(vars)]   # drop empty strings, just in case
  
  unique(vars)
}


.nobars <- function(s) {
  # remove a (+|-) right before a paren block
  out <- gsub("\\s*[+\\-]\\s*\\([^()]*\\)", "", s, perl = TRUE)
  # remove a (+|-) right after a paren block
  out <- gsub("\\([^()]*\\)\\s*[+\\-]\\s*", "", out, perl = TRUE)
  # remove any remaining standalone paren blocks
  out <- gsub("\\([^()]*\\)", "", out, perl = TRUE)
  # tidy spaces and trim leading/trailing operators
  out <- gsub("\\s{2,}", " ", out, perl = TRUE)
  out <- gsub("^\\s*[+\\-]\\s*|\\s*[+\\-]\\s*$", "", out, perl = TRUE)
  trimws(out)
}





.attr<-function(obj,warns) {
    for (n in names(warns)) 
      attr(obj,n)<-warns[[n]]
    obj
}

#### other lines 

get_other_lines <- function(syntax) {
  parts <- .split_syntax_text(syntax)
  # detect tilde that's not part of ~~ or :=
  formula_tilde <- "(?<![~:])\\s*~\\s*(?![~=])"
  keep <- grepl(formula_tilde, parts, perl = TRUE)
  trimws(parts[!keep])
}


digest_other_lines <- function(lines) {
  
  if (!exists("SYNTAX_CMD") || is.null(SYNTAX_CMD)) return()
  results<-list()
  for (l in lines) {
    t <- extract_prefix(l,SYNTAX_CMD)
    if (is.something(t)) ladd(results[[t$keyword]])<- parts<-  gsub("\\s+", "", t$value, perl = TRUE)
    }
  return(results)
}

extract_prefix <- function(s, keywords) {
  # build regex with a capturing group for the keyword
  rx <- paste0("^\\s*(", paste(keywords, collapse="|"), "):\\s*(.*)$")
  
  m <- regexec(rx, s, perl = TRUE)
  hits <- regmatches(s, m)[[1]]
  
  if (length(hits) == 0)
    return(NULL)  # no match
  
  list(
    keyword = hits[2],   # captured keyword
    value   = hits[3]    # remainder of string
  )
}

#' @exportS3Method stats::coef
coef.syntax_formula<-function(obj) unlist(obj$coefs)
