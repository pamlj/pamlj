### text manipulation

split_syntax_text <- function(syntax) {
  # split on either any line break OR semicolon
  parts <- trimws(unlist(strsplit(syntax, "(?:\\R|;)", perl = TRUE)))
  parts <- sub("#.*$", "", parts)   # remove inline comments
  parts[sapply(parts,stringr::str_length)!=0]

}

get_regression_lines <- function(syntax) {
  parts <- split_syntax_text(syntax)
  # detect tilde that's not part of ~~ or :=
  formula_tilde <- "(?<![~:])\\s*~\\s*(?![~=])"
  keep <- grepl(formula_tilde, parts, perl = TRUE)
  parts[keep]
}



get_other_lines <- function(syntax) {
  parts <- split_syntax_text(syntax)
  # detect tilde that's not part of ~~ or :=
  formula_tilde <- "(?<![~:])\\s*~\\s*(?![~=])"
  
  keep <- grepl(formula_tilde, parts, perl = TRUE)
  parts[!keep]
}


## text formula manipulation 

replace_num_blocks <- function(s, letters = LETTERS) {
  rx <- "\\[\\s*[0-9eE+\\-.,\\s]+\\s*\\]"  # bracketed numeric list(s)
  
  m <- gregexpr(rx, s, perl = TRUE)
  hits <- regmatches(s, m)[[1]]
  
  if (length(hits) == 0) {
    return(list(text = s, blocks_raw = character(0), blocks_num = list()))
  }
  
  # strip brackets and split to numeric vectors
  strip_brackets <- function(x) sub("^\\s*\\[(.*)\\]\\s*$", "\\1", x, perl = TRUE)
  blocks_raw <- trimws(vapply(hits, strip_brackets, "", USE.NAMES = FALSE)) 
  to_num <- function(x) {
    parts <- strsplit(x, ",", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    as.numeric(parts)
  }
  blocks_num <- lapply(blocks_raw, to_num)
  
  n <- length(hits)
  used<-get_coefs_symb(s)
  l <- setdiff(letters,used)
  repl <- if (n <= length(l)) {
    l[seq_len(n)]
  } else {
    stop("formula to big")
  }
  
  # perform the replacement
  tmp <- s
  regmatches(tmp, m)[[1]] <- repl
  
  list(
    text = tmp,         # string with [ ... ] replaced by letters
    blocks_raw = blocks_raw,   # character blocks without brackets
    blocks_num = blocks_num,   # parsed numeric vectors
    tokens = repl              # the letters used, in order
  )
}


signed_terms<-function(rhs) {
  if (!grepl("^[+-]", rhs)) rhs <- paste0("+", rhs)
  regmatches(rhs, gregexpr("[+-][^+-]*", rhs))[[1]]
}


raw_terms<-function(rhs) {
  
  gsub("[+-]","",signed_terms(rhs))
  
}

get_terms_signs<-function(rhs, named=FALSE) {
  
  ## first, get the signs of each term
  sign_terms<-signed_terms(rhs)
  signs <- substr(sign_terms,1,1)
  new_form<-paste0(gsub("[-+]","",sign_terms),collapse = "+")
  attr(signs,"clean")<-new_form
  if (named) names(signs)<-get_terms_names(rhs)
  signs
}


get_terms_names<-function(rhs, named=FALSE) {
  
  warns<-list(unique=TRUE, error=FALSE)
  pat <- "(?<![[:alpha:]])[0-9]+(?:\\.[0-9]+)?(?=\\*)"
  terms<-raw_terms(rhs)
  terms<-gsub("^\\*","",gsub(pat, "", terms, perl = TRUE))
  terms<-gsub("^[^*]*\\*","",terms)
  terms<-gsub("^[^*]*\\*","",terms)
  if (named) names(terms)<-terms
  if (length(unique(terms))!=length(terms)) warns$unique<-FALSE
  if (any(terms=="")) warns$error=TRUE
  ## .attr returns the object with the warns as attributes
  .attr(terms,warns)
  
}

# get_coefs_num<-function(rhs,named=FALSE) {
#   
#     pat <- "(?:\\d*\\.\\d+|\\d+)(?=\\s*\\*)"
#     res<-regmatches(raw_terms(rhs), gregexpr(pat, raw_terms(rhs), perl = TRUE))
#     res[unlist(!as.logical(sapply(res,length)))]<-NA
#     res<-as.numeric(unlist(res))
#     coefs<-res*as.numeric(paste0(get_terms_signs(rhs),1))
#     if (named) names(coefs)<-get_terms_names(rhs)
#     coefs
# }


get_coefs_num <- function(s,named=FALSE) {
  
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


fix_intercept<-function(rhs,intercept_coef=0) {

  warns<-list(intadded=FALSE) 
  test<-grepl("(?<![[:alpha:]])1$", raw_terms(rhs),perl = TRUE)
  if (all(!test)) {
    rhs<-paste0(intercept_coef,"*1+",rhs)
    rhs<-gsub("+-","-",rhs, fixed = T)
    warns$intadded<-TRUE
  }
  terms <- strsplit(rhs, "\\+", fixed = FALSE)[[1]]
  first <- sub("\\+.*$", "", rhs) 
  test<-identical(first, "1")
  if (test) {
    rhs<-paste0(intercept_coef,"*",rhs)
    warns$intadded<-TRUE
  }
  .attr(rhs,warns)
}

fix_intercept_coef<-function(rhs, coef=0, named=FALSE) {
 
  warns<-list(intadded=FALSE) 
  new_rhs<-fix_intercept(rhs)
  coefs<-get_coefs_num(new_rhs,named = T)
  if (is.na(coefs[["1"]])) {
    coefs[["1"]]<-coef
    warns$intadded<-TRUE
  }
  if (!named) names(coefs)<-NULL
  .attr(coefs,warns)
}


decompose_formula<-function(s, fix_intercept=FALSE,intercept_coef=0) {
  
  if (length(s)>1) return(lapply(s,decompose_formula, fix_intercept, intercept_coef))

  int_added<-FALSE
  rhs <- gsub("\\s+", "", sub(".*~", "", s))
  lhs <- gsub("\\s+", "", sub("~.*", "", s))
  if (lhs==s) lhs<-NA

  rhsobj<-replace_num_blocks(rhs)
  rhs <- rhsobj$text
  if (fix_intercept) {
    rhs<-fix_intercept(rhs,intercept_coef)  
    if (attr(rhs,"intadded")) int_added<-TRUE
  }
  ## first, get the signs of each term
  signs <- get_terms_signs(rhs)
  ## now we get the numeric coefficients
 
  coefslist<-  get_coefs_num(attr(signs,"clean"))
  coefs_att<-attributes(coefslist)
  coefslist<-lapply(seq_along(coefslist),function(i) coefslist[[i]]*as.numeric(paste0(signs[[i]],"1")))
  
#  if (is.something(intercept_coef)) {
#    coefslist<-fix_intercept_coef(rhs)
#  }
 
 
  ### now we extract symbolic coefficients
  symbs<- get_coefs_symb(rhs)
  
  for (i in seq_along(rhsobj$tokens)) {
     w <- which(symbs==rhsobj$tokens[[i]])
     coefslist[[w]]<-rhsobj$blocks_num[[i]]
  }
  # now the formula terms
  terms<-get_terms_names(rhs)
  attr(terms,"intadded")<-int_added
  ## make a formula
  rhs<-paste0(terms,collapse = "+")
  coefs<-unlist(coefslist)
  
  attributes(coefs)<-attributes(coefslist)
  attr(coefs,"error")<-FALSE
  if (any(is.na(coefs))) attr(coefs,"error")<-TRUE
  results<-list(terms=terms,coefslist=coefslist,coefs=coefs,symbs=symbs,rhs=rhs,lhs=lhs)
  results
}

.findbars <- function(s) {
  rx <- "\\([^()]*\\)"
  hits <- regmatches(s, gregexpr(rx, s, perl = TRUE))[[1]]
  if (length(hits) == 0) return(setNames(character(0), character(0)))
  
  # remove outer parens
  contents <- trimws(sub("^\\(|\\)$", "", hits, perl = TRUE))
  
  # keep only blocks that contain a '|'
  has_bar <- grepl("\\|", contents, perl = TRUE)
  contents <- contents[has_bar]
  
  # split into left (before |) and right (after |)
  left  <- trimws(sub("\\|.*$", "", contents, perl = TRUE))
  right <- trimws(sub("^.*?\\|", "", contents, perl = TRUE))
  
  # sanitize names: drop any parentheses and surrounding spaces
  names_clean <- trimws(gsub("[()]", "", right, perl = TRUE))
  
  out <- left
  names(out) <- make.unique(names_clean, sep = "..")
  out
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



decompose_mixed_formula<-function(s, fix_intercept=FALSE,intercept_coef=0) {
  
  if (length(s)>1) return(lapply(s,decompose_mixed_formula,fix_intercept,intercept_coef))
  
  results<-list()
  warns<-list()
  ### handle fixed
  .fixed <- .nobars(s)
  .re    <- .findbars(s)
  if (is.null(.fixed)) stop("No fixed terms in the model, please refine the input model")
  if (is.null(.re)) stop("No random coefficients in the model, please refine the input")
  results$fixed<-decompose_formula(.fixed,fix_intercept = fix_intercept ,intercept_coef=intercept_coef)
  re<-lapply(.re, function(x) {
    .terms<-decompose_formula(x,fix_intercept = fix_intercept,intercept_coef=intercept_coef)
    .coefs<-unlist(lapply(.terms$coefs,function(x) x))
    .terms
  })
#  names(re)<-unlist(lapply(.re,function(x) as.character(x[[3]])))
  results$re<-re
  ## create a R formula
  .re<-unlist(lapply(re,function(x) x$rhs))
  .re<-paste(.re,names(.re),sep="|")
  .re<-paste0("(",.re,")",collapse = "+")
  .formula<-paste0(results$fixed$lhs,"~",results$fixed$rhs,"+",.re)
  results$formula<-.formula

  return(results)
}


.attr<-function(obj,warns) {
    for (n in names(warns)) 
      attr(obj,n)<-warns[[n]]
    obj
}


