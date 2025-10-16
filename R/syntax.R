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
  
  warns<-list(unique=TRUE)
  #pat <- "(?<=\\*)\\s*[[:alpha:]_.][[:alnum:]_.]*(?::[[:alpha:]_.][[:alnum:]_.]*)*(?=(?:\\s*[+-]|\\s*$))"
#  pat <- "(?:^|[+-]|\\*)\\s*\\K[[:alpha:]_.][[:alnum:]_.]*(?::[[:alpha:]_.][[:alnum:]_.]*)*(?=(?:\\s*[+-]|\\s*$))"
  pat <- "(?<![[:alpha:]])[0-9]+(?:\\.[0-9]+)?(?=\\*)"
  terms<-raw_terms(rhs)
  terms<-gsub("^\\*","",gsub(pat, "", terms, perl = TRUE))
  terms<-gsub("^[^*]*\\*","",terms)
  if (named) names(terms)<-terms
  if (length(unique(terms))!=length(terms)) warns$unique<-FALSE
  .attr(terms,warns)

}

get_coefs_num<-function(rhs,named=FALSE) {
  
    pat <- "(?:\\d*\\.\\d+|\\d+)(?=\\s*\\*)"
    res<-regmatches(raw_terms(rhs), gregexpr(pat, raw_terms(rhs), perl = TRUE))
    res[unlist(!as.logical(sapply(res,length)))]<-NA
    res<-as.numeric(unlist(res))
    coefs<-res*as.numeric(paste0(get_terms_signs(rhs),1))
    if (named) names(coefs)<-get_terms_names(rhs)
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


fix_intercept<-function(rhs) {

  warns<-list(intadded=FALSE) 
  test<-grepl("(?<![[:alpha:]])1$", raw_terms(rhs),perl = TRUE)
  if (all(!test)) {
    rhs<-paste0("1+",rhs)
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


decompose_formula<-function(s, fix_intercept=FALSE,intercept_coef=NULL) {
  
  if (length(s)>1) return(lapply(s,decompose_formula, fix_intercept, intercept_coef))

  int_added<-FALSE
  rhs <- gsub("\\s+", "", sub(".*~", "", s))
  lhs <- gsub("\\s+", "", sub("~.*", "", s))
  if (lhs==s) lhs<-NA

  if (fix_intercept) {
    rhs<-fix_intercept(rhs)  
    if (attr(rhs,"intadded")) int_added<-TRUE
  }
  ## first, get the signs of each term
  signs <- get_terms_signs(rhs)
  ## now we get the numeric coefficients
  coefs<-  get_coefs_num(rhs)
  
  if (is.something(intercept_coef)) {
    coefs<-fix_intercept_coef(rhs)
  }
  
 
  ### now we extract symbolic coefficients
  symbs<- get_coefs_symb(rhs)
  
  # now the formula terms
  terms<-get_terms_names(rhs)
  attr(terms,"intadded")<-int_added
  ## make a formula
  rhs<-paste0(terms,collapse = "+")
  results<-list(terms=terms,coefs=coefs,symbs=symbs,rhs=rhs,lhs=lhs)
  results
}


decompose_mixed_formula<-function(s, fix_intercept=FALSE,intercept_coef=NULL) {
  
  if (length(s)>1) return(lapply(s,decompose_mixed_formula,fix_intercept,intercept_coef))
  
  results<-list()
  warns<-list()
  ### handle fixed
  .fixed <- deparse(lme4::nobars(as.formula(s)))
  .re    <- lme4::findbars(as.formula(s))
  if (is.null(.fixed)) stop("No fixed terms in the model, please refine the input model")
  if (is.null(.re)) stop("No random coefficients in the model, please refine the input")
  results$fixed<-decompose_formula(.fixed,fix_intercept = fix_intercept ,intercept_coef=intercept_coef)
  re<-lapply(.re, function(x) {
    .input<-deparse(x[[2]])
    .terms<-decompose_formula(.input,fix_intercept = fix_intercept,intercept_coef=intercept_coef)
    .terms
  })
  names(re)<-unlist(lapply(.re,function(x) as.character(x[[3]])))
  results$re<-re
  return(results)
}


.attr<-function(obj,warns) {
    for (n in names(warns)) 
      attr(obj,n)<-warns[[n]]
    obj
}


