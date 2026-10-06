#' signfig

#' @param vec a numeric vector
#' @param digits number of significant digits requested

#' @return value(s) as character with the selected number of signiciant digits
#'
#' @export

sigfig <- function(vec, digits){
  return(gsub("\\.$", "", formatC(signif(vec,digits=digits), digits=digits, format="fg", flag="#")))
}