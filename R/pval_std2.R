#' Format p-values for reporting (std2)
#'
#' @param p Numeric vector of p-values.
#' @return Character vector: two decimals for p >= 0.01 and "<0.01" below
#'   that. `NA` stays `NA`.
#' @examples
#' pval_std2(c(0.5, 0.0456, 0.004, 0.0004, NA))
#' @export
pval_std2 <-  function(p) {
  stopifnot(is.numeric(p))

  out <- sprintf("%.2f", p)
  out[which(p < 0.01)]  <- "<0.01"
  out[which(p < 0.001)] <- "<0.001"   # must come after the line above
  out[is.na(p)] <- NA_character_
  out
}
