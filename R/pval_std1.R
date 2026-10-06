#' Format p-values for reporting (std1)
#'
#' @param p Numeric vector of p-values.
#' @return Character vector: two decimals for p >= 0.01, three decimals for
#'   0.001 <= p < 0.01, and "<0.001" below that. `NA` stays `NA`.
#' @examples
#' pval_std1(c(0.5, 0.0456, 0.004, 0.0004, NA))
#' @export
#'
pval_std1 <- function(p) {
  stopifnot(is.numeric(p))

  out <- sprintf("%.2f", p)

  three_dec <- which(p < 0.01)
  out[three_dec] <- sprintf("%.3f", p[three_dec])   # was "%.2f"

  out[which(p < 0.001)] <- "<0.001"
  out[is.na(p)] <- NA_character_
  out
}
