#' Format numbers to a number of significant digits
#'
#' Rounds numbers to the requested number of significant digits and returns
#' them as character, keeping trailing zeros (e.g. `1.5` becomes `"1.50"` with
#' 3 digits) and dropping a dangling decimal point (e.g. `"100"` rather than
#' `"100."`).
#'
#' @param vec A numeric vector.
#' @param digits Number of significant digits (a single positive integer).
#'
#' @return A character vector of the same length as `vec`. `NA` values stay `NA`.
#'
#' @examples
#' sigfig(c(0.012345, 1.5, 100, 123.456), digits = 3)
#' sigfig(c(1234.5, NA), digits = 2)
#'
#' @export
sigfig <- function(vec, digits) {
  stopifnot(
    is.numeric(vec),
    is.numeric(digits), length(digits) == 1, digits >= 1
  )

  out <- formatC(signif(vec, digits = digits),
                 digits = digits, format = "fg", flag = "#")
  out <- sub("\\.$", "", out)   # drop a trailing decimal point
  out[is.na(vec)] <- NA_character_
  out
}
