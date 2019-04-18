#' Is length 1
#'
#' Checks if a given object is a non-list vector of length 1
#' @param x A vector. The object to be checked. No default.
#' @export
IsLength1 <- function(x) {
  return(!is.list(x) & is.vector(x) & length(x) == 1)
}