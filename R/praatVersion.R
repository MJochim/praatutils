#' Get Praat version
#'
#' Print the version of Praat used by the current `parselmouth` installation
#'
#' @returns Used for side effects
#' @export
#'
#' @examples
#' praatVersion()
praatVersion <- function() {
  p <- reticulate::import('parselmouth')
  cat(p$PRAAT_VERSION)
}
