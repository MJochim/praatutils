#' Return file information
#'
#' Prints the default Praat information about any file
#'
#' @param file String giving the path to a file that can be read by Praat.
#'
#' @returns Used for side effects
#' @export
#'
#' @examples
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' printInfo(soundFile)
printInfo <- function(file) {
  p <- reticulate::import('parselmouth')
  p$read(file) |>
    p$praat$call('Info') |>
    strsplit('\n') |>
    unlist() |>
    cat(sep='\n')
}
