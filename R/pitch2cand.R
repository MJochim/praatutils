#' Format Pitch object as long data frame with candidate information
#'
#' Convert Praat Pitch object (as loaded in memory through the Python
#' interface `parselmouth`) to long-form data frame retaining information about
#' pitch candidates and their weights.
#'
#' @param pit `parselmouth.Pitch` object
#' @param filename String giving the file name of audio file.
#'
#' @returns Data frame with pitch candidate values.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' pit_cand <- pitchRawAC(soundFile, output = 'candidates')
pitch2cand <- function(pit, filename) {
  p <- reticulate::import('parselmouth')
  fn <- rep(filename, pit$n_frames)
  timesDF <- data.frame(file = fn,
                        frame = 1:pit$n_frames,
                        t = pit$ts())
  candTable <- p$praat$call(pit, 'Tabulate candidates')
  candDF <- p$praat$call(candTable, 'List', 0) |>
    utils::read.table(text = _, header = TRUE)
  out <- merge(timesDF, candDF)
}
