#' Format Harmonicity object as data frame
#'
#' Convert Praat Harmonicity object (as loaded in memory through the Python
#' interface `parselmouth`) to data frame.
#'
#' @param hnr `parselmouth.Harmonicity` object
#' @param filename String giving the file name of audio file.
#' @param times Optional numeric vector giving the times to return signal
#' values from.
#'
#' @returns Data frame with harmonics-to-noise-ratio values.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' hnr_df <- harmonicityAC(soundFile, output = 'df')
harmonicity2df <- function(hnr, filename, times = NULL) {
  if (is.null(times)) {
    fn <- rep(filename, hnr$n_frames)
    t <- hnr$ts()
    val <- t(hnr$values)
  } else {
    fn <- rep(filename, length(times))
    t <- times
    val <- c()
    for (i in 1:length(times)) {
      val <- c(val, hnr$get_value(time = times[i]))
    }
  }
  out <- data.frame(file = fn, t = t, hnr = val)
}
