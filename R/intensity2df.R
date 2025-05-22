#' Format Intensity object as data frame
#'
#' Convert Praat Intensity object (as loaded in memory through the Python
#' interface `parselmouth`) to data frame.
#'
#' @param rms `parselmouth.Intensity` object
#' @param filename String giving the file name of audio file.
#' @param times Optional numeric vector giving the times to return signal
#' values from.
#'
#' @returns Data frame with root-mean-squared amplitude values.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' rms_df <- intensity(soundFile, output = 'df')
intensity2df <- function(rms, filename, times = NULL) {
  if (is.null(times)) {
    fn <- rep(filename, rms$n_frames)
    t <- rms$ts()
    val <- t(rms$values)
  } else {
    fn <- rep(filename, length(times))
    t <- times
    val <- c()
    for (i in 1:length(times)) {
      val <- c(val, rms$get_value(time = times[i]))
    }
  }
  out <- data.frame(file = fn, t = t, rms = val)
}
