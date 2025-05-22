#' Format Pitch object as data frame
#'
#' Convert Praat Pitch object (as loaded in memory through the Python
#' interface `parselmouth`) to data frame.
#'
#' @param pit `parselmouth.Pitch` object
#' @param filename String giving the file name of audio file.
#' @param times Optional numeric vector giving the times to return signal
#' values from.
#'
#' @returns Data frame with pitch values.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' pit_df <- pitchRawAC(soundFile, output = 'df')
pitch2df <- function(pit, filename, times = NULL) {
  if (is.null(times)) {
    fn <- rep(filename, pit$n_frames)
    t <- pit$ts()
    val <- pit$to_matrix()$values[1,]
    val[which(val == 0)] <- NA
  } else {
    fn <- rep(filename, length(times))
    t <- times
    val <- c()
    for (i in 1:length(times)) {
      val <- c(val, pit$get_value_at_time(times[i]))
    }
  }
  out <- data.frame(file = fn, t = t, f0 = val)
}
