#' Format Formant object as table
#'
#' Convert Praat Formant object (as loaded in memory through the Python
#' interface `parselmouth`) to data frame.
#'
#' @param fm `parselmouth.Formant` object
#' @param filename String giving the file name of audio file.
#' @param returnBw Boolean; should formant bandwidths be returned? Default is
#' `FALSE`.
#' @param returnIntensity Boolean; should intensity values be returned?
#' Default is `FALSE`.
#' @param times Optional numeric vector giving the times to return signal
#' values from.
#'
#' @returns Data frame with formant values.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' fm_df <- formantBurg(soundFile, output = 'df')
formant2table <- function(fm, filename,
                          returnBw = FALSE, returnIntensity = FALSE,
                          times = NULL) {
  p <- reticulate::import('parselmouth')

  if (is.null(times)) {
    out <- p$praat$call(fm, 'List', 0, 1, 3, returnIntensity,
                        3, 1, 3, returnBw) |>
      utils::read.table(text = _, header = TRUE)
    colnames(out) <- gsub('.Hz.', '', colnames(out))
    for (x in 4:ncol(out)) {
      if (!is.numeric(out[,x])) out[,x] <- suppressWarnings(as.numeric(out[,x]))
    }
    colnames(out)[1] <- 't'
    tmp <- cbind(file = rep(filename, nrow(out)), out)
  } else {
    fn <- rep(filename, length(times))
    t <- times
    nFmt <- p$praat$call(fm, 'Get maximum number of formants')
    out <- data.frame(file = fn, t = t)
    fmtVal <- c()
    for (fmt in 1:nFmt) {
      for (i in 1:length(times)) {
        fmtVal[i] <- fm$get_value_at_time(formant_number = as.integer(fmt),
                                       time = times[i])
      }
      out[,paste0('F', fmt)] <- fmtVal
    }
    if (returnBw) {
      for (fmt in 1:nFmt) {
        for (i in 1:length(times)) {
          bwVal <- fm$get_bandwidth_at_time(formant_number = as.integer(fmt),
                                            time = times[i])
        }
        out[,paste0('B', fmt)] <- bwVal
      }
    }
  }
  return(out)
}
