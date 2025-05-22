#' Format Intensity object as SSFF
#'
#' Convert Praat Intensity object (as loaded in memory through the Python
#' interface `parselmouth`) to `list` object with the Simple Signal File Format.
#'
#' @param rms `parselmouth.Intensity` object
#'
#' @returns List object of class `AsspDataObj`.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' rms_ssff <- intensity(soundFile, output = 'ssff')
intensity2ssff <- function(rms) {
  ado <- list()
  attr(ado, 'sampleRate') <- 1 / rms$time_step
  attr(ado, 'origFreq') <- 0
  attr(ado, 'startTime') <- rms$x1
  attr(ado, 'startRecord') <- integer(1)
  attr(ado, 'endRecord') <- rms$n_frames
  class(ado) <- 'AsspDataObj'
  wrassp::AsspFileFormat(ado) <- 'SSFF'
  wrassp::AsspDataFormat(ado) <- as.integer(2)
  ado <- wrassp::addTrack(ado, 'RMS', as.matrix(t(rms$values)),
                          format='REAL32')
  attr(ado, 'trackFormats') <- 'REAL32'
  return(ado)
}
