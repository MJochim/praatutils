#' Format Harmonicity object as SSFF
#'
#' Convert Praat Harmonicity object (as loaded in memory through the Python
#' interface `parselmouth`) to `list` object with the Simple Signal File Format.
#'
#' @param hnr `parselmouth.Harmonicity` object
#'
#' @returns List object of class `AsspDataObj`.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' hnr_ssff <- harmonicityAC(soundFile, output = 'ssff')
harmonicity2ssff <- function(hnr) {
  ado <- list()
  attr(ado, 'sampleRate') <- 1 / hnr$time_step
  attr(ado, 'origFreq') <- 0
  attr(ado, 'startTime') <- hnr$x1
  attr(ado, 'startRecord') <- integer(1)
  attr(ado, 'endRecord') <- hnr$n_frames
  class(ado) <- 'AsspDataObj'
  wrassp::AsspFileFormat(ado) <- 'SSFF'
  wrassp::AsspDataFormat(ado) <- as.integer(2)
  ado <- wrassp::addTrack(ado, 'HNR', as.matrix(t(hnr$values)),
                          format='REAL32')
  attr(ado, 'trackFormats') <- 'REAL32'
  return(ado)
}
