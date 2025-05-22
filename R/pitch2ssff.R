#' Format Pitch object as SSFF
#'
#' Convert Praat Pitch object (as loaded in memory through the Python
#' interface `parselmouth`) to `list` object with the Simple Signal File Format.
#'
#' @param pit `parselmouth.Pitch` object
#'
#' @returns List object of class `AsspDataObj`.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' pit_ssff <- pitchRawAC(soundFile, output = 'ssff')
pitch2ssff <- function(pit) {
  ado <- list()
  attr(ado, 'sampleRate') <- 1 / pit$time_step
  attr(ado, 'origFreq') <- 0
  attr(ado, 'startTime') <- pit$x1
  attr(ado, 'startRecord') <- integer(1)
  attr(ado, 'endRecord') <- pit$n_frames
  class(ado) <- 'AsspDataObj'
  wrassp::AsspFileFormat(ado) <- 'SSFF'
  wrassp::AsspDataFormat(ado) <- as.integer(2)
  ado <- wrassp::addTrack(ado, 'F0', as.matrix(pit$to_matrix()$values[1,]),
                          format='REAL32')
  attr(ado, 'trackFormats') <- 'REAL32'
  return(ado)
}
