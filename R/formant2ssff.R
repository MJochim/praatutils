#' Format Formant object as SSFF
#'
#' Convert Praat Formant object (as loaded in memory through the Python
#' interface `parselmouth`) to `list` object with the Simple Signal File Format.
#'
#' @param fm `parselmouth.Formant` object
#' @param fmTab Table of formant values created under the hood with
#' `formantBurg`.
#' @param returnBw Boolean; should formant bandwidths be returned? Default is
#' `FALSE`.
#' @param returnIntensity Boolean; should intensity values be returned?
#' Default is `FALSE`.
#'
#' @returns List object of class `AsspDataObj`.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' fm_ssff <- formantBurg(soundFile, output = 'ssff')
formant2ssff <- function(fm, fmTab,
                         returnBw = FALSE, returnIntensity = FALSE) {
  ado <- list()
  attr(ado, 'sampleRate') <- 1 / fm$time_step
  attr(ado, 'origFreq') <- 0
  attr(ado, 'startTime') <- fm$x1
  attr(ado, 'startRecord') <- integer(1)
  attr(ado, 'endRecord') <- fm$n_frames
  class(ado) <- 'AsspDataObj'
  wrassp::AsspFileFormat(ado) <- 'SSFF'
  wrassp::AsspDataFormat(ado) <- as.integer(2)
  fmCols <- which(colnames(fmTab) %in% paste0('F', 1:20))
  fmMat <- as.matrix(fmTab[,fmCols])
  fmMat[is.na(fmMat)] <- 0
  ado <- wrassp::addTrack(ado, 'fm', fmMat)
  if (returnBw) {
    bwCols <- which(colnames(fmTab) %in% paste0('B', 1:20))
    ado <- wrassp::addTrack(ado, 'bw', as.matrix(fmTab[,bwCols]))
  }
  if (returnIntensity) wrassp::addTrack(ado, 'intensity',
                                        as.matrix(fmTab[,'intensity']))
  attr(ado, 'trackFormats') <- rep('REAL32', length(ado))
  return(ado)
}
