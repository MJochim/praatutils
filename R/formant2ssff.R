formant2ssff <- function(fm, fmTab, returnBw, returnIntensity) {
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
