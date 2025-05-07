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
