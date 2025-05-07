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
