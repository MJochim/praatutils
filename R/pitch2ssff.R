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
