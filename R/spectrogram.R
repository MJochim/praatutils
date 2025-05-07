spectrogram <- function(filename, start = NULL, end = NULL,
                        windowLength = 0.005, maxFreq = 5000, timeStep = 0.002,
                        freqStep = 20, windowShape = 'GAUSSIAN') {

  legalWindows <- c('BARTLETT', 'GAUSSIAN', 'HAMMING', 'HANNING',
                    'SQUARE', 'WELCH')

  if (!windowShape %in% legalWindows) stop(paste(windowShape, 'is not a known',
                                                 'window shape'))

  p <- reticulate::import('parselmouth')

  out <- list()
  spec <- p$read(filename)$extract_part(start, end, preserve_times=T)$
    to_spectrogram(window_length = windowLength,
                   maximum_frequency = maxFreq, time_step = timeStep,
                   frequency_step = freqStep, window_shape = windowShape)
  out$spec <- spec$values |> t()
  out$t <- spec$ts()
  out$freq <- spec$ys()
  return(out)

}
