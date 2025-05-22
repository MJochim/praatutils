#' Generate spectrogram
#'
#' Wrapper for Praat procedure that generates a matrix of energy values by
#' frequency bin and time from an audio file using the fast Fourier transform.
#'
#' @param filename String giving the path to an audio file.
#' @param start Numeric giving the desired start time of analysis in
#' seconds. Default is `NULL`, equivalent to the start of the audio file.
#' @param end Numeric giving the desired end time of analysis in seconds.
#' Default is `NULL`, equivalent to the duration of the audio file.
#' @param windowLength Numeric giving the duration of analysis windows in
#' seconds. Default is `0.005`.
#' @param maxFreq Numeric giving the maximum frequency subject to analysis.
#' Default is `5000`.
#' @param timeStep Numeric giving the desired sampling frequency in
#' seconds. Default is `0.002`.
#' @param freqStep Numeric giving the desired frequency resolution in Hz.
#' Default is `20`.
#' @param windowShape String giving the window shape to use for the analysis.
#' Default is `GAUSSIAN`; other options are `BARTLETT`, `HAMMING`, `HANNING`,
#' `SQUARE`, and `WELCH`.
#'
#' @returns A matrix.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' spec <- spectrogram(soundFile)
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
