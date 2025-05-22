#' Estimate formant values using the Burg algorithm
#'
#' Wrapper for Praat procedure that estimates formant values from an audio file
#' or list of audio files.
#'
#' @param filename String giving the path to an audio file or a directory of
#' audio files. Alternatively a vector of strings giving the paths to audio
#' files.
#' @param output String giving the desired output format. Should be either
#' `ssff` (default) for a list with the Simple Signal File Format or `df` for a
#' data frame.
#' @param ext String giving the file extension for audio files; default is
#' `.wav`. Only used if `filename` is a directory.
#' @param timeStep Numeric giving the desired sampling frequency in
#' seconds. Default is
#' `NULL`, in which case `timeStep` corresponds to `windowLength * 0.25`.
#' @param start Numeric giving the desired start time of analysis in
#' seconds. Default is `NULL`, equivalent to the start of the audio file.
#' @param end Numeric giving the desired end time of analysis in seconds.
#' Default is `NULL`, equivalent to the duration of the audio file.
#' @param times Optional numeric vector giving the times to return signal
#' values from. Default is `NULL`.
#' @param maxNoFormants Integer giving the maximum number of formants to
#' estimate. Default is `5`.
#' @param maxFormant Numeric giving the upper bound of the frequency range
#' where formants are searched for in Hz. Default is `5500`.
#' @param windowLength Numeric giving the desired window length in seconds.
#' Default is `0.025`.
#' @param preEmphFrom Numeric giving the lower frequency bound used for
#' spectral pre-emphasis. Default is `50`.
#' @param returnBw Boolean; should formant bandwidths be returned? Default is
#' `FALSE`.
#' @param returnIntensity Boolean; should intensity values be returned?
#' Default is `FALSE`.
#' @param track Numeric giving the number of formants to return if a Viterbi
#' tracking algorithm is applied to the output. Default is `NULL`, in which
#' case Viterbi tracking isn't applied.
#' @param refF1 Numeric giving the reference frequency for F1 to be used for
#' Viterbi tracking. Default is `550`.
#' @param refF2 Numeric giving the reference frequency for F2 to be used for
#' Viterbi tracking. Default is `1650`.
#' @param refF3 Numeric giving the reference frequency for F3 to be used for
#' Viterbi tracking. Default is `2750`.
#' @param refF4 Numeric giving the reference frequency for F4 to be used for
#' Viterbi tracking. Default is `3850`.
#' @param refF5 Numeric giving the reference frequency for F4 to be used for
#' Viterbi tracking. Default is `4950`.
#' @param freqCost Numeric specifying the local cost of values deviating
#' from the reference frequency per kHz. Default is `1`. Only used with
#' Viterbi tracking.
#' @param bwCost Numeric specifying the cost of bandwidths relative to formant
#' frequency. Default is `1`. Only used with Viterbi tarcking.
#' @param transitionCost Numeric specifying the cost per octave of having
#' different consecutive formant values in a track. Default is `1`.
#' Only used with Viterbi tracking.
#' @param toFile Boolean; if the output format is `ssff`, should these
#' objects be saved to disk as individual files? Default is `FALSE`.
#' @param outputDir String giving the location of the output directory of
#' SSFF files when `toFile = TRUE`. Default is to use the current working
#' directory.
#' @param outputExt String giving the file extension of SSFF files when
#' `toFile = TRUE`. Default is `.fmt`.
#'
#' @returns A list of class `AsspDataObj` or data frame depending on the value
#' of `output`.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' fmt <- formantBurg(soundFile)
formantBurg <- function(filename, output = 'ssff', ext = '.wav',
                        timeStep = NULL, start = NULL, end = NULL, times = NULL,
                        maxNoFormants = 5, maxFormant = 5500,
                        windowLength = 0.025, preEmphFrom = 50,
                        returnBw = FALSE, returnIntensity = FALSE,
                        track = NULL, refF1 = 550, refF2 = 1650, refF3 = 2750,
                        refF4 = 3850, refF5 = 4950, freqCost = 1, bwCost = 1,
                        transitionCost = 1, toFile = FALSE,
                        outputDir = getwd(), outputExt = '.fmt') {

  if (!output %in% c('df', 'ssff')) stop(
    'output should be either df or ssff')

  if (any(dir.exists(filename))) filename <-
      list.files(filename, pattern = ext)
  if (length(filename) > 1 & output == 'ssff' & !toFile) stop(
    'When processing multiple files with ssff output, toFile must be TRUE')

  p <- reticulate::import('parselmouth')

  for (f in filename) {
    fm <- p$read(f)$extract_part(start, end, preserve_times=T)$to_formant_burg(
      time_step = timeStep, max_number_of_formants = maxNoFormants,
      maximum_formant = maxFormant, window_length = windowLength,
      pre_emphasis_from = preEmphFrom)
    if (!is.null(track)) fm <- p$praat$call(
      fm, 'Track', track, refF1, refF2, refF3, refF4, refF5,
      freqCost, bwCost, transitionCost)
    tmp <- formant2table(fm, f, returnBw, returnIntensity, times)

    if (output == 'ssff') {
      ado <- formant2ssff(fm, tmp, returnBw, returnIntensity)
      if (!toFile) {
        out <- ado
      } else {
        wrassp::write.AsspDataObj(ado, file.path(outputDir,
                                                 gsub(ext, outputExt, f)))
      }
    } else if (output == 'df') {
      if ('out' %in% ls()) {
        out <- rbind(out, tmp)
      } else {
        out <- tmp
      }
    }
  }

  if (!toFile) return(out)

}
