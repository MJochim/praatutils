#' Estimate harmonics-to-noise ratio using raw autocorrelation
#'
#' Wrapper for Praat procedure that estimates harmonicity (harmonics-to-noise
#' ratio) values from an audio file or list of audio files based on the raw
#' autocorrelation of the signal.
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
#' seconds. Default is `0.01`.
#' @param start Numeric giving the desired start time of analysis in
#' seconds. Default is `NULL`, equivalent to the start of the audio file.
#' @param end Numeric giving the desired end time of analysis in seconds.
#' Default is `NULL`, equivalent to the duration of the audio file.
#' @param times Optional numeric vector giving the times to return signal
#' values from. Default is `NULL`.
#' @param floor Numeric giving the lowest expected pitch frequency in Hz.
#' Default is `75`.
#' @param silenceThreshold Numeric giving the threshold below which frames are
#' considered silent. The value is a proportion of the global maximum
#' amplitude. Default is `0.1`. Silent frames are returned as `-200`.
#' @param periodsPerWindow Numeric giving the minimum number of pitch periods
#' to include per window. Default is `4.5`, which then sets the actual window
#' length with reference to `floor`.
#' @param filterFrom Numeric giving the lower frequency bound of an optional
#' bandpass filter applied to the signal before estimating HNR in Hz.
#' Default is `0`.
#' @param filterTo Numeric giving the upper frequency bound of an optional
#' bandpass filter applied to the signal before estimating HNR in.
#' Default is `0` (interpreted as "no upper bound").
#' @param filterSmoothing Numeric giving the width of the frequency width of
#' the window
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
#' hnr <- harmonicityAC(soundFile)
harmonicityAC <- function(filename, output = 'ssff', ext = '.wav',
                          timeStep = 0.01, start = NULL, end = NULL,
                          times = NULL, floor = 75,silenceThreshold = 0.1,
                          periodsPerWindow = 1, filterFrom = 0,
                          filterTo = 0, filterSmoothing = 100,
                          toFile = FALSE,
                          outputDir = getwd(), outputExt = '.hnr') {

  if (!output %in% c('df', 'ssff')) stop(
    'output should be either df or sff')

  if (any(dir.exists(filename))) filename <-
      list.files(filename, pattern = ext)
  if (length(filename) > 1 & output == 'ssff' & !toFile) stop(
    'When processing multiple files with ssff output, toFile must be TRUE')

  if (output == 'df') out <- data.frame(file = NULL, t = NULL, hnr = NULL)

  p <- reticulate::import('parselmouth')

  for (f in filename) {
    snd <- p$read(f)$extract_part(start, end, preserve_times=T)
    snd <- p$praat$call(snd, 'Filter (pass Hann band)', filterTo, filterFrom,
                        filterSmoothing)
    hnr <- snd$to_harmonicity_ac(time_step = timeStep, minimum_pitch = floor,
                                 silence_threshold = silenceThreshold,
                                 periods_per_window = periodsPerWindow)

    if (output == 'ssff') {
      ado <- harmonicity2ssff(hnr)
      if (!toFile) {
        out <- ado
      } else {
        wrassp::write.AsspDataObj(ado, file.path(outputDir,
                                                 gsub(ext, outputExt, f)))
      }
    } else if (output == 'df') {
      tmp <- harmonicity2df(hnr, f, times)
      out <- rbind(out, tmp)
    }
  }

  if (!toFile) return(out)

}
