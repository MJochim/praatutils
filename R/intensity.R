#' Estimate root-mean-squared amplitude
#'
#' Wrapper for Praat procedure that estimates intensity (root-mean-squared
#' amplitude) values from an audio file or list of audio files.
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
#' `NULL`, in which case `timeStep` corresponds to `0.8 / minPitch`.
#' @param start Numeric giving the desired start time of analysis in
#' seconds. Default is `NULL`, equivalent to the start of the audio file.
#' @param end Numeric giving the desired end time of analysis in seconds.
#' Default is `NULL`, equivalent to the duration of the audio file.
#' @param times Optional numeric vector giving the times to return signal
#' values from. Default is `NULL`.
#' @param minPitch Numeric giving the lowest pitch value in Hz to account for.
#' Default is `100`.
#' @param subtractMean Boolean; should mean pressure be subtracted around each
#' frame before applying Gaussian window? Default is `TRUE`.
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
#' rms <- intensity(soundFile)
intensity <- function(filename, output = 'ssff', ext = '.wav',
                      timeStep = NULL, start = NULL, end = NULL,
                      times = NULL, minPitch = 100, subtractMean = TRUE,
                      toFile = FALSE, outputDir = getwd(), outputExt = '.rms') {

  if (!output %in% c('df', 'ssff')) stop(
    'output should be either df or sff')

  if (any(dir.exists(filename))) filename <-
      list.files(filename, pattern = ext)
  if (length(filename) > 1 & output == 'ssff' & !toFile) stop(
    'When processing multiple files with ssff output, toFile must be TRUE')

  if (output == 'df') out <- data.frame(file = NULL, t = NULL, rms = NULL)

  p <- reticulate::import('parselmouth')

  for (f in filename) {
    rms <- p$read(f)$extract_part(start, end, preserve_times=T)$to_intensity(
      minimum_pitch = minPitch, time_step = timeStep,
      subtract_mean = subtractMean)

    if (output == 'ssff') {
      ado <- intensity2ssff(rms)
      if (!toFile) {
        out <- ado
      } else {
        wrassp::write.AsspDataObj(ado, file.path(outputDir,
                                                 gsub(ext, outputExt, f)))
      }
    } else if (output == 'df') {
      tmp <- intensity2df(rms, f, times)
      out <- rbind(out, tmp)
    }
  }

  if (!toFile) return(out)

}
