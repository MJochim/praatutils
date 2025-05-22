#' Estimate pitch using raw autocorrelation
#'
#' Wrapper for Praat procedure that estimates pitch
#' values from an audio file or list of audio files based on the raw
#' autocorrelation of the signal.
#'
#' @param filename String giving the path to an audio file or a directory of
#' audio files. Alternatively a vector of strings giving the paths to audio
#' files.
#' @param output String giving the desired output format. Should be either
#' `ssff` (default) for a list with the Simple Signal File Format, `df` for a
#' data frame, or `candidates` for a long data frame listing all pitch
#' candidates.
#' @param ext String giving the file extension for audio files; default is
#' `.wav`. Only used if `filename` is a directory.
#' @param timeStep Numeric giving the desired sampling frequency in
#' seconds. Default is
#' `NULL`, in which case `timeStep` corresponds to `0.75 / floor`.
#' @param start Numeric giving the desired start time of analysis in
#' seconds. Default is `NULL`, equivalent to the start of the audio file.
#' @param end Numeric giving the desired end time of analysis in seconds.
#' Default is `NULL`, equivalent to the duration of the audio file.
#' @param times Optional numeric vector giving the times to return signal
#' values from. Default is `NULL`.
#' @param floor Numeric giving the lowest expected pitch frequency in Hz.
#' Default is `75`.
#' @param ceiling Numeric giving the highest expected pitch frequency in Hz.
#' Default is `600`.
#' @param maxNoCandidates Integer giving the highest number of pitch candidates
#' to estimate. Default is `15`.
#' @param gaussianWindow Boolean; should a Gaussian window with a physical
#' length of 6 pitch cycles be used for windowing the signal? Default is
#' `FALSE`, in which case a Hanning window with a physical length of 3 pitch
#' cycles is used.
#' @param silenceThreshold Numeric giving threshold for when to consider frames
#' silent. Default is `0.03` relative to the global maximum amplitude.
#' @param voicingThreshold Numeric giving threshold above which pitch candidates
#' will be consider voiced relative to the maximum autocorrelation in the
#' signal. Default is `0.45`.
#' @param octaveCost Numeric giving the degree to which high-frequency
#' candidates are favored relative to the maximum autocorrelation in the signal.
#' Default is `0.01` per octave.
#' @param octaveJumpCost Numeric giving the degree to which pitch changes are
#' disfavored relative to the maximum autocorrelation in the signal. Default is
#' `0.35`.
#' @param voicedUnvoicedCost Numeric giving the degree to which transitions
#' in voicing status are disfavored relative to the maximum autocorrelation in
#' the signal. Default is `0.14`.
#' @param interpolate Boolean; should Praat's interpolation routine be applied
#' to the resulting Pitch object? Default is `FALSE`.
#' @param smoothingBW Numeric giving the bandwidth frequency in Hz to be used
#' for smoothing the pitch track. Default is `NULL`, in which case the pitch
#' track is not smoothed.
#' @param killOctaveJumps Boolean; should Praat's routine for removing octave
#' jumps be applied to the resulting Pitch object? Default is `FALSE`.
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
#' pitch <- pitchRawAC(soundFile)
pitchRawAC <- function(filename, output = 'ssff', ext = '.wav',
                       timeStep = NULL, start = NULL, end = NULL, times = NULL,
                       floor = 75, ceiling = 600,
                       maxNoCandidates = 15, gaussianWindow = FALSE,
                       silenceThreshold = 0.03, voicingThreshold = 0.45,
                       octaveCost = 0.01, octaveJumpCost = 0.35,
                       voicedUnvoicedCost = 0.14, interpolate = FALSE,
                       smoothingBW = NULL, killOctaveJumps = FALSE,
                       toFile = FALSE, outputDir = getwd(), outputExt = '.f0') {

  if (!output %in% c('df', 'ssff', 'candidates')) stop(
    'output should be either df, ssff, or candidates')

  if (any(dir.exists(filename))) filename <-
      list.files(filename, pattern = ext)
  if (length(filename) > 1 & output == 'ssff' & !toFile) stop(
    'When processing multiple files with ssff output, toFile must be TRUE')

  if (output == 'df') out <- data.frame(file = NULL, t = NULL, f0 = NULL)
  if (output == 'candidates') out <- data.frame(
    file = NULL, frame = NULL, t = NULL, frequency = NULL, strength = NULL)

  p <- reticulate::import('parselmouth')

  for (f in filename) {
    pit <- p$read(f)$extract_part(start, end, preserve_times=T)$to_pitch_ac(
      time_step = timeStep, pitch_floor = floor,
      max_number_of_candidates = as.integer(maxNoCandidates),
      very_accurate = gaussianWindow, silence_threshold = silenceThreshold,
      voicing_threshold = voicingThreshold, octave_cost = octaveCost,
      octave_jump_cost = octaveJumpCost,
      voiced_unvoiced_cost = voicedUnvoicedCost, pitch_ceiling = ceiling)
    if (interpolate) pit <- pit$interpolate()
    if (!is.null(smoothingBW)) pit <- pit$smooth(smoothingBW)
    if (killOctaveJumps) pit <- pit$kill_octave_jumps()

    if (output == 'ssff') {
      ado <- pitch2ssff(pit)
      if (!toFile) {
        out <- ado
      } else {
        wrassp::write.AsspDataObj(ado, file.path(outputDir,
                                                 gsub(ext, outputExt, f)))
      }
    } else if (output == 'df') {
      tmp <- pitch2df(pit, f, times)
      out <- rbind(out, tmp)
    } else if (output == 'candidates') {
      tmp <- pitch2cand(pit, f)
      out <- rbind(out, tmp)
    }
  }

  if (!toFile) return(out)

}
