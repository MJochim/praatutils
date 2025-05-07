pitchRawCC <- function(filename, output = 'ssff', ext = '.wav',
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

  p <- reticulate::import('parselmouth')

  for (f in filename) {
    pit <- p$read(f)$extract_part(start, end, preserve_times=T)$to_pitch_cc(
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
    }
  }

  if (!toFile) return(out)

}
