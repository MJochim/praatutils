pitchSHS <- function(filename, output = 'ssff', ext = '.wav',
                     timeStep = 0.01, start = NULL, end = NULL, times = NULL,
                     floor = 50, ceiling = 600,
                     maxNoCandidates = 15, maxFreqComponent = 1250,
                     maxNoSubharmonics = 15, compressionFactor = 0.84,
                     pointsPerOctave = 48, interpolate = FALSE,
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
    pit <- p$read(f)$extract_part(start, end, preserve_times=T)$to_pitch_shs(
      time_step = timeStep, minimum_pitch = floor,
      max_number_of_candidates = as.integer(maxNoCandidates),
      maximum_frequency_component = maxFreqComponent,
      max_number_of_subharmonics = as.integer(maxNoSubharmonics),
      compression_factor = compressionFactor, ceiling = ceiling,
      number_of_points_per_octave = as.integer(pointsPerOctave))
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
