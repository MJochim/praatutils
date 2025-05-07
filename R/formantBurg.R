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
