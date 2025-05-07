harmonicityCC <- function(filename, output = 'ssff', ext = '.wav',
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
    hnr <- snd$to_harmonicity_cc(time_step = timeStep, minimum_pitch = floor,
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
