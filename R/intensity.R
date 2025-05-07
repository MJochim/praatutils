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
