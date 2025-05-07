readPitch <- function(filename, output = 'ssff', ext = '.Pitch', times = NULL,
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
    pit <- p$read(f)

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
