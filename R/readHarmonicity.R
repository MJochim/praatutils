readHarmonicity <- function(filename, output = 'ssff', ext = '.Harmonicity',
                            times = NULL, toFile = FALSE, outputDir = getwd(),
                            outputExt = '.hnr') {

  if (!output %in% c('df', 'ssff')) stop(
    'output should be either df or ssff')

  if (any(dir.exists(filename))) filename <-
      list.files(filename, pattern = ext)
  if (length(filename) > 1 & output == 'ssff' & !toFile) stop(
    'When processing multiple files with ssff output, toFile must be TRUE')

  if (output == 'df') out <- data.frame(file = NULL, t = NULL, hnr = NULL)

  p <- reticulate::import('parselmouth')

  for (f in filename) {
    hnr <- p$read(f)

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
