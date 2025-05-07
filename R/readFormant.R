readFormant <- function(filename, output = 'ssff', ext = '.Pitch', times = NULL,
                        returnBw = FALSE, returnIntensity = FALSE,
                        toFile = FALSE, outputDir = getwd(),
                        outputExt = '.fmt') {

  if (!output %in% c('df', 'ssff')) stop(
    'output should be either df or ssff')

  if (any(dir.exists(filename))) filename <-
      list.files(filename, pattern = ext)
  if (length(filename) > 1 & output == 'ssff' & !toFile) stop(
    'When processing multiple files with ssff output, toFile must be TRUE')

  p <- reticulate::import('parselmouth')

  for (f in filename) {
    fm <- p$read(f)
    tmp <- formant2table(fm, f, returnBw, returnIntensity)

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
