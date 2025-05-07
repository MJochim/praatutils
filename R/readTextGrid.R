readTextGrid <- function(filename, ext = '.TextGrid') {

  if (any(dir.exists(filename))) filename <-
      list.files(filename, pattern = ext)

  p <- reticulate::import('parselmouth')

  out <- data.frame(file = NULL, tmin = NULL, tier = NULL,
                    text = NULL, tmax = NULL)

  for (f in filename) {
    tg <- p$read(f)
    tmp <- p$praat$call(tg, 'List', 0, 3, 1, 0) |>
      read.table(text = _, header = TRUE)
    tmp <- cbind(file = rep(f, nrow(tmp)), tmp)
    out <- rbind(out, tmp)
  }

  return(out)

}
