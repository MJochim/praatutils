pitch2cand <- function(pit, filename) {
  p <- reticulate::import('parselmouth')
  fn <- rep(filename, pit$n_frames)
  timesDF <- data.frame(file = fn,
                        frame = 1:pit$n_frames,
                        t = pit$ts())
  candTable <- p$praat$call(pit, 'Tabulate candidates')
  candDF <- p$praat$call(candTable, 'List', 0) |>
    read.table(text = _, header = TRUE)
  out <- merge(timesDF, candDF)
}
