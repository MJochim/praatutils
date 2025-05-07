formant2table <- function(fm, filename, returnBw, returnIntensity,
                          times = NULL) {
  p <- reticulate::import('parselmouth')

  if (is.null(times)) {
    out <- p$praat$call(fm, 'List', 0, 1, 3, returnIntensity,
                        3, 1, 3, returnBw) |>
      read.table(text = _, header = TRUE)
    colnames(out) <- gsub('.Hz.', '', colnames(out))
    for (x in 4:ncol(out)) {
      if (!is.numeric(out[,x])) out[,x] <- suppressWarnings(as.numeric(out[,x]))
    }
    colnames(out)[1] <- 't'
    tmp <- cbind(file = rep(filename, nrow(out)), out)
  } else {
    fn <- rep(filename, length(times))
    t <- times
    nFmt <- p$praat$call(fm, 'Get maximum number of formants')
    out <- data.frame(file = fn, t = t)
    fmtVal <- c()
    for (fmt in 1:nFmt) {
      for (i in 1:length(times)) {
        fmtVal[i] <- fm$get_value_at_time(formant_number = as.integer(fmt),
                                       time = times[i])
      }
      out[,paste0('F', fmt)] <- fmtVal
    }
    if (returnBw) {
      for (fmt in 1:nFmt) {
        for (i in 1:length(times)) {
          bwVal <- fm$get_bandwidth_at_time(formant_number = as.integer(fmt),
                                            time = times[i])
        }
        out[,paste0('B', fmt)] <- bwVal
      }
    }
  }
  return(out)
}
