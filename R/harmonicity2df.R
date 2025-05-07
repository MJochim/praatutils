harmonicity2df <- function(hnr, filename, times = NULL) {
  if (is.null(times)) {
    fn <- rep(filename, hnr$n_frames)
    t <- hnr$ts()
    val <- t(hnr$values)
  } else {
    fn <- rep(filename, length(times))
    t <- times
    val <- c()
    for (i in 1:length(times)) {
      val <- c(val, hnr$get_value(time = times[i]))
    }
  }
  out <- data.frame(file = fn, t = t, hnr = val)
}
