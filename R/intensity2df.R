intensity2df <- function(rms, filename, times = NULL) {
  if (is.null(times)) {
    fn <- rep(filename, rms$n_frames)
    t <- rms$ts()
    val <- t(rms$values)
  } else {
    fn <- rep(filename, length(times))
    t <- times
    val <- c()
    for (i in 1:length(times)) {
      val <- c(val, rms$get_value(time = times[i]))
    }
  }
  out <- data.frame(file = fn, t = t, rms = val)
}
