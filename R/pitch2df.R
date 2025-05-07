pitch2df <- function(pit, filename, times = NULL) {
  if (is.null(times)) {
    fn <- rep(filename, pit$n_frames)
    t <- pit$ts()
    val <- pit$to_matrix()$values[1,]
    val[which(val == 0)] <- NA
  } else {
    fn <- rep(filename, length(times))
    t <- times
    val <- c()
    for (i in 1:length(times)) {
      val <- c(val, pit$get_value_at_time(times[i]))
    }
  }
  out <- data.frame(file = fn, t = t, f0 = val)
}
