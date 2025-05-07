readSound <- function(filename, start = NULL, end = NULL) {

  p <- reticulate::import('parselmouth')

  out <- list()
  snd <- p$read(filename)$extract_part(start, end, preserve_times=T)
  out$signal <- snd$values |> t()
  out$t <- snd$ts()
  out$fs <- snd$sampling_frequency
  out$start <- snd$start_time
  out$end <- snd$end_time
  return(out)

}
