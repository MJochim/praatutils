#' Read audio file
#'
#' Read audio file with most extensions as a list object using Praat
#'
#' @param filename String giving the file name of audio file.
#' @param start Numeric giving the desired start time to load in
#' seconds. Default is `NULL`, equivalent to the start of the audio file.
#' @param end Numeric giving the desired end time to load in
#' Default is `NULL`, equivalent to the duration of the audio file.
#'
#' @returns A list object
#' @export
#'
#' @examples
#' datapath <- system.file('extdata', package='praatutils')
#' soundFile <- paste0(datapath, '/1.wav')
#' snd <- readSound(soundFile)
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
