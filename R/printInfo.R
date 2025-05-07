printInfo <- function(file) {
  p <- reticulate::import('parselmouth')
  p$read(file) |>
    p$praat$call('Info') |>
    strsplit('\n') |>
    unlist() |>
    cat(sep='\n')
}
