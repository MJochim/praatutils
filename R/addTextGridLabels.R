addTextGridLabels <- function(signal, TextGrid,
                              soundExt = '.wav', tgExt = '.TextGrid') {

  files <- unique(TextGrid$file)
  tiers <- unique(TextGrid$tier)

  for (f in files) {
    sigFile <- signal[which(signal$file == gsub(tgExt, soundExt, f)),]
    for (x in tiers) {
      sigFile[,x] <- NA
      filt <- TextGrid[which(TextGrid$tier == x),]
      for (i in 1:nrow(filt)) {
        sigFile[which(sigFile$t >= filt$tmin[i] &
                        sigFile$t <= filt$tmax[i]),x] <-
          filt$text[i]
      }
    }
    if ('out' %in% ls()) {
      out <- rbind(out, sigFile)
    } else {
      out <- sigFile
    }
  }

  return(out)

}
