bpm <- function(df = NULL, timeOut = 5, plot = F) {
  if (is.null(df)) {
    df <- data.frame(rep = 0, Sys.time = as.numeric(Sys.time()), BPM = NA)
    df[1:2,] <- c(0,1, Sys.time(), Sys.time(), NA, NA)
    bpm(df, timeOut)
  } else if (nrow(df) < 2) {
    df <- data.frame(rep = 0, Sys.time = as.numeric(Sys.time()), BPM = NA)
    df[1:2,] <- c(0,1, Sys.time(), Sys.time(), NA, NA)
    bpm(df, timeOut)
  } else if (df[nrow(df),2] - df[nrow(df)-1,2] < timeOut) {
    if (nrow(df) > 3 & is.na(df[1,3])) df <- df[2:nrow(df),]
    readline(cat('Current median BPM:',round(median(df[,3], na.rm = T), 1),'\nPress Return\n'))
    y <- as.numeric(Sys.time())
    df[nrow(df) + 1, 1] <- nrow(df)
    df[nrow(df), 2] <- y
    df[nrow(df), 3] <- 60/(df[nrow(df), 2] - df[nrow(df) -1, 2])
    bpm(df, timeOut)
  } else {
    df <- df[1:nrow(df)-1,]
    if (plot == F) {
      cat('Break: timeOut reached.','\nMean:', round(mean(df[,3], na.rm = T), 1), 'bpm\nMedian:', round(median(df[,3], na.rm = T), 1), 'bpm\nEstimated beats per minute is:', round((round(mean(df[,3], na.rm = T), 1)+round(median(df[,3], na.rm = T), 1))/2), '\n')
      df
    } else if (plot == T) hist(df[,3]) #not plotting for some reason
  }
}

bpm(timeOut = 2, plot = T) #sample run

#shapiro.test(tap[,3]) #experimenting with testing the normaility, potentially to detect multiple bpms within one series (song)