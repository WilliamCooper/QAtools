
SplitDV <- function(v, cutoffPeriod) {
  vs <- zoo::na.approx (as.vector(v), na.rm=FALSE)
  rmean <- zoo::rollapply(vs, width=51, FUN=mean, fill=NA)
  rmean <- zoo::na.approx (as.vector(rmean), na.rm=FALSE)
  means <- mean(vs, na.rm=TRUE)
  vs[is.na(vs)] <- rmean[is.na(vs)]
  vs[is.na(vs)] <- means
  vs <- signal::filtfilt (signal::butter (3, 2/cutoffPeriod), vs)
  return(vs)
}
