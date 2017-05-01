PitchSearch <- function (data) {
  rtn <- NA
  ## needs PITCH, GGALT
  ## Avoid leading sequence of NA in PITCH:
  ip <- which(!is.na(data$PITCH))[1]
  if (length(ip) > 0 && ip[1] != 1) {
    data$PITCH[1:ip] <- 0
  }
  ## filter to isolate fluctuations with 15--30 s period
  data$P <- zoo::na.approx(as.vector(data$PITCH), maxgap=1000, na.rm=FALSE)
  data$P[is.na(data$P)] <- 0
  data$PF <- signal::filter (signal::butter (3, 2/15), data$P) - signal::filter (signal::butter (3, 2/30), data$P)
  ## also calculate a mean climb rate, expect < 5 m/s
  data$dh <- SmoothInterp(c(0, diff(data$GGALT)), .Length=121)
  data$VAR <- zoo::rollapply(as.vector(data$PF), width=60, FUN=var, fill=NA)
  ## look for variance > 2 for 90 s and peak value > 4 in that interval:
  iv <- which(data$VAR > 2)
  if (length(iv) < 1) {
    return (rtn)
  }
  ivn <- which(data$VAR <= 2)
  ivd <- c(1, diff(iv))
  ivs <- c(iv[1], iv[ivd > 5])
  if (length(ivs) < 1) {
    return (rtn)
  }
  ive <- vector('numeric')
  for (i in 1:length(ivs)) {ive[i] <- which(ivn > ivs[i])[1]}
  # with(data, plotWAC(data.frame(Time, PF, VAR)))
  for (i in 1:length(ivs)) {
    if (is.na(ive[i])) {next}
    r <- ivs[i]:ivn[ive[i]]
    # print (mean (data$GGALT[r], na.rm=TRUE))
    if(abs(mean(data$dh[r], na.rm=TRUE)) > 5) {next}
    if(mean(data$GGALT[r], na.rm=TRUE) < 1000) {next}
    startTime <- data$Time[r[1]]
    endTime <- data$Time[r[length(r)]]
    rt <- sprintf( "%s %s pitch maneuver  %s--%s",
                   attr(data, 'project'), attr(data, 'FlightNumber'),
                   strftime(startTime-300, format="%H%M%S", tz='UTC'),
                   strftime (endTime+300, format="%H%M%S", tz='UTC'))
    if (is.na(rtn[1])) {
      rtn <- rt
    } else {
      rtn <- c(rtn, rt)
    }
    # points(data$Time[r], rep(2, length(r)), pch=20, col='forestgreen')
  }
  return (rtn)
}

