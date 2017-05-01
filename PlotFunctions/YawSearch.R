YawSearch <- function (data) {
  rtn <- NA
  ## needs SSRD
  ## Avoid leading sequence of NA in SSRD:
  ip <- which(!is.na(data$SSRD))[1]
  if (length(ip) > 0 && ip[1] != 1) {
    data$SSRD[1:ip] <- 0
  }
  ## filter to isolate fluctuations with 15--30 s period
  data$S <- zoo::na.approx(as.vector(data$SSRD), maxgap=1000, na.rm=FALSE)
  data$S[is.na(data$S)] <- 0
  data$SF <- signal::filter (signal::butter (3, 2/5), 
                             data$S) - signal::filter (signal::butter (3, 2/60), 
                                                       data$S)
  ## also calculate a mean climb rate, expect < 5 m/s
  data$dh <- SmoothInterp(c(0, diff(data$GGALT)), .Length=121)
  data$VAR <- zoo::rollapply(as.vector(data$SF), width=60, FUN=var, fill=NA)
  iv <- which(data$VAR > 0.2)
  if (length(iv) < 1) {
    return (rtn)
  }
  ivn <- which(data$VAR <= 0.2)
  ivd <- c(1, diff(iv))
  ivs <- c(iv[1], iv[ivd > 5])
  if (length(ivs) < 1) {
    return (rtn)
  }
  ive <- vector('numeric')
  for (i in 1:length(ivs)) {ive[i] <- which(ivn > ivs[i])[1]}
  if (is.na(ive[1])) {return}
  # with(data, plotWAC(data.frame(Time, PF, VAR)))
  for (i in 1:length(ivs)) {
    if (is.na(ive[i])) {next}
    r <- ivs[i]:ivn[ive[i]]
    if (length(r) < 120) {next}
    # print (mean (data$GGALT[r], na.rm=TRUE))
    if(abs(mean(data$dh[r], na.rm=TRUE)) > 5) {next}
    if(!(any(!is.na(data$GGALT[r]))) || mean(data$GGALT[r], na.rm=TRUE) < 1000) {next}
    startTime <- data$Time[r[1]]
    endTime <- data$Time[r[length(r)]]
    rt <- sprintf( "%s %s yaw maneuver %s %s", 
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

