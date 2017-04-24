CircleSearch <- function (data) {
  ## needs THDG, GGALT, TASX, ROLL
  valid <- (!is.na(data$Time)) & (!is.na(data$TASX))
  valid[is.na(data$GGALT)] <- FALSE
  valid[is.na(data$THDG)] <- FALSE
  valid[is.na(data$ROLL)] <- FALSE
  DataT <- data[valid, ]
  ## look for steady roll for near-360-deg turn
  ## require approx level
  del <- 30	# level for 30 s
  tol <- .3
  delz <- 30
  L <- dim(DataT)[1]
  r <- 1:L
  for (i in (del+1):(L-del)) {
    ##if ((abs (DataT$THDG[i+del]-DataT$THDG[i]) < tol*del) ||
    ##      (abs(DataT$GGALT[i+del]-DataT$GGALT[i]) > delz)) {
    if (abs (DataT$ROLL[i]) < 15 || 
          abs(DataT$GGALT[i+del] - DataT$GGALT[i] > delz)) {
      r[i] <- NA 
    } else {
      r[i] <- i
    }
  }
  r[1:del] <- NA
  r[(L-del):L] <- NA
  r[L] <- L 	# force consideration of last break
  s <- r[!is.na(r)]
  if (length (s) > 120) {
    # look for breaks:
    # print (sprintf ("start at 1 %d", s[1]))
    #plot(DataT$Time, DataT$GGALT/100, type='l', col='blue', lwd=1)
    #lines(DataT$Time[r], DataT$GGALT[r]/100, col='darkorange', lwd=5)
    startCircle <- s[1]
    startTime <- DataT$Time[s[1]]
    for (j in 1:(length(s)-1)) {
      if (s[j+1]-s[j] > 3) {
        endTime <- DataT$Time[s[j]]
        ## get total turn angle
        Turn <- 0
        for (k in startCircle:(s[j]-1)) {
          DTurn <- abs (DataT$THDG[k+1] - DataT$THDG[k])
          if (DTurn > 180) {DTurn <- 360 - DTurn}
          Turn <- Turn + DTurn
        }
        if (Turn > 330) {
          print (sprintf( "possible circle:         %s--%s, %.0f-deg turn", 
                          strftime(startTime, format="%H%M%S", tz='UTC'),
                          strftime (endTime, format="%H%M%S", tz='UTC'),
                          Turn))
        }
        startTime <- DataT$Time[s[j+1]]
        startCircle <- s[j+1]
      }
    }
  }
}

