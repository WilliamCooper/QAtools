PitchSearch <- function (data) {
  ## needs PITCH, GGALT
  valid <- (!is.na(data$Time)) & (!is.na(data$PITCH))
  valid[is.na (data$GGALT)] <- FALSE
  valid[data$TASX < 150] <- FALSE
  DataT <- data[valid, ]
  ## look for high 5-min pitch variance
  del <- 300	# calculate variance over 5 min (typical pitch maneuver)
  Ptest <- 10
  delz <- 50
  L <- dim(DataT)[1]
  r <- 1:L
  cb <- vector ("numeric", del)
  kcb <- 1
  DataT$Pvar <- rep (0, L)
  for (i in 1:L) {
    cb[kcb] <- DataT$PITCH[i]
    kcb <- (kcb + 1) %% del
    DataT$Pvar[i] <- var(cb)
    if (DataT$Pvar[i] < Ptest) {r[i] <- NA}
    if (i < L-10) {
      if (abs (DataT$GGALT[i] - DataT$GGALT[i+10]) > delz) {
        r[i] <- NA
      } 
    } else {r[i] <- NA}
  }
  # plotWAC (DataT[, c("Time", "Pvar", "PITCH")])
  ## require Pvar > 10 for 60 s
  r[L] <- L 	# this forces consideration of the last segment
  s <- r[!is.na(r)]
  if (length (s) > 60) {
    # look for breaks:
    startPitch <- s[1]
    startTime <- DataT$Time[s[1]]
    for (j in 1:(length(s)-1)) {
      if (s[j+1]-s[j] > 3) {
        endTime <- DataT$Time[s[j]]
        if (s[j] - startPitch > 60) {
          print (sprintf( "possible pitch maneuver: %s--%s", 
                          strftime(startTime-250, format="%H%M%S", tz='UTC'),
                          strftime (endTime-100, format="%H%M%S", tz='UTC')))
        }
        startTime <- DataT$Time[s[j+1]]
        startPitch <- s[j+1]
      }
    }
  }
}

