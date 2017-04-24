YawSearch <- function (data) {
  ## needs SSRD
  valid <- (!is.na(data$Time)) & (!is.na(data$SSRD))
  DataT <- data[valid, ]
  ## look for high 3-min sideslip variance
  del <- 180	# calculate variance over 3 min (typical yaw maneuver)
  Stest <- 0.4
  delz <- 50
  L <- dim(DataT)[1]
  r <- 1:L
  cb <- vector ("numeric", del)
  kcb <- 1
  DataT$Svar <- rep (0, L)
  for (i in 1:L) {
    cb[kcb] <- DataT$SSRD[i]
    kcb <- (kcb + 1) %% del
    DataT$Svar[i] <- var(cb)
    if (DataT$Svar[i] < Stest) {r[i] <- NA}
  }
  # plotWAC (DataT[, c("Time", "Svar", "SSRD")])
  ## require Svar > 5 for 60 s
  r[L] <- L 	# this forces consideration of the last segment
  s <- r[!is.na(r)]
  if (length (s) > 60) {
    # look for breaks:
    startSS <- s[1]
    startTime <- DataT$Time[s[1]]
    for (j in 1:(length(s)-1)) {
      if (s[j+1]-s[j] > 3) {
        endTime <- DataT$Time[s[j]]
        if (s[j] - startSS > 60) {
          print (sprintf( "possible yaw maneuver:   %s--%s", 
                          strftime(startTime-150, format="%H%M%S", tz='UTC'),
                          strftime (endTime-150, format="%H%M%S", tz='UTC')))
        }
        startTime <- DataT$Time[s[j+1]]
        startSS <- s[j+1]
      }
    }
  }
}

