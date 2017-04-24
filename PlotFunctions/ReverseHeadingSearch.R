ReverseHeadingSearch <- function (data) {
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
  L <- nrow(DataT)
  r <- 1:L
  ## look for straight legs, save indices in r
  ## (this won't work near the 0-360 transition...)
  for (i in (del+1):(L-del)) {
    if (abs (DataT$THDG[i+del] - DataT$THDG[i]) > 15 || 
          abs (DataT$ROLL) > 15 ||
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
    legStart <- vector ("numeric")
    legEnd   <- vector ("numeric")
    legHead  <- vector ("numeric")
    legHeight <- vector ("numeric")
    # look for breaks:
    # print (sprintf ("start at 1 %d", s[1]))
    #plot(DataT$Time, DataT$GGALT/100, type='l', col='blue', lwd=1)
    #lines(DataT$Time[r], DataT$GGALT[r]/100, col='darkorange', lwd=5)
    startLeg <- s[1]
    startTime <- DataT$Time[s[1]]
    for (j in 1:(length(s)-1)) {
      if (s[j+1]-s[j] > 3) {
        endTime <- DataT$Time[s[j]]
        if (s[j] - startLeg > 120) {
          ## get mean heading
          meanH <- mean(DataT$THDG[startLeg:s[j]], na.rm=TRUE)
          legHead <- append(legHead, meanH)
          legStart <- append(legStart, startLeg)
          legEnd <- append(legEnd, s[j])
          legHeight <- append (legHeight, mean (DataT$GGALT[startLeg:s[j]], na.rm=TRUE))
          # print (sprintf ("straight leg, hdg %f, from %s to %s", meanH, 
          #                 DataT$Time[startLeg], DataT$Time[s[j]]))
        }
        startTime <- DataT$Time[s[j+1]]
        startLeg <- s[j+1]
      }
    }
    ## check legs for 180 change within 5 min and about same altitude
    for (j in 2:length(legHead)) {
      if (abs (legHead[j-1]-legHead[j]) > 160 && abs (legHead[j-1]-legHead[j]) < 200) {
        if (legStart[j]-legEnd[j-1] < 300) {
          if (abs(legHeight[j-1] - legHeight[j]) < 100) {
            print (sprintf ("possible reverse-heading maneuver: %s--%s and %s--%s",
                            strftime (DataT$Time[legStart[j-1]], format="%H%M%S", tz='UTC'),
                            strftime (DataT$Time[legEnd[j-1]], format="%H%M%S", tz='UTC'),
                            strftime (DataT$Time[legStart[j]], format="%H%M%S", tz='UTC'),
                            strftime (DataT$Time[legEnd[j]], format="%H%M%S", tz='UTC')))
          }
        }
      }
    }
  }
}

