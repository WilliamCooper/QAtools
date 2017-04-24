SpeedRunSearch <- function (data, File=' ') {
  ## needs TASX, GGALT
  del <- 75
  tol <- .3
  delz <- 20
  DataT <- data[!is.na(data$Time) & !is.na(data$TASX) & !is.na(data$GGALT), ]
  L <- dim(DataT)[1]
  if (L == 0) {return()}
  r <- 1:L
  for (i in (del+1):(L-del)) {
    if ((!is.na(DataT$TASX[i]) && abs (DataT$TASX[i+del]-DataT$TASX[i]) < tol*del) ||
          (abs(DataT$GGALT[i+del]-DataT$GGALT[i]) > delz)) {
      r[i] <- NA 
    }
    if ((abs (DataT$TASX[i-del]-DataT$TASX[i]) > tol*del) &&
          (abs(DataT$GGALT[i-del]-DataT$GGALT[i]) < delz)) {
      r[i] <- i
    }
  }
  r[1:del] <- NA
  r[(L-del):L] <- NA
  r[L] <- L 	# forces consideration of last candidate
  s <- r[!is.na(r)]
  if (length (s) > 100) {
    # look for breaks of at least del:
    # print (sprintf ("start at 1 %d", s[1]))
    #plot(DataT$Time, DataT$GGALT/100, type='l', col='blue', lwd=1)
    #lines(DataT$Time[r], DataT$GGALT[r]/100, col='darkorange', lwd=5)
    startSpeedRun <- 1
    startTime <- DataT$Time[s[1]]
    lastSpeedChange <- 0
    for (j in 1:(length(s)-1)) {
      if (s[j+1]-s[j] > del) {
        endTime <- DataT$Time[s[j]]
        ## find range in speed:
        slow <- min (DataT$TASX[s[startSpeedRun]:s[j]], na.rm=TRUE)
        fast <- max (DataT$TASX[s[startSpeedRun]:s[j]], na.rm=TRUE)
        if ((fast-slow > 40) && (fast > 180)) {
          print (sprintf( "%s possible speed run:      %s--%s, %f to %f m/s", 
                          File, 
                          strftime(startTime, format="%H%M%S", tz='UTC'),
                          strftime (endTime, format="%H%M%S", tz='UTC'),
                          slow, fast))
        }
        startTime <- DataT$Time[s[j+1]]
        startSpeedRun <- j+1
      }
    }
  }
}

