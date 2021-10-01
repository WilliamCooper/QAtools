ReverseHeadingSearch <- function (data) {
  rtn <- NA
  ip <- which(!is.na(data$THDG))[1]
  if (length(ip) > 0 && ip[1] != 1) {
    data$THDG[1:ip] <- 0
  }
  data$H <- zoo::na.approx(as.vector(data$THDG), maxgap=1000, na.rm=FALSE)
  data$H[is.na(data$H)] <- 0
  data$dz <- SmoothInterp(c(0, diff(data$GGALT)), .Length=121)
  data$dH <- c(0, diff(data$H))
  iu <- which(data$dH > 270)
  id <- which(data$dH < -270)
  for (j in iu) {
    r <- j:nrow(data)
    data$H[r] <- data$H[r] - 360
  }
  for (j in id) {
    r <- j:nrow(data)
    data$H[r] <- data$H[r] + 360
  }
  # data$VAR <- zoo::rollapply(as.vector(data$H), width=60, FUN=var, fill=NA)
  iroll <- which(abs(data$ROLL) > 5)
  ird <- c(1, diff(iroll))
  j <- which(ird > 10)
  mhl <- NA
  ## these are the straight legs:
  for (k in j) {
    rg1 <- iroll[k-1]+1
    rg <- rg1:(iroll[which(iroll > rg1)[1]]-1)
    if (length(rg) < 240) {next} ## require at least 4 min straight leg
    ## is the leg reasonably level?
    th <- max(data$GGALT[rg], na.rm=TRUE) - min(data$GGALT[rg], na.rm=TRUE)
    if (th > 40) {next}
    ## get the mean heading
    mh <- mean(data$H[rg], na.rm=TRUE) %% 360
    dmh <- abs(mh-mhl) %% 360
    mhl <- mh
    if (!is.na(dmh) && abs(dmh-180) < 5) {
      # print (sprintf ('straight leg: %s--%s mean hdg is %.1f change %.1f', 
      #                 formatTime(data$Time[rg[1]]), formatTime(data$Time[rg[length(rg)]]),
      #                 mh, dmh))
      rt <- sprintf ("%s %s reverse-heading %s %s %s %s",
                     attr(data, 'project'), attr(data, 'FlightNumber'),
                     strftime (data$Time[rgs1], format="%H%M%S", tz='UTC'),
                     strftime (data$Time[rgs2], format="%H%M%S", tz='UTC'),
                     strftime (data$Time[rg1], format="%H%M%S", tz='UTC'),
                     strftime (data$Time[rg[length(rg)]], format="%H%M%S", tz='UTC'))
      if(is.na(rtn[1])) {rtn <- rt}
      else {rtn <- c(rtn, rt)}
    }
    rgs1 <- rg1
    rgs2 <- rg[length(rg)]
  }
  return(rtn)
}

  
  
#   ## needs THDG, GGALT, TASX, ROLL
#   valid <- (!is.na(data$Time)) & (!is.na(data$TASX))
#   valid[is.na(data$GGALT)] <- FALSE
#   valid[is.na(data$THDG)] <- FALSE
#   valid[is.na(data$ROLL)] <- FALSE
#   DataT <- data[valid, ]
#   ## look for steady roll for near-360-deg turn
#   ## require approx level
#   del <- 30	# level for 30 s
#   tol <- .3
#   delz <- 30
#   L <- nrow(DataT)
#   r <- 1:L
#   ## look for straight legs, save indices in r
#   ## (this won't work near the 0-360 transition...)
#   for (i in (del+1):(L-del)) {
#     if (abs (DataT$THDG[i+del] - DataT$THDG[i]) > 15 || 
#           abs (DataT$ROLL) > 15 ||
#           abs(DataT$GGALT[i+del] - DataT$GGALT[i] > delz)) {
#       r[i] <- NA 
#     } else {
#       r[i] <- i
#     }
#   }
#   r[1:del] <- NA
#   r[(L-del):L] <- NA
#   r[L] <- L 	# force consideration of last break
#   s <- r[!is.na(r)]
#   if (length (s) > 120) {
#     legStart <- vector ("numeric")
#     legEnd   <- vector ("numeric")
#     legHead  <- vector ("numeric")
#     legHeight <- vector ("numeric")
#     # look for breaks:
#     # print (sprintf ("start at 1 %d", s[1]))
#     #plot(DataT$Time, DataT$GGALT/100, type='l', col='blue', lwd=1)
#     #lines(DataT$Time[r], DataT$GGALT[r]/100, col='darkorange', lwd=5)
#     startLeg <- s[1]
#     startTime <- DataT$Time[s[1]]
#     for (j in 1:(length(s)-1)) {
#       if (s[j+1]-s[j] > 3) {
#         endTime <- DataT$Time[s[j]]
#         if (s[j] - startLeg > 120) {
#           ## get mean heading
#           meanH <- mean(DataT$THDG[startLeg:s[j]], na.rm=TRUE)
#           legHead <- append(legHead, meanH)
#           legStart <- append(legStart, startLeg)
#           legEnd <- append(legEnd, s[j])
#           legHeight <- append (legHeight, mean (DataT$GGALT[startLeg:s[j]], na.rm=TRUE))
#           # print (sprintf ("straight leg, hdg %f, from %s to %s", meanH, 
#           #                 DataT$Time[startLeg], DataT$Time[s[j]]))
#         }
#         startTime <- DataT$Time[s[j+1]]
#         startLeg <- s[j+1]
#       }
#     }
#     ## check legs for 180 change within 5 min and about same altitude
#     for (j in 2:length(legHead)) {
#       if (abs (legHead[j-1]-legHead[j]) > 160 && abs (legHead[j-1]-legHead[j]) < 200) {
#         if (legStart[j]-legEnd[j-1] < 300) {
#           if (abs(legHeight[j-1] - legHeight[j]) < 100) {
#             rt <- sprintf ("%s %s reverse-heading maneuver: %s--%s and %s--%s",
# 			    attr(data, 'project'), attr(data, 'FlightNumber'),
#                             strftime (DataT$Time[legStart[j-1]], format="%H%M%S", tz='UTC'),
#                             strftime (DataT$Time[legEnd[j-1]], format="%H%M%S", tz='UTC'),
#                             strftime (DataT$Time[legStart[j]], format="%H%M%S", tz='UTC'),
#                             strftime (DataT$Time[legEnd[j]], format="%H%M%S", tz='UTC'))
#             if(is.na(rtn[1])) {rtn <- rt}
# 	    else {rtn <- c(rtn, rt)}
#           }
#         }
#       }
#     }
#   }
#   return (rtn)
# }
# 
