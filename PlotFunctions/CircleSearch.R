CircleSearch <- function (data) {
  rtn <- NA
  ## needs THDG, GGALT, TASX, ROLL
  ip <- which(!is.na(data$ROLL))[1]
  if (length(ip) > 0 && ip[1] != 1) {
    data$ROLL[1:ip] <- 0
  }
  data$R <- zoo::na.approx(as.vector(data$ROLL), maxgap=1000, na.rm=FALSE)
  data$R[is.na(data$R)] <- 0
  data$dz <- SmoothInterp(c(0, diff(data$GGALT)), .Length=121)
  data$dR <- c(0, diff(data$R))
  # data$VAR <- zoo::rollapply(as.vector(data$R), width=30, FUN=var, fill=NA)
  iroll <- which(abs(data$ROLL) > 5)
  ird <- c(1, diff(iroll))
  j <- which(ird > 10)
  iw <- which(abs(data$ROLL) > 20)
  diw <- c(0, diff(iw))
  iwn <- which(abs(data$ROLL) <= 20)
  diwn <- c(1, diff(iwn))
  j <- iw[which(diw > 10)]
  for (k in j) {
    rg2 <- iwn[which(iwn > k)[1]]
    if (rg2-k < 100) {next}
    r <- k:rg2
    if (max(data$GGALT[r], na.rm=TRUE) - min(data$GGALT[r], na.rm=TRUE) > 50) {next}
    meanZ <- mean(data$GGALT[r])
    ## require total turn angle to exceed 300 deg:
    rmn <- mean(abs(data$ROLL[r]), na.rm=TRUE)
    tasm <- mean(data$TASX[r], na.rm=TRUE)
    if (is.na(tasm)) {next}
    ta <- 9.8*tan(rmn*pi/180)*length(r)/tasm * 180/pi
    if (abs(ta) < 300) {next}
    # print (sprintf ('%d--%d', k, rg2))
    rt  <- sprintf( "%s %s circle          %s %s %.0f", 
                    attr(data, 'project'), attr(data, 'FlightNumber'),
                    strftime(data$Time[k], format="%H%M%S", tz='UTC'),
                    strftime(data$Time[rg2], format="%H%M%S", tz='UTC'),
                    ta)
    if (is.na(rtn[1])) {
      rtn <- rt
    } else {
      ## consider if this turn should be added to the previous one
      if (as.integer(difftime(data$Time[k], TLast, units='secs')) < 150 && abs(meanZ-meanZlast) < 200) {
        rtn[length(rtn)] <- sprintf( "%s %s circle          %s %s %.0f", 
                                     attr(data, 'project'), attr(data, 'FlightNumber'),
                                     strftime(TFirst, format="%H%M%S", tz='UTC'),
                                     strftime(data$Time[rg2], format="%H%M%S", tz='UTC'),
                                     ta)
      } else {
        rtn <- c(rtn, rt)
      }
    }
    meanZlast <- meanZ
    TLast <- data$Time[rg2]
    TFirst <- data$Time[k]
  }
  return(rtn)
}
  
#   
# 
#   
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
#   L <- dim(DataT)[1]
#   r <- 1:L
#   for (i in (del+1):(L-del)) {
#     ##if ((abs (DataT$THDG[i+del]-DataT$THDG[i]) < tol*del) ||
#     ##      (abs(DataT$GGALT[i+del]-DataT$GGALT[i]) > delz)) {
#     if (abs (DataT$ROLL[i]) < 15 || 
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
#     # look for breaks:
#     # print (sprintf ("start at 1 %d", s[1]))
#     #plot(DataT$Time, DataT$GGALT/100, type='l', col='blue', lwd=1)
#     #lines(DataT$Time[r], DataT$GGALT[r]/100, col='darkorange', lwd=5)
#     startCircle <- s[1]
#     startTime <- DataT$Time[s[1]]
#     for (j in 1:(length(s)-1)) {
#       if (s[j+1]-s[j] > 3) {
#         endTime <- DataT$Time[s[j]]
#         ## get total turn angle
#         Turn <- 0
#         for (k in startCircle:(s[j]-1)) {
#           DTurn <- abs (DataT$THDG[k+1] - DataT$THDG[k])
#           if (DTurn > 180) {DTurn <- 360 - DTurn}
#           Turn <- Turn + DTurn
#         }
#         if (Turn > 330) {
#           rt  <- sprintf( "%s %s circle          %s--%s, %.0f-deg turn", 
# 			  attr(data, 'project'), attr(data, 'FlightNumber'),
#                           strftime(startTime, format="%H%M%S", tz='UTC'),
#                           strftime (endTime, format="%H%M%S", tz='UTC'),
#                           Turn)
# 	  if (is.na(rtn[1])) {
# 		  rtn <- rt
# 	  } else {
# 		  rtn <- c(rtn, rt)
# 	  }
#         }
#         startTime <- DataT$Time[s[j+1]]
#         startCircle <- s[j+1]
#       }
#     }
#   }
#   return (rtn)
# }

