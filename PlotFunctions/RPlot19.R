### plot 19: potential-temperature plots
RPlot19 <- function (data, Seq=NA) {
  ## needs THETA, THETAV, THETAE, THETAP, THETAQ, PSXC
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
  vp <- VRPlot$PV19[match(c('THETA', 'THETAV'), VRPlot$PV19)]
  vp <- vp[!is.na(vp)]
  plotWAC (data[, c("Time", vp)], ylab="potential temperatures",
           legend.position = "top")
  op <- par (mar=c(5,4,1,1)+0.1)
  vp <- VRPlot$PV19[match(c('THETAE', 'THETAP', 'THETAQ'), VRPlot$PV19)]
  vp <- vp[!is.na(vp)]
  ## check THETAP:
  if ('THETAP' %in% vp) {
  data <- data[!is.na(data$EWX), ]
  data$TP2 <- EquivalentPotentialTemperature (data$PSXC, data$ATX, data$EWX)
  }
  if ("THETAQ" %in% vp) {
    if (!("PLWCC" %in% names(data))) {data$PLWCC <- rep (0, nrow(data))}
    data$TQ2 <- WetEquivalentPotentialTemperature (data$PSXC, data$ATX, data$EWX, data$PLWCC)
    if (max (data$THETAE, na.rm=TRUE) < Inf) {
      plotWAC (data[, c('Time', vp)], 
               ylab="ad. pot. temperatures", legend.position = "top")
      title (sprintf("mean difference THETAE-TP2=%.2f THETAQ-TQ2=%.2f", 
                     mean(data$THETAP-data$TP2, na.rm=TRUE),
                     mean(data$THETAQ-data$TQ2, na.rm=TRUE)), cex.main=0.7)
    }
  } else {
    if (max (data$THETAE, na.rm=TRUE) < Inf) {
      plotWAC (data[, c('Time', vp)], 
                 ylab="ad. pot. temperatures", legend.position = "top")
        title (sprintf("mean difference THETAE-TP2=%.2f", 
                       mean(data$THETAP-data$TP2, na.rm=TRUE)), cex.main=0.7)
    }
  }
  AddFooter ()
  if (!is.na(Seq) && (Seq == 1)) {return()}
  # plots vs pressure:
  layout(matrix(2:1, ncol = 2), widths = c(5,5), heights = 1)
  op <- par (mar=c(5,2,1,1)+0.1)
  yl <- c(max (data$PSXC, na.rm=TRUE), min (data$PSXC, na.rm=TRUE))
  plot (data[, c("THETAP", "PSXC")], type='l', col='blue', 
        xlab='Ad. Pot. T. [K]', ylab='P [hPa]', xlim=c(250,350), ylim=yl)
  points (data$THETAE, data$PSXC, type='l', col='green')
  points (data$THETAQ, data$PSXC, type='l', col='red')
  legend ("topright", legend=c("THETAP", "THETAE", "THETAQ"), 
          lwd=1, col=c('blue', 'green', 'red'), cex=0.75)
  op <- par (mar=c(5,5,1,1)+0.1)
  plot (data[, c("THETA", "PSXC")], type='l', col='blue', xlab='Pot. T. [K]', 
        ylim=yl, ylab='P [hPa]')
  points (data$THETAV, data$PSXC, type='l', col='green')
  legend ("topleft", legend=c("THETA", "THETAV"), lwd=1, 
          col=c('blue', 'green'), cex=0.75)
  AddFooter ()
}

