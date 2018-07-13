### plot 19: potential-temperature plots
## needs THETA, THETAV, THETAE, THETAP, THETAQ, PSXC
RPlot19 <- function (data, Seq=NA) {
  layout(matrix(1:3, ncol = 1), widths = 1)
  op <- par (mar=c(5,5,5,1),oma=c(2,2,2,1))
  par(cex.lab=2, cex.main=2)
  
  ylb <- expression (paste (theta[y]," [", degree, "C]"))

  vp <- VRPlot$PV19[match(c('THETA', 'THETAV'), VRPlot$PV19)]
  vp <- vp[!is.na(vp)]
  plotWAC (data[, c("Time", vp)], ylab=ylb,
           legend.position = "top")
  title('Potential Temperatures')
  vp <- VRPlot$PV19[match(c('THETAE', 'THETAP', 'THETAQ'), VRPlot$PV19)]
  vp <- vp[!is.na(vp)]
  ## check THETAP:
  if ('THETAP' %in% vp) {
  data <- data[!is.na(data$EWX), ]
  data$TP2 <- EquivalentPotentialTemperature (data$PSXC, data$ATX, data$EWX)
  }

  ylm<-c(min(data$THETA,na.rm=TRUE), max(data$THETA,na.rm=TRUE)+5)
   if ("THETAQ" %in% vp) {
    if (!("PLWCC" %in% names(data))) {data$PLWCC <- rep (0, nrow(data))}
    data$TQ2 <- WetEquivalentPotentialTemperature (data$PSXC, data$ATX, data$EWX, data$PLWCC)
    if (max (data$THETAE, na.rm=TRUE) < Inf) {
      plotWAC (data[, c('Time', vp)], 
               ylab=ylb, legend.position = "bottomright", ylim=ylm)
      title (sprintf("Mean Differences: THETAE-TP2=%.2f THETAQ-TQ2=%.2f", 
                     mean(data$THETAP-data$TP2, na.rm=TRUE),
                     mean(data$THETAQ-data$TQ2, na.rm=TRUE)))
     
    }
  } else {
    if (max (data$THETAE, na.rm=TRUE) < Inf) {
      plotWAC (data[, c('Time', vp)], 
                 ylab="ad. pot. temperatures", legend.position = "bottomright", ylim=ylm)
        title (sprintf("Mean Difference: THETAE-TP2=%.2f", 
                       mean(data$THETAP-data$TP2, na.rm=TRUE)))
      
    }
  }
  AddFooter ()
  if (!is.na(Seq) && (Seq == 1)) {return()}


# PLOT potential temperatures vs pressure:
  layout(matrix(c(1,2,0,0), nrow = 2, ncol=2, byrow=TRUE))#, widths = c(5,5), heights = 1)
  
  vp <- VRPlot$PV19[match(c('THETAE', 'THETAP', 'THETAQ'), VRPlot$PV19)]
  yl <- c(max (data$PSXC, na.rm=TRUE), min (data$PSXC, na.rm=TRUE))
  plot (data[, c("THETAP", "PSXC")], type='l', col='blue', 
        xlab='THETAP [K]', ylab='Pressure [hPa]', xlim=c(250,350), ylim=yl)
  points (data$THETAE, data$PSXC, type='l', col='green')
  if ('THETAQ' %in% vp) {
    points (data$THETAQ, data$PSXC, type='l', col='red')
    legend ("topleft", legend=c("THETAP", "THETAE", "THETAQ"), 
            lwd=1, col=c('blue', 'green', 'red'), cex=2)
  } else {
    legend ("topleft", legend=c("THETAP", "THETAE"), 
      lwd=1, col=c('blue', 'green'), cex=2)
  }

  plot (data[, c("THETA", "PSXC")], type='l', col='blue', xlab='THETA [K]', 
        ylim=yl, ylab='Pressure [hPa]')
  points (data$THETAV, data$PSXC, type='l', col='green')
  legend ("topleft", legend=c("THETA", "THETAV"), lwd=1, 
          col=c('blue', 'green'), cex=2)
  AddFooter ()
}

