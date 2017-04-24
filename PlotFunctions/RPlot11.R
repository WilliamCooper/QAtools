### plot 11: attack, sideslip
RPlot11 <- function (data, ...) {
  ## needs AKRD, PITCH, SSRD, WSC, WDC, GGVEW, GGVNS, VSPD, TASX, THDG, 
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
  DF <- data[, c("Time", "AKRD", "PITCH")]
  DF$AOAREF <- DF$PITCH - (180/pi) * data[, VRPlot[[11]][3]] / data$TASX
  plotWAC (DF, lwd=c(2,1,1), lty=c(1,2,1), ylab=expression (paste ("AKRD [",degree,"]")), legend.position='topright')
  title (sprintf ("mean diff AKRD-AOAREF = %.02f", 
                  mean (DF$AKRD-DF$AOAREF, na.rm=TRUE)), cex.main=0.75)
  hline (2); hline (4)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "SSRD")]
  u <- -1. * data$WSC * sin (data$WDC*pi/180)
  v <- -1. * data$WSC * cos (data$WDC*pi/180)
  DF$SSREF <- -data$THDG + atan2((data$GGVEW-u), (data$GGVNS-v)) * 180/pi
  DF$SSREF[!is.na(DF$SSREF) & (DF$SSREF < -180.)] <- 
    DF$SSREF[!is.na(DF$SSREF) & (DF$SSREF < -180.)] + 360.
  plotWAC (DF, ylab=expression (paste ("SSRD [",degree,"]")), legend.position='topright')
  title (sprintf ("mean diff SSRD-SSREF = %.02f", 
                  mean (DF$SSRD-DF$SSREF, na.rm=TRUE)), cex.main=0.75)
  hline (-2); hline (2)
  AddFooter ()
}

