### plot 11: attack, sideslip
RPlot11 <- function (data, ...) {
  ## needs AKRD, PITCH, SSRD, WSC, WDC, GGVEW, GGVNS, VSPD, TASX, THDG, 
  layout(matrix(1:4, ncol = 1), widths = 1)
  op <- par (mar=c(5,5,5,1),oma=c(0,3,0,3))
  par(cex.lab=2, cex.main=2)

  # Plot Pitch and Angle of Attack
  DF <- data[, c("Time", "AKRD", "PITCH")]
  labl<-VRPlot[[11]]
  DF$AOAREF <- DF$PITCH - (180/pi) * data[,labl[grepl("VSPD",labl)]] / data$TASX
  
  plotWAC (DF, ylab=expression (paste ("[",degree,"]")), legend.position='topright')
  titl <-(sprintf ("Mean Difference: AKRD-AOAREF = %.02f", 
                  mean (DF$AKRD-DF$AOAREF, na.rm=TRUE)))
  title(main = paste("Pitch, Angle of Attack",'\n',titl))
  hline (-2); hline (2)
 
  # Plot ADIFR
  if ("ADIFR" %in% VRPlot[[11]]){
    plotWAC(data[,c("Time", "ADIFR")], ylab='[hPa]')
    title('ADIFR')
    grid()
  }
  
  # Plot SideSlip and Reference 
  DF <- data[, c("Time", "SSRD")]
  u <- -1. * data$WSC * sin (data$WDC*pi/180)
  v <- -1. * data$WSC * cos (data$WDC*pi/180)
  DF$SSREF <- -data$THDG + atan2((data$GGVEW-u), (data$GGVNS-v)) * 180/pi
  DF$SSREF[!is.na(DF$SSREF) & (DF$SSREF < -180.)] <- 
    DF$SSREF[!is.na(DF$SSREF) & (DF$SSREF < -180.)] + 360.
  
  plotWAC (DF, ylab=expression (paste ("[",degree,"]")), legend.position='topright')
  titl<- (sprintf ("Mean Difference: SSRD-SSREF = %.02f", 
                  mean (DF$SSRD-DF$SSREF, na.rm=TRUE)))
  title(main = paste("Sideslip",'\n',titl))
  hline (-2); hline (2)
  
  # Plot BDIFR
  if ("BDIFR" %in% VRPlot[[11]]){
    plotWAC(data[,c("Time", "BDIFR")], ylab='[hPa]')
    title('BDIFR')
    grid()
  }
  
  AddFooter ()
}

