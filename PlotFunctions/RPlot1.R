### plot 1: construct flight track with map
RPlot1 <- function (data, Flight=NA, Seq=NA) { 
  if (is.na(Seq) || Seq == 1) {
  ## needs LATC, LONC, WDC, WSC, GGALT
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1,oma=c(1.1,0,0,0))
  dt <- data
  dt$LATC <- data[ ,eval (parse (text=sprintf('VRPlot[[1]][%d]', 
                                              which (grepl ('LAT', VRPlot[[1]])))))]
  dt$LONC <- data[ ,eval (parse (text=sprintf('VRPlot[[1]][%d]', 
                                              which (grepl ('LON', VRPlot[[1]])))))]
  dt$WDC <- data[ ,eval (parse (text=sprintf('VRPlot[[1]][%d]', 
                                             which (grepl ('WD', VRPlot[[1]])))))]
  dt$WSC <- data[ ,eval (parse (text=sprintf('VRPlot[[1]][%d]', 
                                             which (grepl ('WS', VRPlot[[1]])))))]
  plotTrack(dt, .Spacing=60, .WindFlags=10)
  AddFooter ()
  if (!is.na(Seq) && (Seq == 1)) {return ()}
  }
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1,oma=c(1.1,0,0,0))
  SE <- getStartEnd (data$Time)
  i <- getIndex (data$Time, SE[1])
  title (sprintf("%s %s %d-%d", Flight,
                 strftime(data$Time[i], format="20%y-%m-%d", tz='UTC'), 
                 SE[1], SE[2]), cex=0.75, font.main=1)
  par(mar=c(5,5,2,4)+0.1)
  DF <- data.frame(Time=data$Time)
  DF$GGALTF <- data[, 'GGALT']/0.3048
  DF$PALTF <- data[, 'PALT']/0.3048
  # data[is.na(data$PSXC), "PSXC"] <- 1013.25
  DF$PA2 <- data$PSXC
  # DF <- DF[!is.na (DF$PA2), ]
  DF$PA2[!is.na(DF$PA2)] <- PressureAltitude (DF$PA2[!is.na(DF$PA2)]) / 0.3048
  plotWAC(DF[, c("Time", "GGALTF", "PALTF", "PA2")], ylab="Altitude [ft]")
  #axis(4,at=axTicks(2),labels=round(axTicks(2)*0.3048)) ## this adds a metric axis on right side but at even foot intervals
  axis(4, labels = NA, tck = 0.02,col='white',lwd=0,lwd.ticks=3) # erase the foot ticks plotWAC puts on right axis
  par(new=TRUE);plot(data$Time,data[, 'GGALT'], axes=F,bty='n', xlab='',ylab='',type='n');axis(4,tck=0.02) ## this adds a metric axis on the right side at even meter intervals
  mtext('Altitude [m]',4,3)
  DF <- DF[!is.na(data$TASX) & (data$TASX > 110), ]
  title (sprintf ("mean diff in pressure altitude: %.1f", mean (DF$PALTF-DF$PA2, na.rm=TRUE)))
  AddFooter ()
}

