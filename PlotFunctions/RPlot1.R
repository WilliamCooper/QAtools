### plot 1: construct flight track with map
RPlot1 <- function (data, Flight=NA, Seq=NA) { 
  if (is.na(Seq) || Seq == 1) {
  ## needs LATC, LONC, WDC, WSC, GGALT
  # op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1,oma=c(1.1,0,0,0))
    layout(matrix(1:2, ncol = 1), widths = 1)
    op <- par (mar=c(5,5,2,2),oma=c(1,5,1,1))
    par(cex.lab=2, cex.axis=2)
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
  SE <- getStartEnd (data$Time)
  i <- getIndex (data$Time, SE[1])
  title (sprintf("%s %s %d-%d", Project,
                 strftime(data$Time[i], format="20%y-%m-%d", tz='UTC'), 
                 SE[1], SE[2]), cex=2, font.main=2)
   AddFooter ()
  if (!is.na(Seq) && (Seq == 1)) {return ()}
  }

# Adding THDG to the page with Altitude plots
  layout(matrix(1:3, ncol = 1), widths = 1)
  op <- par (mar=c(5,5,5,1),oma=c(0,3,0,3))
  par(cex.lab=2, cex.main=2)

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
  mtext('Altitude [m]',4,3, cex=1.5)
  DF <- DF[!is.na(data$TASX) & (data$TASX > 110), ]
  title (sprintf ("Mean Difference: PALTF-PA2: %.1f", mean (DF$PALTF-DF$PA2, na.rm=TRUE)))
  
  if ('THDG' %in% VRPlot[[1]]){
    plotWAC(data[,c("Time",'THDG')],ylab=expression("["*degree*"]"))
    title('Heading')
  }
  
  if ('GGQUAL' %in% VRPlot[[1]]){
    plotWAC(data[,c("Time", "GGQUAL")], ylab='Integer')
    title('GPS Precision , 5 = Terra Star Corrected, 2 = Receiving, 1 = Standard GPS, 0 = No FIX')
  }
  
  AddFooter ()
}

