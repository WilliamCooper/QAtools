### plot 9: wind
RPlot9 <- function (data, Seq=NA) {
  ## needs WDC, WSC, WIC, IWD, IWS, ADIFR, QCF, PSF
  op <- par (mar=c(2,5,1,1)+0.1,oma=c(1.1,0,0,0))
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  line.widths <- c(1,1,2)
  line.types <- c(1,3,2)
  # set high transparency (30) to avoid obscuring first trace
  tgreen <- rgb(0,200,0,120,maxColorValue=255)
  cs <- c('blue', tgreen, 'red', 'cyan', 'darkorange', 'violet')
  if (is.na (Seq) || (Seq == 1)) {
    WD <- VRPlot[[9]][grepl ('^WD', VRPlot[[9]])]
    if ('IWD' %in% VRPlot[[9]]) {WD <- c(WD, 'IWD')}
    WS <- VRPlot[[9]][grepl ('^WS', VRPlot[[9]])]
    if ('IWS' %in% VRPlot[[9]]) {WS <- c(WS, 'IWS')}
    WI <- VRPlot[[9]][grepl ('^WI', VRPlot[[9]])]
    plotWAC (data[, c("Time", WD)], 
             col=cs, lwd=line.widths, lty=line.types, ylab=expression (paste ("WDC [",degree,"]")),legend.position=NA,cex.axis=1.5,cex.lab=1.5)
    legend('bottomright',WD,col=cs,
           text.col=cs,lty=c(1,3),lwd=c(1,1))
    hline (0); hline (90); hline (180); hline (270); hline (360)
    plotWAC (data[, c("Time", WS)], 
             col=cs, lwd=line.widths, lty=line.types, ylab="WSC [m/s]",legend.position=NA,cex.axis=1.5,cex.lab=1.5)
    legend('bottomright',WS,col=cs,text.col=cs,lty=c(1,3),lwd=c(1,1))
    op <- par (mar=c(5,5,2,1)+0.1)
    # cf <- c(4.90, 13.36, 8.04)
    # data$AK <- cf[1] + data$ADIFR/data$QCF * (cf[2] 
    #                                     + cf[3] * MachNumber(data$PSF, data$QCF))
    # next line is no longer necessary; only in original HIPPO processing
    # data$VSPD_G <- (data$VSPD_G + 0.06) / 1.02
    # data$WIX <- data$WIC + (data$AK-data$AKRD)*pi*data$TASF/180. + (data$VSPD_A-data$VSPD)
    # data$WIXS <- SmoothInterp (data$WIX)
    plotWAC (data[, c("Time", WI)], ylab="vertical wind WIC [m/s]",cex.axis=1.5,cex.lab=1.5)
    title (sprintf ("flight-average vertical wind: WIC %.02f", 
                    mean (data[,WI[1]], na.rm=TRUE)), cex.main=1.5)
    hline (2); hline (-2); hline (0,'red')
    AddFooter ()
    if (!is.na(Seq) && (Seq == 1)) {return()}
  }
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  data$IUX <- data$IWS * sin (data$IWD*pi/180)
  data$UIC <- data$WSC * sin (data$WDC*pi/180)
  plotWAC (data[, c("Time", "UIC", "IUX")], col=cs, lwd=line.widths, lty=line.types, 
           ylab="easterly wind [m/s]",legend.position=NA)
  legend('bottom',c("UIC", "IUX"),col=c("blue",tgreen),text.col=c("blue",tgreen),lty=c(1,3),lwd=c(1,1),cex=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  data$IVY <- -data$IWS * cos (data$IWD*pi/180)
  data$VIC <- -data$WSC * cos (data$WDC*pi/180)
  plotWAC (data[, c("Time", "VIC", "IVY")], col=cs, lwd=line.widths, lty=line.types, 
           ylab="southerly wind [m/s]",legend.position=NA)
  legend('bottom',c("VIC", "IVY"),col=c("blue",tgreen),text.col=c("blue",tgreen),lty=c(1,3),lwd=c(1,1),cex=0.75)
  AddFooter ()
}

