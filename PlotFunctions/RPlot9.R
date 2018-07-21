### plot 9: Wind
## needs WDC, WSC, WIC, IWD, IWS, ADIFR, QCF, PSF

RPlot9 <- function (data, Seq=NA) {
  layout(matrix(1:4, ncol = 1), widths = 1)
  op <- par (mar=c(5,5,5,1),oma=c(0,3,0,3))
  par(cex.lab=2, cex.main=2)
   
  # set high transparency (30) to avoid obscuring first trace
  red <- rgb(0,200,0,120,maxColorValue=255)
  cs <- c('blue',  'red', 'cyan', 'darkorange', 'violet')
  line.widths <- c(2,2,2)
  line.types <- c(1,3,2)
  
  if (is.na (Seq) || (Seq == 1)) {
# Plot Wind DIRECTION
    WD <- VRPlot[[9]][grepl ('^WD', VRPlot[[9]])]
    if ('IWD' %in% VRPlot[[9]]) {WD <- c(WD, 'IWD')}
    WS <- VRPlot[[9]][grepl ('^WS', VRPlot[[9]])]
    if ('IWS' %in% VRPlot[[9]]) {WS <- c(WS, 'IWS')}
    WI <- VRPlot[[9]][grepl ('^WI', VRPlot[[9]])]
    plotWAC (data[, c("Time", WD)], 
             #col=cs, lwd=line.widths, lty=line.types, 
             ylab=expression (paste ("WDC [",degree,"]")), ylim=c(0,360))
    grid(ny=NA)
    
    title('Wind Direction')
    # legend('bottomright',WD,col=cs,
    #        text.col=cs,lty=c(1,3),lwd=c(1,1))
    hline (0); hline (90); hline (180); hline (270); hline (360)

# Plot Wind SPEED    
    plotWAC (data[, c("Time", WS)], 
             #col=cs, lwd=line.widths, lty=line.types, 
              ylab="WSC [m/s]")
    title('Wind Speed')
    # legend('bottomright',WS,col=cs,text.col=cs,lty=c(1,3),lwd=c(1,1))
    # op <- par (mar=c(5,5,2,1)+0.1)
    # cf <- c(4.90, 13.36, 8.04)
    # data$AK <- cf[1] + data$ADIFR/data$QCF * (cf[2] 
    #                                     + cf[3] * MachNumber(data$PSF, data$QCF))
    # next line is no longer necessary; only in original HIPPO processing
    # data$VSPD_G <- (data$VSPD_G + 0.06) / 1.02
    # data$WIX <- data$WIC + (data$AK-data$AKRD)*pi*data$TASF/180. + (data$VSPD_A-data$VSPD)
    # data$WIXS <- SmoothInterp (data$WIX)

# Plot VERTICAL Wind    
    plotWAC (data[, c("Time", WI)], ylab="WIC [m/s]")
    title (sprintf ("Flight-Average Vertical Wind: %.02f", 
                    mean (data[,WI[1]], na.rm=TRUE)))
    hline (2); hline (-2); hline (0,'red')
   
    AddFooter ()
    if (!is.na(Seq) && (Seq == 1)) {return()}
  }
  
# End First page of wind plots
  
  op <- par (mar=c(5,5,5,1),oma=c(0,3,0,3))
  layout(matrix(1:4, ncol = 1))
  data$IUX <- data$IWS * sin (data$IWD*pi/180)
  data$UIC <- data$WSC * sin (data$WDC*pi/180)
  plotWAC (data[, c("Time", "UIC", "IUX")], #col=cs, lwd=line.widths, lty=line.types, 
           ylab="easterly wind [m/s]")
  #legend('bottom',c("UIC", "IUX"),col=c("blue",red),text.col=c("blue",red),lty=c(1,3),lwd=c(1,1),cex=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  data$IVY <- -data$IWS * cos (data$IWD*pi/180)
  data$VIC <- -data$WSC * cos (data$WDC*pi/180)
  plotWAC (data[, c("Time", "VIC", "IVY")], col=cs, lwd=line.widths, lty=line.types, 
           ylab="noutherly wind [m/s]")
  #legend('bottom',c("VIC", "IVY"),col=c("blue",red),text.col=c("blue",red),lty=c(1,3),lwd=c(1,1),cex=0.75)
  AddFooter ()
}

