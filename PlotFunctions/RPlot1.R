### plot 1: construct flight track with map
RPlot1 <- function (data, Flight=NA, Seq=NA, panl=1) { 
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  panel11 <- function(data, Flight) {
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
    if (shinyDisplay)  
    {
      title (sprintf("%s %d-%d",
        strftime(data$Time[i], format="20%y-%m-%d", tz='UTC'), 
        SE[1], SE[2]), cex = cexmain, font.main=1)
    } else
    {
      title (sprintf("%s %s %d-%d", Flight,
        strftime(data$Time[i], format="20%y-%m-%d", tz='UTC'), 
        SE[1], SE[2]), cex = cexmain, font.main=1)
    }
  }
  
  panel21 <- function (data) {
    DF <- data.frame(Time=data$Time)
    DF$GGALTF <- data[, 'GGALT']/0.3048
    DF$PALTF <- data[, 'PALT']/0.3048
    DF$PA2 <- data$PSXC
    DF$PA2[!is.na(DF$PA2)] <- PressureAltitude (DF$PA2[!is.na(DF$PA2)]) / 0.3048
    # Get the altitude range:
    ylm <- range (as.matrix (DF[, -1]), finite = TRUE)
    plotWAC(DF, 
      ylim = YLMF (1, ylm),
      ylab="Altitude [ft]")
    ylmm <- ceiling (ylm * 0.3048 / 1000) *  1000
    at4 <- seq (ylmm[1], ylmm[2], by=1000)
    at4l <- format (at4/1000, 0)
    axis (4, at = at4 / 0.3048, labels = at4l,
      col.axis = 'red', col = 'red', col.ticks = 'red') ## this adds a metric axis on right side
    # axis(4, labels = NA, tck = 0.02,col='white',lwd=0,lwd.ticks=3) # erase the foot ticks plotWAC puts on right axis
    # par(new=TRUE)
    #plot(data$Time,data[, 'GGALT'], axes=FALSE, bty='n', xlab='', ylab='',
    #  type='n')
    # axis(4,tck=0.02) ## this adds a metric axis on the right side at km intervals
    mtext('Altitude [km]', 4, 2, col = 'red')
    DF <- DF[!is.na(data$TASX) & (data$TASX > 110), ]
    ## This isn't needed any more; it was an old check that pressure altitude was OK.
    ## It's always OK now.
    if (abs (mean (DF$PALTF-DF$PA2, na.rm=TRUE)) > 0.05) {
      title (sprintf ("mean diff in pressure altitude: %.1f", 
        mean (DF$PALTF-DF$PA2, na.rm=TRUE)))
    }
  }
  
  panel22 <- function (data) {
    if ('THDG' %in% names(data)) {
      plotWAC(data[,c("Time",'THDG')],
        ylim = YLMF (2, c(0,360)),
        ylab=expression("THDG ["*degree*"]"))
      title('Heading', cex.main = cexmain)
    }
  }
  
  panel23 <- function (data) 
    if ('GGQUAL' %in% names(data)) {
      plotWAC(data[,c("Time", "GGQUAL")], ylim=c(0,10))
      title('GPS Quality: 5 = TerraStar corrected, 2 = TerraStar converging, 9 = WAAS, 1 = Standard GPS, 0 = No FIX',
        cex.main = cexmain)
    }
  
  ####################################################
  if (shinyDisplay) {
    op <- par (mfrow=c(1,1), mar=c(5,5,1,4.5)+0.1,oma=c(1.1,0,0,0)) 
    # oma is for Footer; non-standard 4.5 for right axis
    if (Seq == 1) {
      panel11 (data, Flight)
      AddFooter()
    } else switch (panl,   # plot 2: altitude
      {
        op <- par (mar=c(1,5,1,4.5)+0.1)
        panel21 (data)
      },
      {
        op <- par (mar=c(1,5,1,4.5)+0.1)
        panel22 (data)
      }, 
      {
        panel23 (data)
        AddFooter()
      }
    )
    
    ####################################################
  } else {
    if (is.na(Seq) || Seq == 1) {
      ## needs LATC, LONC, WDC, WSC, GGALT for track plots
      op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1,oma=c(1.1,0,0,0))
      panel11 (data, Flight)
      AddFooter ()
      if (!is.na(Seq) && (Seq == 1)) {return ()}
    }
    layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,4))
    op <- par (mar=c(2,5,1,4.5)+0.1)
    panel21 (data)
    # par(cex.lab=2, cex.main=2)
    panel22 (data)
    op <- par (mar=c(5,5,1,4.5)+0.1)
    panel23 (data)
    AddFooter()
  }
}
