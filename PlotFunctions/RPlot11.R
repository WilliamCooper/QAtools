### plot 11: attack, sideslip
RPlot11 <- function (data, Seq=NA, panl=1, ...) {
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  panel11 <- function(data) {
    DF <- data[, c("Time", "AKRD", "PITCH")]
    if ('GGVSPD' %in% VRPlot[[11]]) {
      VSPD <- data$GGVSPD
    } else if ('VSPD_A' %in% VRPlot[[11]]) {
      VSPD <- data$VSPD_A
    } else {
      VSPD <- NA
    }
    DF$AOAREF <- DF$PITCH - (180/pi) * VSPD / data$TASX
    ifelse (exists('panel1ylim'), 
      plotWAC (DF, lwd=c(2,1,1), lty=c(1,2,1), ylim=panel1ylim,
        ylab=expression (paste ("AKRD [",degree,"]")), 
        legend.position='topright'),
      plotWAC (DF, lwd=c(2,1,1), lty=c(1,2,1), 
        ylab=expression (paste ("AKRD [",degree,"]")), 
        legend.position='topright')
    )
    title (sprintf ("mean diff AKRD-AOAREF = %.02f", 
      mean (DF$AKRD-DF$AOAREF, na.rm=TRUE)), cex.main=0.75)
    hline (2); hline (4)
  }
  
  panel12 <- function(data) {
    # Plot ADIFR
    if ("ADIFR" %in% VRPlot[[11]]){
      ifelse (exists ('panel2ylim'),
        plotWAC(data[,c("Time", "ADIFR")], ylab='ADIFR [hPa]', ylim=panel2ylim),
        plotWAC(data[,c("Time", "ADIFR")], ylab='ADIFR [hPa]')
      )
      grid()
    }
  }
  
  panel13 <- function(data) {
    DF <- data[, c("Time", "SSRD")]
    u <- -1. * data$WSC * sin (data$WDC*pi/180)
    v <- -1. * data$WSC * cos (data$WDC*pi/180)
    DF$SSREF <- -data$THDG + atan2((data$GGVEW-u), (data$GGVNS-v)) * 180/pi
    DF$SSREF[!is.na(DF$SSREF) & (DF$SSREF < -180.)] <- 
      DF$SSREF[!is.na(DF$SSREF) & (DF$SSREF < -180.)] + 360.
    ifelse (exists ('panel3ylim'),
      plotWAC (DF, ylab=expression (paste ("SSRD [",degree,"]")), 
        legend.position='topright', ylim=panel3ylim),
      plotWAC (DF, ylab=expression (paste ("SSRD [",degree,"]")), 
        legend.position='topright')
    )
    title (sprintf ("mean diff SSRD-SSREF = %.02f", 
      mean (DF$SSRD-DF$SSREF, na.rm=TRUE)), cex.main=0.75)
    hline (-2); hline (2)
  }
  
  panel14 <- function(data) {
    # Plot BDIFR
    if ("BDIFR" %in% VRPlot[[11]]){
      ifelse (exists ('panel4ylim'),
        plotWAC(data[,c("Time", "BDIFR")], ylab=' BDIFR [hPa]',
          ylim=panel4ylim),
        plotWAC(data[,c("Time", "BDIFR")], ylab=' BDIFR [hPa]')
      )
      grid()
    }
  }
  
  ##############################################################
  if (shinyDisplay) {
    op <- par (mfrow=c(1,1), mar=c(5,5,1,1)+0.1,oma=c(1.1,0,0,0))
    switch(panl,
      {
        op <- par (mar=c(1,5,1,1)+0.1)
        panel11(data)
      },
      {
        op <- par (mar=c(1,5,1,1)+0.1)
        panel12(data)
      },
      {
        op <- par (mar=c(1,5,1,1)+0.1)
        panel13(data)
      },
      {
        panel14(data)
        AddFooter()
      }
    )
    
    ##############################################################
  } else {
    ## needs AKRD, PITCH, SSRD, WSC, WDC, GGVEW, GGVNS, TASX, THDG, GGVSPD or VSPD_A
    layout(matrix(1:4, ncol = 1), widths = 1, heights = c(5,5,5,6))
    op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
    panel11(data)
    panel12(data)
    panel13(data)
    op <- par (mar=c(5,4,1,1)+0.1)
    panel14(data)
    AddFooter ()
  }
}
