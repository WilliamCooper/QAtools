### plot 13: IRU continued, ACINS, VSPD
RPlot13 <- function (data, Seq=NA, panl=1, ...) {
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  panel11 <- function(data) {
    ACINS <- VRPlot[[13]]
    ACINS <- ACINS[which("ACINS" == substr(ACINS, 1, 5))]
    DF <- data[, c("Time", ACINS)]
    ifelse (exists ('panel1ylim'),
      plotWAC (DF, ylab="ACINS", ylim=panel1ylim),
      plotWAC (DF, ylab="ACINS")
    )
    title (sprintf ("mean vertical acceleration: %.3f", 
      mean (data[, VRPlot[[13]][1]], na.rm=TRUE)))
  }
  
  panel12 <- function(data) {
    VSPD <- VRPlot[[13]]
    VSPD <- VSPD[grep("VSPD", VSPD)]
    ifelse (exists ('panel2ylim'),
      plotWAC (data[, c("Time", VSPD)], legend.position='topright',
        ylim=panel2ylim),
      plotWAC (data[, c("Time", VSPD)], legend.position='topright')
    )
    title (sprintf ("mean vertical speed: %.3f (IRS) and %.3f (GPS)",
      mean (data$VSPD, na.rm=TRUE), mean (data$VSPD_A, na.rm=TRUE)))
  }
  
  panel13 <- function(data) {
    ALT <- VRPlot[[13]]
    ALT <- ALT[grep("ALT", ALT)]
    ifelse (exists ('panel3ylim'),
      plotWAC (data[, c("Time", ALT)], legend.position = "top", ylim=panel3ylim),
      plotWAC (data[, c("Time", ALT)], legend.position = "top")
    )
  }
  
  
  #########################################################
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
        panel13(data)
        AddFooter()
      }
    )
    #########################################################
  } else {
    layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
    op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
    panel11(data)
    panel12(data)
    op <- par (mar=c(5,4,1,1)+0.1)
    panel13(data)
    AddFooter ()
  }
}

