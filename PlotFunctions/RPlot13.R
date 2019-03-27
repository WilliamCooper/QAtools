### plot 13: IRU continued, ACINS, VSPD
RPlot13 <- function (data, Seq=NA, panl=1, ...) {
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  panel11 <- function(data) {
    ACINS <- VRPlot[[13]]
    ACINS <- ACINS[which("ACINS" == substr(ACINS, 1, 5))]
    DF <- data[, c("Time", ACINS)]
    plotWAC (DF, ylab="ACINS", 
      ylim = YLMF (1, range(as.matrix (data[, ACINS]), finite=TRUE)))
    title (sprintf ("mean vertical acceleration: %.3f", 
      mean (data[, VRPlot[[13]][1]], na.rm=TRUE)))
  }
  
  panel12 <- function(data) {
    VSPD <- VRPlot[[13]]
    VSPD <- VSPD[grep("VSPD", VSPD)]
    plotWAC (data[, c("Time", VSPD)], legend.position='topright',
      ylim = YLMF (2, range (as.matrix (data[, VSPD]), finite=TRUE)))
    title (sprintf ("mean vertical speed: %.3f (IRS) and %.3f (GPS)",
      mean (data$VSPD, na.rm=TRUE), mean (data$VSPD_A, na.rm=TRUE)))
  }
  
  panel13 <- function(data) {
    ALT <- VRPlot[[13]]
    ALT <- ALT[grep("ALT", ALT)]
    plotWAC (data[, c("Time", ALT)], legend.position = "top", 
      ylim = YLMF (3, range (as.matrix (data[, ALT]), finite=TRUE)))
  }
  
  
  #########################################################
  if (shinyDisplay) {
    switch(panl,
      {
        setMargins (2)
        panel11 (data)
      },
      {
        setMargins (2)
        panel12 (data)
      },
      {
        setMargins (3)
        panel13 (data)
        AddFooter()
      }
    )
    #########################################################
  } else {
    layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
    setMargins (4)
    panel11(data)
    panel12(data)
    setMargins (5)
    panel13(data)
    AddFooter ()
  }
}

