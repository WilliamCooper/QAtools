RPlot14 <- function(data, Seq=NA, panl=1, ...) {
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  panel11 <- function(data) {
    par(cex.lab=1, cex.main=1)
    
    # Panel 1: RSTB}
    ylb <- expression (paste ("[", degree, "C]")) 
    if (any(grepl("^RSTB", VRPlot[[14]]))) {
      RSTB <- VRPlot[[14]][grepl('^RSTB', VRPlot[[14]])]
      ylb1 <- expression (paste("RSTB [", degree, "C]"))
      plotWAC (data[c('Time', RSTB)], ylab=ylb1, 
        ylim = YLMF (1, range (as.matrix (data[, RSTB]), finite=TRUE)))
      title('Radiometric Temperature')
    }
  }
  
  panel12 <- function(data) {
    if (any(grepl("^RSTB", VRPlot[[14]]))) {
      RSTB <- VRPlot[[14]][grepl('^RSTB', VRPlot[[14]])]
      if (length(RSTB) > 1) {
        ylb2 <- expression (paste (Delta," [", degree, "C]"))
        plotWAC(data$Time, data[, RSTB[1]]-data[, RSTB[2]],
          ylim = YLMF (2, c(-2, 2)),
          ylab=ylb2)
        abline(h=-0.3, lty=2); abline(h=0.3, lty=2)
        title (sprintf('%s - %s', RSTB[1], RSTB[2]))
      }
    }
  }
  
  panel13 <- function(data) {
    # next panel: RSTT
    if ("RSTT" %in% VRPlot[[14]]) {
      ylb3 <- expression (paste ("RSTT [", degree, "C]"))
      plotWAC (data[, c("Time", "RSTT")], ylab=ylb3, 
        ylim = YLMF (3, range (as.matrix (data[, 'RSTT']), finite=TRUE)))
    }
  }
  
  panel14 <- function(data) {
    # Panel 4: TRSTB
    if ("TRSTB" %in% VRPlot[[14]]) {
      ylb4 <- expression (paste ('TRSTB [', degree, "C]"))
      plotWAC (data[, c("Time", "TRSTB")], ylab=ylb4, 
        ylim = YLMF (4, range (data[, 'TRSTB'], finite=TRUE)))
      title('RSTB Sensor-Heat Setting')
      # par(new=T)
      # plotWAC(data[,c("Time","GGALT")], axes=FALSE, xlab=NA, ylab=NA, col='black', lwd=1)
      # axis(side=4)
      # mtext(side=4, line=3, 'Altitude [m]')
      
    }
  }
  
  
  ###########################################################
  if(shinyDisplay) {
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
        setMargins (2)
        panel13 (data)
      },
      {
        setMargins (3)
        panel14 (data)
        AddFooter()
      }
    )
    ###########################################################
  } else {
    ## be sure that the variables are in 'VarList'. If not, where VarList
    ## is defined, add the new variable to the variable names or follow the
    ## present definition with VarList <- c(VarList, "NewVariable1", "NewVariable2")
    # next just resets geometry, in case previous plot used multiple panes
    # layout(matrix(1:1, ncol = 1), widths = 1, heights = c(5,6))
    # op <- par (mar=c(5,4,1,2.5)+0.1,oma=c(1.1,0,0,0))
    
    layout(matrix(1:4, ncol = 1), widths=1, heights=c(5,5,5,6))
    setMargins (4)
    panel11 (data)
    panel12 (data)
    panel13 (data)
    # # next panel: VISB    
    #   if ("VISB" %in% VRPlot[[14]]) {
    #     plotWAC (data[, c("Time", "VISB")], ylab=ylb)
    #   } else { 
    #   
    # # alternate: IRBC 
    #   if ("IRBC" %in% VRPlot[[14]] && "IRTC" %in% VRPlot[[14]]) {
    #     plotWAC (data[, c("Time", "IRBC", "IRTC")], ylab=ylb)
    #   } 
    #   }
    setMargins (5)
    panel14 (data)
    AddFooter ()
  }
}
