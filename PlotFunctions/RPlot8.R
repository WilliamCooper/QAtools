### plot 8: total pressure (static + dynamic)
RPlot8 <- function (data, Seq=NA, panl=1, ...) { 
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))

  findPairs <- function(DFP) {
    ## find possible pairs
    if ('PS_A' %in% VRPlot[[8]]) {
      pair <- c(which ('PS_A' == VRPlot[[8]]), which ('QC_A' == VRPlot[[8]]))
      DFP$PST1 <- data[, VRPlot[[8]][pair[1]]] + data[, VRPlot[[8]][pair[2]]]
    }
    if ('PSF' %in% VRPlot[[8]]) { ## only for GV
      pair <- c(which ('PSF' == VRPlot[[8]]), which ('QCF' == VRPlot[[8]]))
      DFP$PST2 <- data[, VRPlot[[8]][pair[1]]] + data[, VRPlot[[8]][pair[2]]]
    } else {
      if ('PSFD' %in% VRPlot[[8]]) {  ## only for C-130
        pair <- c(which ('PSFD' == VRPlot[[8]]), which ('QCF' == VRPlot[[8]]))
        DFP$PST2 <- data[, VRPlot[[8]][pair[1]]] + data[, VRPlot[[8]][pair[2]]]
      }
      if ('PSFRD' %in% VRPlot[[8]]) {
        pair <- c(which ('PSFRD' == VRPlot[[8]]), which ('QCFR' == VRPlot[[8]]))
        DFP$PST3 <- data[, VRPlot[[8]][pair[1]]] + data[, VRPlot[[8]][pair[2]]]
      }
    }
    return (DFP)
  }  
  
  panel11 <- function (data) {
    DFP <- data.frame(Time=data$Time)
    # Add total-p variables to DFP
    DFP <- findPairs (DFP)
    if (length (VRPlot[[8]]) > 4) {
      colnames <- c("Time", "PtotAvionics", "PtotF1", "PtotF2")
    } else {
      colnames <- c("Time", "PtotAvionics", "PtotF")
    }
    names(DFP) <- colnames
    plotWAC (DFP,  
      ylab='Ptot [hPa]', legend.position='topright', 
      ylim = YLMF (1, range (as.matrix (DFP[, names(DFP)[-1]]), finite=TRUE)))
    if (length (VRPlot[[8]]) > 4) {
      title (sprintf ("mean differences research 1 & 2 vs avionics: %.2f %.2f",
        mean (DFP$PtotF1 - DFP$PtotAvionics, na.rm=TRUE), 
        mean (DFP$PtotF2 - DFP$PtotAvionics, na.rm=TRUE)), cex.main = cexmain)
    } else {
      title (sprintf ("mean difference research - avionics: %.2f", 
        mean (DFP$PtotF - DFP$PtotAvionics, na.rm=TRUE)), cex.main = cexmain)
    }
  }
  
  panel12 <- function (data) {
    DFP <- data.frame(Time=data$Time)
    # Add total-p variables to DFP
    DFP <- findPairs (DFP)
    PST <- names (DFP)[which ('PST' == substr(names (DFP), 1, 3))]
    DFP[, PST] <- DFP[, PST] - DFP[, PST[1]]
    DFP[, PST] <- SmoothInterp (DFP[, PST])
    plotWAC (DFP[, c('Time', PST[-1])],
      ylab=expression(paste (Delta,' [hPa]')), 
      lwd = c(2.5, 2, 1.5, 1),
      legend.position='topright', 
      ylim = YLMF (2, range (as.matrix (DFP[, PST[-1]]), finite=TRUE))
    )
    title ('1-min-smoothed differences vs avionics total pressure', 
      cex.main = cexmain)
  }
  
  #############################################################
  if (shinyDisplay) {
    switch (panl,
      {
        setMargins (2)
        panel11 (data)
      },
      {
        setMargins (3)
        panel12 (data)
        AddFooter ()
      }
    )
  
  #############################################################
  } else {
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,5.5))
  setMargins (4)
  panel11 (data)
  setMargins (5)
  panel12 (data)
  AddFooter ()
  }
}
