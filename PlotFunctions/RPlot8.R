### plot 8: total pressure (static + dynamic)
RPlot8 <- function (data, Seq=NA, panl=1, ...) { 
  op <- par (mar=c(5,4,1,1)+0.1, oma=c(1.1,0,0,0))
  layout(matrix(1:1, ncol = 1), widths = 1, heights = 5)
  DFP <- data.frame(Time=data$Time)
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
  DFP$Diff <- 20 * (DFP$PST1 - DFP$PST2) + 500
  if (length (VRPlot[[8]]) > 4) {
    DFP$Diff2 <- 20*(DFP$PST1-DFP$PST3)+500
  }
  # if ('PC' %in% names (data)) {
  #   DFP$PC <- 500 + 20 * (data$PSF * PCorFunction 
  #                        (data$PSF, data$QCF, data$AKRD) + data$PSF - data$PSXC)
  # }
  if (length (VRPlot[[8]]) > 4) {
    colnames <- c("Time", "PtotAvionics", "PtotF1", "PtotF2", "Diff1", "Diff2")
  } else {
    colnames <- c("Time", "PtotAvionics", "PtotF", "Diff1")
  }
  names(DFP) <- colnames
  # colnames(DF) <- c("Time", "PtotF", "PtotAvionics", "Diff*20+500", 'PCOR chk')
  ifelse (exists ('panel1ylim'),
    plotWAC (DFP, col=c('blue', 'darkgreen', 'red', 'cyan', 'darkorange'), 
      ylab='Ptot [hPa]', legend.position='topright', ylim=panel1ylim),
    plotWAC (DFP, col=c('blue', 'darkgreen', 'red', 'cyan', 'darkorange'), 
      ylab='Ptot [hPa]', legend.position='topright')
  )
  abline(h=520, col='red', lty=2); abline(h=480, col='red', lty=2)
  if (length (VRPlot[[8]]) > 4) {
    title (sprintf ("mean differences avionics - research 1 & 2: %.2f %.2f",
      (mean (DFP$Diff1, na.rm=TRUE)-500)/20, (mean (DFP$Diff2, na.rm=TRUE)-500)/20), cex.main=0.75)
  } else {
    title (sprintf ("mean difference avionics-research: %.2f", 
                  (mean (DFP$Diff1, na.rm=TRUE)-500)/20), cex.main=0.75)
  }
  hline (0.2); hline (-0.2)
  AddFooter ()
}
