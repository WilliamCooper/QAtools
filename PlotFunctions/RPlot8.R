### plot 8: total pressure (static + dynamic)
RPlot8 <- function (data, ...) { 
  layout(matrix(1:3, ncol = 1), widths = 1)
  op <- par (mar=c(5,5,5,1),oma=c(0,3,0,3))
  par(cex.lab=2, cex.main=2)

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
  DFP$Diff <-  (DFP$PST1 - DFP$PST2) 
  if (length (VRPlot[[8]]) > 4) {
    DFP$Diff2 <-(DFP$PST1-DFP$PST3)
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
  
  
  plotWAC (DFP[,colnames[!grepl('Diff',colnames)]], ylab='Total Pressures [hPa]')
  title('Total Pressures')
  
  #
  plotWAC(DFP[,c("Time", colnames[grepl('Diff',colnames)])], 
          ylim=c(-2,2), ylab='Differences [hPa]')
  abline(h=-1, col='red', lty=2); abline(h=1, col='red', lty=2)
  if (length (VRPlot[[8]]) > 4) {
     title (sprintf ("Mean Differences: avionics - research [1, 2]: [%.2f, %.2f]",
      (mean (DFP$Diff1, na.rm=TRUE)-500)/20, (mean (DFP$Diff2, na.rm=TRUE)-500)/20))
  } else {
    title (sprintf ("Mean Difference: avionics-research: %.2f", 
                  (mean (DFP$Diff1, na.rm=TRUE)-500)/20))
  }
  hline (0.2); hline (-0.2)
  AddFooter ()
}
