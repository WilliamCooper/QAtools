### plot 8: total pressure (static + dynamic)
RPlot8 <- function (data, ...) { 
  op <- par (mar=c(5,4,1,1)+0.1, oma=c(1.1,0,0,0))
  layout(matrix(1:1, ncol = 1), widths = 1, heights = 5)
  DF <- data[, c("Time", "PSF", "PS_A")]
  DF$PSF <- data$PSF + data$QCF
  DF$PS_A <- data$PS_A + data$QC_A
  DF$Diff <- 20*(DF$PSF-DF$PS_A)+500
  if ('AKRD' %in% names (data)) {
    DF$PC <- 500 + 20 * (data$PSF * PCorFunction 
                         (data$PSF, data$QCF, data$AKRD) + data$PSF - data$PSXC)
  }
  colnames(DF) <- c("Time", "PtotF", "PtotAvionics", "Diff*20+500", 'PCOR chk')
  plotWAC (DF, col=c('blue', 'darkgreen', 'red', 'cyan'), ylab='Ptot [hPa]',
           legend.position='topright')
  abline(h=520, col='red', lty=2); abline(h=480, col='red', lty=2)
  title (sprintf ("mean difference: %.1f", 
                  mean (DF$PtotF-DF$PtotAvionics, na.rm=TRUE)), cex.main=0.75)
  hline (0.2); hline (-0.2)
  AddFooter ()
}
