
### plot 7: Dynamic pressure; also TAS and MACH
RPlot7 <- function (data, Seq=NA) { 
  layout(matrix(1:4, ncol = 1), widths = 1)
  op <- par (mar=c(5,5,5,1),oma=c(0,3,0,3))
  par(cex.lab=2, cex.main=2)

  if (is.na (Seq) || Seq == 1) {
    # Use uncorrected QCs for the first panel:
    QC <- VRPlot[[7]][grepl ('^QC', VRPlot[[7]])]
    QC <- QC[!grepl ('QC_A',QC) & !grepl ('C$', QC)]
    plotWAC (data[, c("Time", QC)], ylab='QCy [hPa]', 
             ylim=c(max(c(0,min(data[,QC], na.rm=TRUE))),
                    min(c(max(data[,QC], na.rm=TRUE),200))))
    labl <- QC
    #labl <- sub("QC", "", labl)
    titl <- "Mean differences: "
   
    ir <- which ('QCF' == labl)
    if (length (ir) != 1) {
      ir <- which ('QCR' %in% labl)
    }
    if (length (ir) != 1) {ir <- 1}
    for (i in 1:length(labl)) {
      if (i == ir) {next}
      titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[ir],
                      mean(data[, QC[i]] - data[, QC[ir]], na.rm=TRUE))
    }
    title(main = paste("Dynamic Pressures",'\n',titl))

    
    # Corrected QCs for the second panel
    QC <- VRPlot[[7]][grepl ('^QC', VRPlot[[7]])]
    QC <- QC[grepl ('QC_A', QC) | grepl ('C$', QC)]
    ir <- which ('QCXC' == QC)
    if (length (ir) != 1) {
      ir <- which ('QCFC' == QC)
    }
    if (length(ir) != 1) {ir <- 1}
    plotWAC (data[, c("Time", QC)], ylab='QCyC [hPa]',
             ylim=c(max(c(0,min(data[,QC], na.rm=TRUE))),
                    min(c(max(data[,QC], na.rm=TRUE),200))))
  
    labl <- QC
    #labl <- sub("QC", "", labl)
    titl <- "Mean differences: "
    for (i in 1:length(labl)) {
      if (i == ir) {next}
      titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[ir],
                      mean(data[, QC[i]] - data[, QC[ir]], na.rm=TRUE))
    }
    title(titl)
    
    
    # Difference plot
    if ('QC_A' %in% QC) {
      plotWAC(data$Time, (data$QC_A-data[, QC[ir]]), ylab='Difference [hPa]',            
              ylim=c(-4,4))
      # These next lines set grid to yrange spanning -4 to 4.
      for (ny in seq(-4,4,by=1)){
        abline(h=ny, lwd=1, lty=3, col='gray')
      }
      abline (h=-2, lwd=1.5, lty=2); abline (h=2, lwd=1.5, lty=2)
      title('QC_A minus QCFC')
    }
    
    # axis (4, at=c(100,120,140), labels=c("-2", "0", "2"), col='brown', col.axis='brown', cex.axis=0.7)
    # abline (h=100, col='brown', lty=2); abline (h=140, col='brown', lty=2)
    # ltext <- sprintf("brown: (%s-%s)*10+120", 'QC_A', QC[ir])
    # legend("topleft", legend=c(ltext, 
    #                               "dashed brown: +/- 2 hPa [diff]"), cex=0.75)

    AddFooter ()
    if (!is.na(Seq) && (Seq == 1)) {return()}
  }
  
  
# Add TAS plot
  TAS <- VRPlot[[7]]
  TAS <- TAS[which("TAS" == substr(TAS, 1, 3))]
  plotWAC (data[, c("Time", TAS)], 
           col=c('blue', 'darkorange', 'darkgreen', 'cyan'), ylab='TASy [m/s]', 
           legend.position='bottom')

  labl <- TAS
  #labl <- sub("TAS", "", labl)
  titl <- "Mean differences: "
  for (i in 2:length(labl)) {
    titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[1],
                    mean(data[, TAS[i]] - data[, TAS[1]], na.rm=TRUE))
  }
  title(main = paste("Airspeed",'\n',titl))
  
  # Difference plot
  TAS <- VRPlot[[7]]
  if ('TASFR' %in% TAS && 'TAS_A' %in% TAS) {
    plotWAC(data$Time, (data$TASFR-data[, "TAS_A"]), ylab='Difference [m/s]',            
            ylim=c(-4,4))
    # These next lines set grid to yrange spanning -4 to 4.
    for (ny in seq(-4,4,by=1)){
      abline(h=ny, lwd=1, lty=3, col='gray')
    }
    abline (h=-1, lwd=1.5, lty=2); abline (h=1, lwd=1.5, lty=2)
    title('TASFR minus TAS-A')
  }
  
# Add MACH plot
  MACH <- VRPlot[[7]]
  MACH <- MACH[which("MACH" == substr(MACH, 1, 4))]
  plotWAC (data[, c("Time", MACH)], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='MACHy', 
           legend.position='bottom')
  title('MACH number')
  AddFooter ()
}
