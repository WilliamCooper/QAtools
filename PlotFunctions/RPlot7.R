
### plot 7: dynamic pressure; also TAS and MACH
RPlot7 <- function (data, Seq=NA) { 
  op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  if (is.na (Seq) || Seq == 1) {
    QC <- VRPlot[[7]][grepl ('^QC', VRPlot[[7]])]
    ## use uncorrected QCs only:
    QC <- QC[!grepl ('QC_A',QC) & !grepl ('C$', QC)]
    plotWAC (data[, c("Time", QC)], ylab='QCy [hPa]', 
             legend.position='top', ylim=c(0,200))
    labl <- QC
    labl <- sub("QC", "", labl)
    titl <- "Mean diff: "
    for (i in 2:length(labl)) {
      titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[1],
                      mean(data[, QC[i]] -
                             data[, QC[1]], na.rm=TRUE))
    }
    title(titl, cex.main=0.8)
    op <- par (mar=c(5,4,1,2)+0.1)
    QC <- VRPlot[[7]][grepl ('^QC', VRPlot[[7]])]
    ## corrected QCs only
    QC <- QC[grepl ('QC_A', QC) | grepl ('C$', QC)]
    ir <- which ('QCXC' == QC)
    if (length (ir) != 1) {
      ir <- which ('QCFC' == QC)
    }
    if (length(ir) != 1) {ir <- 1}
    plotWAC (data[, c("Time", QC)], ylab=' corrected QCyC [hPa]',
             legend.position='top', ylim=c(0,200))
    if ('QC_A' %in% QC) {
      points (data$Time, (data$QC_A-data[, QC[ir]])*10+120, type='l', col='brown')
    }
    axis (4, at=c(100,120,140), labels=c("-2", "0", "2"), col='brown', col.axis='brown', cex.axis=0.7)
    abline (h=100, col='brown', lty=2); abline (h=140, col='brown', lty=2)
    ltext <- sprintf("brown: (%s-%s)*10+120", 'QC_A', QC[ir])
    legend("topleft", legend=c(ltext, 
                                  "dashed brown: +/- 2 hPa [diff]"), cex=0.75)
    labl <- QC
    labl <- sub("QC", "", labl)
    titl <- "Mean diff: "
    for (i in 1:length(labl)) {
      if (i == ir) {next}
      titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[ir],
                      mean(data[, QC[i]] - data[, QC[ir]], na.rm=TRUE))
    }
    title(titl, cex.main=0.8)
    AddFooter ()
    if (!is.na(Seq) && (Seq == 1)) {return()}
  }
  # add TAS and MACH plots:
  op <- par (mar=c(2,4,1,1)+0.1)
  TAS <- VRPlot[[7]]
  TAS <- TAS[which("TAS" == substr(TAS, 1, 3))]
  plotWAC (data[, c("Time", TAS)], 
           col=c('blue', 'darkorange', 'darkgreen', 'cyan'), ylab='TASy [m/s]', 
           legend.position='bottom')
  points(data$Time, (data[, TAS[length(TAS)]] - data[, TAS[1]]) *20+200, type='l',
         col='red')  
  ltext <- sprintf("red: (%s-%s)*20+200", TAS[length(TAS)], TAS[1])
  legend("bottomleft", c(ltext, "dashed red: +/- 1 m/s [diff]"), cex=0.75)
  hline(220, 'red'); hline(180, 'red')
  labl <- TAS
  labl <- sub("TAS", "", labl)
  titl <- "Mean diff: "
  for (i in 2:length(labl)) {
    titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[1],
                    mean(data[, TAS[i]] - data[, TAS[1]], na.rm=TRUE))
  }
  title(titl, cex.main=0.8)
  op <- par (mar=c(5,4,1,1)+0.1)
  MACH <- VRPlot[[7]]
  MACH <- MACH[which("MACH" == substr(MACH, 1, 4))]
  plotWAC (data[, c("Time", MACH)], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='MACHy', 
           legend.position='bottom')
  AddFooter ()
}
