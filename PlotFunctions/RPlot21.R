### plot 21: UHSAS and SP200 (PCASP) size distributions
RPlot21 <- function (data, Seq=NA) {
  ## needs CUHSAS_xxx, SP200_xxx; references fname from calling environment
  if (is.na (VRPlot$PV21) || (length(VRPlot$PV21) < 1)) {return ()}
  kount = 0
  plotTest <- 50
  nms <- names(data)
  nm1 <- nm2 <- character(0)
  if (length (grep ("CUHSAS_", VRPlot[[21]])) > 0) {
    nm1 <- nms[grep ('CUHSAS_', nms)]
    CellLimitsU <- attr(data[,nm1[1]], 'CellSizes')
    print (c('CellLimitsU', CellLimitsU))
  }
  if (length (grep ("CS200_", VRPlot[[21]])) > 0) {
    nm2 <- nms[grep ('CS200_', nms)]
    CellLimitsP <- attr(data[,nm2[1]], 'CellSizes')
  }
  
  
  layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(5,5,6))
  op <- par (mar=c(2,2,1,1)+0.1,oma=c(1.1,0,0,0))
  idx1 <- getIndex (data$Time, StartTime)
  if (idx1 < 1) {idx1 <- 1}
  ## reference to calling environment for StartTime
  jstart <- ifelse (StartTime > 0, idx1, 1)
  # print (sprintf ("start time in RPlot21 is %d and jstart is %d\n",
  #                 StartTime, jstart))
  CV <- nms[grep ('CONCU_', nms)][1]
  iw <- which(data[, CV] > plotTest)  ## get list of indices where CONCU > PlotTest
  if (length(nm1) > 0) {
    nm <- nm1[1]
    # for (j in jstart:length(Time)) {
    for (j in iw) {
      if (is.na(data$Time[j])) {next}
      if (!is.na(data$TASX[j]) && (data$TASX[j] < 60)) {next}
      CUHSAStot <- sum (data[j, nm], na.rm=TRUE)
      ## convert distributions to number per cm per um
      data[j, nm] <- data[j, nm] / diff(CellLimitsU)
      data[j, nm][data[j, nm] <= 0] <- 1e-4
      if (length (nm1) > 1) {
        CUHSAS2tot <- sum (data[j, nm1[2]], na.rm=TRUE)
        data[j, nm1[2]] <- data[j, nm1[2]] / diff(CellLimitsU)
        data[j, nm1[2]][data[j, nm1[2]] <= 0] <- 1e-4
      }
      if (length(nm2) > 0) {
        CPCASP <- sum(data[j, nm2[1]], na.rm=TRUE)
        data[j, nm2[1]] <- data[j, nm2[1]] / diff(CellLimitsP)
        data[j, nm2[1]][data[j, nm2[1]] <= 0] <- 1e-4
      }
      if ((any(data[j, nm] > 1, na.rm=TRUE))) {
        kount <- kount + 1
        if (is.na (Seq) || (kount > (Seq-1)*6)) {
          ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
            op <- par (mar=c(5.2,2,1,1)+0.1))
          plot (CellLimitsU, c(1.e-4, data[j, nm]), type='S', ylim=c(1,1.e6),
            xlab="Diameter [um]", log="y", col='blue', lwd=2)
        } 
        if (length(nm2) > 0) {
          lines (CellLimitsP, c(1.e-4, data[j, nm2[1]]), type='S', col='magenta', lty=3)
        }
        if (length(nm1) > 1) {
          lines (CellLimitsU, c(1.e-4,data[j, nm1[2]]), type='S', col='forestgreen', lty=2)
          legend('bottomright', legend=nm1, text.col=c('blue', 'forestgreen'), 
            col=c('blue', 'forestgreen'), lty=c(1,2), lwd=c(2,1))
        }
        title(sprintf("size distribution, Time=%s", strftime (data$Time[j], format="%H:%M:%S", tz='UTC')), 
          cex.main=.75)
        if (length (nm2) > 0) {
          legend ("topright", legend=c("UHSAS", "PCASP"), col=c('blue', 'magenta'), 
            lwd=c(2,1), cex=0.75) 
        } else {
          legend ('topright', legend='UHSAS', col='blue', text.col='blue')
        }
        if (kount%%6==0)   AddFooter ()
      }
      if (!is.na(Seq) && (kount >= (Seq*6))) {break}
      if (kount >= 24) {break}
    }
  }
}

