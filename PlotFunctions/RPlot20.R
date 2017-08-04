### plot 20: CDP/SP100 size distributions
RPlot20 <- function (data, Seq=NA) {
  if (is.na (VRPlot$PV20) || (length(VRPlot$PV20) < 1)) {return ()}
  ## needs CCDP_LWOI; references fname from calling environment
  print ('entry to RPlot20: names in dat are')
  print (sort(names(data)))
  # Z <- getAttributes(data$CCDP_RWII)
  # print (Z)
  kount = 0
  nms <- names(data)
  nm1 <- nm2 <- character(0)
  if (length (grep ("CCDP_", VRPlot[[20]])) > 0) {
    nm1 <- nms[grep ('CCDP_', nms)]
    CellLimitsD <- attr(data[,nm1[1]], 'CellSizes')
    print (c('CellLimitsD', CellLimitsD))
  }
  if (length (grep ("CS100_", VRPlot[[20]])) > 0) {
    nm2 <- nms[grep ('CS100_', nms)]
    CellLimitsF <- attr(data[,nm2[1]], 'CellSizes')
  }
  
  
  layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(5,5,6))
  idx1 <- getIndex (data$Time, StartTime)
  if (idx1 < 1) {idx1 <- 1}
  ## reference to calling environment for StartTime
  jstart <- ifelse (StartTime > 0, idx1, 1)
  print (sprintf ("start time in RPlot20 is %d and jstart is %d\n",
    StartTime, jstart))
  op <- par (mar=c(2,2,1,1)+0.1,oma=c(1.1,0,0,0))
  if (length(nm1) > 0) {
    nm <- nm1[1]
    for (j in jstart:length(data$Time)) {
      if (is.na(data$Time[j])) {next}
      if (is.na(data$TASX[j]) || data$TASX[j] < 60) {next}
      if (any(is.na(data[j, nm]))) {next}
      if (all(data[j, nm] < 1)) {next}
      ## convert distributions to number per cm per um
      CDPtot <- 0
      for (m in 2:length(data[j, nm])) {
        CDPtot <- CDPtot + data[j, nm][m]
        # print (sprintf ('R20: m/j=%d %d', m, j))
        # print (data[j, nm][m])
        # print (sprintf ('CellLimits[m,m-1]=%f %f', CellLimitsD[m], CellLimitsD[m-1]))
        data[j, nm][m] <- data[j, nm][m] / (CellLimitsD[m] - CellLimitsD[m-1])
      }
      data[j, nm][data[j, nm] <= 0] <- 1e-4
      if ((any(data[j, nm] > 1, na.rm=TRUE))) {
        kount <- kount + 1
        if (is.na (Seq) || (!is.na(Seq) && (kount > (Seq-1)*6))) {
          ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
            op <- par (mar=c(5.2,2,1,1)+0.1))
          plot (CellLimitsD, data[j, nm], type='s', ylim=c(1.e-1,1.e3), 
            xlab="Diameter [um]", log="y", col='blue', lwd=2)
          if (length(nm1) > 1) {
            lines (CellLimitsD, data[j, nm1[2]], type='s', col='blue', lty=2)
          }
          title(sprintf("Time=%s CONCD=%.1f", strftime (data$Time[j], format="%H:%M:%S", tz='UTC'), CDPtot), 
            cex.main=1)
          legend ("topright", legend=c("CDP"), col='blue', 
            lwd=c(2,1), cex=0.75) 
          if (kount%%6==0)   AddFooter ()
        }
      }
    }
    if (!is.na(Seq) && (kount >= (Seq*6))) {break}
    if (kount >= 24) {break}
  }
  if (kount == 0) {
    print ('no qualifying seconds found for CDP plot')
    plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
    text (0.5, 0.8, labels='no CDP/SP100 measurements')
  }
}

