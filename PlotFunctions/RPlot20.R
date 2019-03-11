### plot 20: CDP/CS100 (FSSP) size distributions
RPlot20 <- function (data, Seq=NA) {
  print('entry to RPlot20')
  print(VRPlot$PV20)
  if (is.na (VRPlot$PV20) || (length(VRPlot$PV20) < 1)) {
    plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
    text (0.5, 0.8, 'no cloud-droplet measurements')
    return ()}
  ## needs CCDP_xxx and SP100_xxx; references fname from calling environment
  # ptm <- proc.time()
  # tic('RPlot20')
#  print (sprintf('entry to RPlot20, Seq=%d: names in data are', Seq))
  #print (sort(names(data)))
  #print (str(data))
  kount = 0
  nms <- names(data)
  nm1 <- nm2 <- character(0)
  if (length (grep ("CCDP_", VRPlot[[20]])) > 0) {
    nm1 <- nms[grep ('CCDP_', nms)]
    CellLimitsD <- attr(data[,nm1[1]], 'CellSizes')
    #print (sprintf ('CCDP sizes in %s', nm1[1]))
    #print (CellLimitsD)
  }
  if (length (grep ("CS100_", VRPlot[[20]])) > 0) {
    nm2 <- nms[grep ('CS100_', nms)]
    CellLimitsF <- attr(data[,nm2[1]], 'CellSizes')
  }
  if (length (nm1) < 1 && length (nm2) < 1) {
    plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
    text (0.5, 0.8, 'no cloud-droplet measurements')
    return ()
  }
  
  layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(5,5,6))
  idx1 <- getIndex (data$Time, StartTime)
  if (idx1 < 1) {idx1 <- 1}
  ## reference to calling environment for StartTime
  jstart <- ifelse (StartTime > 0, idx1, 1)
  #print (sprintf ("start time in RPlot20 is %d and jstart is %d",
    #StartTime, jstart))
  op <- par (mar=c(2,2,1,1)+0.1,oma=c(1.1,0,0,0))
  CV <- nms[grep ('CONCD_', nms)][1]
  iw <- which(data[, CV] > 1)  ## get list of indices where CONCD > 1
  if (length(nm1) > 0) {
    nm <- nm1[1]
      # for (j in jstart:length(data$Time)) {
    for (j in iw) {    ## (about x10 faster than the preceding statement)
      if (is.na(data$Time[j])) {next}
      if (is.na(data$TASX[j]) || data$TASX[j] < 60) {next}
      # if (any(is.na(data[j, nm]))) {next}
      # if (all(data[j, nm] < 1)) {next}
      ## convert distributions to number per cm^3 per um
      CDPtot <- sum (data[j, nm], na.rm=TRUE)
      data[j, nm] <- data[j, nm] / diff(CellLimitsD)
      data[j, nm][data[j, nm] <= 0] <- 1e-4
      if (length (nm1) > 1) {
        CDPi2tot <- sum (data[j, nm1[2]], na.rm=TRUE)
        data[j, nm1[2]] <- data[j, nm1[2]] / diff(CellLimitsD)
        data[j, nm1[2]][data[j, nm1[2]] <= 0] <- 1e-4
      }
      if (length(nm2) > 0) {
        CFSSP <- sum(data[j, nm2[1]], na.rm=TRUE)
        data[j, nm2[1]] <- data[j, nm2[1]] / diff(CellLimitsF)
        data[j, nm2[1]][data[j, nm2[1]] <= 0] <- 1e-4
      }
      if ((any(data[j, nm] > 1, na.rm=TRUE))) {
        kount <- kount + 1
        # print (sprintf ('kount is %d, j is %d', kount, j))
        if (is.na (Seq) || (kount > (Seq-1)*6)) {
          ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
            op <- par (mar=c(5.2,2,1,1)+0.1))
          # print (sprintf ('j=%d, nm=%s', j, nm))
          # print (c('CellLimitsD', CellLimitsD))
          # print (data[j, nm])
          ## Type 'S' draws line in vertical, then horizontal, so spans the bin correctly
          ## if x_i is the upper size limit of bin i and x_(i+1) that of bin i+1,
          ## because the line should be drawn at the height of bin i+1, moving
          ## at the level of the concentration in bin i+1. This works if there are
          ## 
          plot (CellLimitsD, c(1.e-4, data[j, nm]), type='S', ylim=c(1.e-1,1.e3), 
            xlab="Diameter [um]", log="y", col='blue', lwd=2)
          if (length (nm2) > 0) {
            lines (CellLimitsF, c(1.e-4, data[j, nm2[1]]), type='S', col='magenta', lty=3)
          }
          if (length(nm1) > 1) {
            lines (CellLimitsD, c(1.e-4,data[j, nm1[2]]), type='S', col='forestgreen', lty=2)
            legend('bottomright', legend=nm1, text.col=c('blue', 'forestgreen'), 
              col=c('blue', 'forestgreen'), lty=c(1,2), lwd=c(2,1))
          }
          title(sprintf("Time=%s CONCD=%.1f", strftime (data$Time[j], format="%H:%M:%S", tz='UTC'), CDPtot), 
            cex.main=1)
          legend ("topright", legend=c("CDP"), col='black', 
            lwd=c(2,1), cex=0.75) 
          if (kount%%6==0)   AddFooter ()
        }
      }
      if (!is.na(Seq) && (kount >= (Seq*6))) {break}
      if (kount >= 24) {break}
    }
  }
  if (!is.na(Seq) && kount == (Seq-1)*6) {
    print ('no qualifying seconds found for CDP plot')
    plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
    text (0.5, 0.8, labels='no CDP/SP100 measurements')
  }
  # print(proc.time() - ptm)
  # toc()
}

