### plot 22: 2DC size distributions
RPlot22 <- function (data, Seq=NA, panl=1) {
  ## needs C1DC_LWOI; references fname from calling environment
  if (is.na (VRPlot$PV22) || (length(VRPlot$PV22) < 1)) {
    plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
    text (0.5, 0.8, 'no 2DC measurements')
    return ()
  }
  kount = 0
  nms <- names(data)
  nm1 <- nm2 <- character(0)
  if (length (grep ("C1DC_", VRPlot[[22]])) > 0) {
    nm1 <- nms[grep ('^C1DC_', nms)]
    CellLimits <- attr(data[,nm1[1]], 'CellSizes')
#   print (c('CellLimits', CellLimits))
  }
  
  layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(5,5,6))
  idx1 <- getIndex (data$Time, StartTime)
  if (idx1 < 1) {idx1 <- 1}
  ## reference to calling environment for StartTime
  jstart <- ifelse (StartTime > 0, idx1, 1)
  # print (sprintf ("start time in RPlot23 is %d and jstart is %d\n",
  #                 StartTime, jstart))
  op <- par (mar=c(2,2,1,1)+0.1,oma=c(1.1,0,0,0))
  CV <- nms[grep ('CONC1DC_', nms)][1]
  iw <- which(data[, CV] > 0.1)  ## get list of indices where CONC1DC > 0.1
  if (length(nm1) > 0) {
    nm <- nm1[1]
    # for (j in jstart:length(Time)) {
    for (j in iw) {
      if (is.na(data$Time[j])) {next}
      if (!is.na(data$TASX[j]) && (data$TASX[j] < 60)) {next}
      ## convert distributions to number per L per um
      C1DCtot <- sum (data[j, nm], na.rm=TRUE)
      data[j, nm] <- data[j, nm] / diff(CellLimits)
      data[j, nm][data[j, nm] <= 0] <- 1e-4
      if (length (nm1) > 1) {
        C1DC2tot <- sum (data[j, nm1[2]], na.rm=TRUE)
        data[j, nm1[2]] <- data[j, nm1[2]] / diff(CellLimits)
        data[j, nm1[2]][data[j, nm1[2]] <= 0] <- 1e-4
      }
      
      if ((any(data[j, nm] > 0.1, na.rm=TRUE))) {
        kount <- kount + 1
        if (is.na (Seq) || (!is.na(Seq) && (kount > (Seq-1)*6))) {
          ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
            op <- par (mar=c(5.2,2,1,1)+0.1))
          plot (CellLimits, c(1.e-4, data[j, nm]), type='S', ylim=c(1.e-2,1.e2), 
            xlab="Diameter [um]", log="y", col='blue', lwd=2)
          title(sprintf("Time=%s", strftime (data$Time[j], format="%H:%M:%S", tz='UTC')), 
            cex.main=.75)
          legend ("topright", legend=c("1DC"), col='blue', 
            lwd=c(2,1), cex=0.75) 
          if (length (nm1) > 1) {
            lines (CellLimits, c(1.e-4, data[j, nm1[2]]), type='S', col='forestgreen', lty=2)
            legend('bottomright', legend=nm1, text.col=c('blue', 'forestgreen'), 
              col=c('blue', 'forestgreen'), lty=c(1,2), lwd=c(2,1))
          }
          if (kount%%6==0)   AddFooter ()
        }
      }
      if (!is.na(Seq) && (kount >= (Seq*6))) {break}
      if (kount >= 24) {break}
    }
  }
}
  
  
