### Plot 12: IRU comparisons
RPlot12 <- function (data, ...) {
  ## variables can include PITCH, ROLL, THDG from IRUs
  ## apply project-dependent offsets:
#   pitch_offset <- 0.37
#   roll_offset <- -0.26
#   thdg_offset <- -0.35
  # pitch_offset <- roll_offset <- thdg_offset <- 0
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,2)+0.1,oma=c(1.1,0,0,0))
  PITCH <- VRPlot[[12]]
  PITCH <- PITCH[which("PITCH" == substr(PITCH, 1, 5))]
  DF <- data[, c("Time", PITCH)]
  if ("PITCH_IRS2" %in% names(data)) {
    DF$PITCH_IRS2 <- DF$PITCH_IRS2 - pitch_offset
    DF$DifferenceX50 <- (DF$PITCH - DF$PITCH_IRS2) * 50
  }
  if ("PITCH_IRS3" %in% names(data)) {
    DF$PITCH_IRS3 <- DF$PITCH_IRS3 - pitch_offset
    DF$DifferenceX50 <- (DF$PITCH - DF$PITCH_IRS3) * 50
  }
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylim=c(-10.,10.), ylab="PITCH [deg.]",
           col=line.colors, lty=line.types)
  axis (4, at=c(-2.5,0,2.5), labels=c("-0.05", "0", "0.05"), col='red', col.axis='red')
  hline (-2.5, 'red'); hline (2.5, 'red')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference",
         box.col='red', text.col='red', cex=0.5)
  title( sprintf ("mean difference: %.2f +/- %.2f after offset %.2f", 
                  mean (DF$DifferenceX50/50, na.rm=TRUE),
                  sd   (DF$DifferenceX50/50, na.rm=TRUE), pitch_offset))
  ROLL <- VRPlot[[12]]
  ROLL <- ROLL[which("ROLL" == substr(ROLL, 1, 4))]
  DF <- data[, c("Time", ROLL)]
  if ("ROLL_IRS2" %in% names (data)) {
    DF$ROLL_IRS2 <- DF$ROLL_IRS2 - roll_offset
    DF$DifferenceX50 <- (DF$ROLL - DF$ROLL_IRS2) * 50
  }
  if ("ROLL_IRS3" %in% names (data)) {
    DF$ROLL_IRS3 <- DF$ROLL_IRS3 - roll_offset
    DF$DifferenceX50 <- (DF$ROLL - DF$ROLL_IRS3) * 50
  }
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylab="ROLL [deg.]",
           col=line.colors, lty=line.types)
  axis (4, at=c(-2.5,0,2.5), labels=c(NA, "+/-0.05", NA), col='red', col.axis='red')
  hline (-2.5, 'red'); hline (2.5, 'red')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference",
         box.col='red', text.col='red', cex=0.5)
  title( sprintf ("mean difference: %.2f sd %.2f after offset %.2f", 
                  mean (DF$DifferenceX50/50, na.rm=TRUE),
                  sd   (DF$DifferenceX50/50, na.rm=TRUE), roll_offset))
  op <- par (mar=c(5,4,1,2)+0.1)
  THDG <- VRPlot[[12]]
  THDG <- THDG[which("THDG" == substr(THDG, 1, 4))]
  DF <- data[, c("Time", THDG)]
  if ("THDG_IRS2" %in% names(data)) {
    DF$THDG_IRS2 <- DF$THDG_IRS2 - thdg_offset
    t <- !is.na(DF$THDG_IRS2) & !is.na(DF$THDG) & (DF$THDG - DF$THDG_IRS2 > 180)
    DF$THDG_IRS2[t] <- DF$THDG_IRS2[t] + 360
    t <- !is.na(DF$THDG_IRS2) & !is.na(DF$THDG) & (DF$THDG - DF$THDG_IRS2 < -180)
    DF$THDG_IRS2[t] <- DF$THDG_IRS2[t] - 360
    DF$DifferenceX500 <- (DF$THDG - DF$THDG_IRS2) * 500 + 180
  }
  if ("THDG_IRS3" %in% names(data)) {
    DF$THDG_IRS3 <- DF$THDG_IRS3 - thdg_offset
    t <- !is.na(DF$THDG_IRS3) & !is.na(DF$THDG) & (DF$THDG - DF$THDG_IRS3 > 180)
    DF$THDG_IRS3[t] <- DF$THDG_IRS3[t] + 360
    t <- !is.na(DF$THDG_IRS3) & !is.na(DF$THDG) & (DF$THDG - DF$THDG_IRS3 < -180)
    DF$THDG_IRS3[t] <- DF$THDG_IRS3[t] - 360
    DF$DifferenceX500 <- (DF$THDG - DF$THDG_IRS3) * 500 + 180
  }
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.types <- c(1, 9, 1, 2)
  plotWAC (DF, ylim=c(-60,390), ylab="THDG [deg.]",
           col=line.colors, lty=line.types)
  axis (4, at=c(180-25, 180, 180+25), labels=c(NA, "+/-0.05", NA), col='red', col.axis='red')
  hline (180-25, 'red'); hline (180+25, 'red')
  #hline (180-125, 'green', lwd=2)
  hline (90, 'lightblue'); hline (180, 'lightblue')
  hline (270, 'lightblue'); hline (360, 'lightblue'); hline (0, 'lightblue')
  legend("bottomleft", legend="dashed lines: +/- 0.05 deg Difference, wrt 180 deg",
         box.col='red', text.col='red', cex=0.5)
  title( sprintf ("mean difference, THDG-THDG_IRS2: %.2f sd: %.2f after offset %.2f (but beware wrap-around)", 
         mean ((DF$DifferenceX500-180)/500, na.rm=TRUE),
         sd   ((DF$DifferenceX500-180)/500, na.rm=TRUE), thdg_offset))
  AddFooter ()
}

