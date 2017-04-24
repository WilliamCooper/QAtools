### plot 10: Schuler oscillation
RPlot10 <- function (data, Flight=NA, Seq=NA) {
  ## needs GGVEW, GGVNS, VEW, VNS, GGQUAL
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,3))
  op <- par (mar=c(2,4,1,2)+0.1,oma=c(1.1,0,0,0))
  DF <- data[, c("Time", "GGVEW", "VEW")]
  DF$DifferenceX50 <- (data$GGVEW-data$VEW)*50
  line.colors=c('blue', 'darkorange', 'red', 'skyblue')
  line.widths <- c(1,1,1)
  line.types <- c(1, 9, 1, 2)
  plotWAC(DF, col=line.colors, lwd=line.widths, lty=line.types)
  axis (4, at=c(-100,-50,0,50,100), labels=c("-2", "-1", "0", "1", "2"), 
        col='red', col.axis='red')
  hline (50, 'red'); hline (-50, 'red')
  legend ("bottomleft", legend="dashed-red: +/- 1 m/s, Difference", 
          box.col='red', text.col='red', cex=0.5)
  DF <- data[, c("Time", "GGVNS", "VNS")]
  DF$DifferenceX50 <- (data$GGVNS-data$VNS)*50
  plotWAC(DF, col=line.colors, lwd=line.widths, lty=line.types)
  axis (4, at=c(-100,-50,0,50,100), labels=c("-2", "-1", "0", "1", "2"), 
        col='red', col.axis='red')
  hline (50, 'red'); hline (-50, 'red')
  legend ("bottomleft", legend="dashed-red: +/- 1 m/s, Difference", 
          box.col='red', text.col='red', cex=0.5)
  op <- par (mar=c(5,4,1,2)+0.1)
  DF <- data[, c("Time", "GGQUAL")]
  plotWAC(DF, ylim=c(0,10))
  AddFooter ()
  if (!is.na(Seq) && (Seq == 1)) {return()}
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  op <- par (mar=c(2,4,1,2)+0.1,oma=c(1.1,0,0,0))
  DF <- data.frame(Time=data$Time)
  DF$DVEW <- data$VEWC - data$VEW
  DF$DVNS <- data$VNSC - data$VNS
  DF$DVEWG <- data$VEWC - data$GGVEW
  DF$DVNSG <- data$VNSC - data$GGVNS
  plotWAC(DF[, c("Time", "DVEW", "DVEWG")])
  op <- par (mar=c(5,4,1,2)+0.1)
  plotWAC(DF[, c("Time", "DVNS", "DVNSG")])
  AddFooter()
}

