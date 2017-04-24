### plot 13: IRU continued, ACINS, VSPD
RPlot13 <- function (data, ...) {
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
  ACINS <- VRPlot[[13]]
  ACINS <- ACINS[which("ACINS" == substr(ACINS, 1, 5))]
  DF <- data[, c("Time", ACINS)]
  plotWAC (DF, ylab="ACINS")
  title (sprintf ("mean vertical acceleration: %.3f", mean (data[, VRPlot[[13]][1]], na.rm=TRUE)))
  VSPD <- VRPlot[[13]]
  VSPD <- VSPD[grep("VSPD", VSPD)]
  plotWAC (data[, c("Time", VSPD)], legend.position='topright')
  title (sprintf ("mean vertical velocity: %.3f (IRS) and %.3f (GPS)",
                  mean (data$VSPD, na.rm=TRUE), mean (data$VSPD_A, na.rm=TRUE)))
  op <- par (mar=c(5,4,1,1)+0.1)
  ALT <- VRPlot[[13]]
  ALT <- ALT[grep("ALT", ALT)]
  plotWAC (data[, c("Time", ALT)],
           legend.position = "top")
  AddFooter ()
}

