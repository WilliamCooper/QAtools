### plot 2: construct one-per-hour track plots
RPlot2 <- function (data, ...) { 
  ## add outer margin footer as for other plots?
  op <- par (oma=c(1.1,0,0,0))
  SE <- getStartEnd(data$Time)
  for (hr in 0:32) {
    if (hr*10000 < SE[2] && (hr+1)*10000 > SE[1]) {
      Start <- ifelse ((hr*10000 < SE[1]), SE[1], hr*10000)
      End <- ifelse (((hr+1)*10000 > SE[2]), SE[2], (hr+1)*10000)
      plotTrack (data, .Range=setRange(data$Time, Start, End), 
                 .Spacing=15, .WindFlags=10)
    }
  }
  AddFooter ()
}

