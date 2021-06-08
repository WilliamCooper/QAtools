### plot 18: plot skew-T for individual climbs and descents:
RPlot18 <- function (data, ...) {
## need to add footer if/when reimplemented
  ## needs PSXC, ATX, DPXC, GGALT
  # search for soundings: 3 min climb/descent > tol
  del <- 180
  tol <- 3.5
  DataT <- data[!is.na(data$Time) & !is.na(data$GGALT), ]
  r <- 1:length(DataT$GGALT)
  for (i in (del+1):(length(DataT$GGALT)-del)) {
    if (!is.na(DataT$GGALT[i]) && abs (DataT$GGALT[i+del]-DataT$GGALT[i]) < tol*del) {
      r[i] <- NA 
    }
    if (abs (DataT$GGALT[i-del]-DataT$GGALT[i]) > tol*del) {
      r[i] <- i
    }
  }
  r[1:del] <- NA
  L <- length(DataT$GGALT)
  r[(L-del):L] <- NA
  s <- r[!is.na(r)]
  # look for breaks of at least del:
  #print (sprintf ("start at 1 %d", s[1]))
  startSounding <- 1
  startAlt <- DataT$GGALT[s[1]]
  lastAltChange <- 0
  for (j in 1:(length(s)-1)) {
    if (s[j+1]-s[j] > del) {
      #print (sprintf ("break at %d %d", j, s[j]))
      #print (sprintf ("alt change: %.1f", DataT$GGALT[s[j]]-startAlt))
      AltChange <- DataT$GGALT[s[j]] - startAlt
      if (AltChange * lastAltChange < 0) {
        DF <- DataT[s[startSounding:endSounding], c("PSXC", "ATX", "DPXC")]
        colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
        print(SkewTSounding (DF, BackgroundSpecs="skewTDiagramC130.Rdata")
              +ggtitle(sprintf("Flight %s, Times %s--%s", Flight,
                               formatTime(DataT$Time[s[startSounding]]),
                               formatTime(DataT$Time[s[endSounding]]))))
        startSounding <- j+1
      }
      startAlt <- DataT$GGALT[s[j+1]]
      lastAltChange <- AltChange
      endSounding <- j
    }
  }
  L <- length (s)
  #print (sprintf ("end at %d %d", L, s[L]))
  #print (sprintf (" alt change: %.1f", DataT$GGALT[L]-startAlt))
  DF <- DataT[s[startSounding:endSounding], c("PSXC", "ATX", "DPXC")]
  colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
  print(SkewTSounding (DF, BackgroundSpecs="skewTDiagramC130.Rdata")
        +ggtitle(sprintf("Flight %s, Times %s--%s", Flight,
                         formatTime(DataT$Time[s[startSounding]]),
                         formatTime(DataT$Time[s[endSounding]]))))
  startSounding <- endSounding + 1
  endSounding <- length(s)
  DF <- DataT[s[startSounding:endSounding], c("PSXC", "ATX", "DPXC")]
  colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
  print(SkewTSounding (DF, BackgroundSpecs="skewTDiagramC130.Rdata")
        +ggtitle(sprintf("Flight %s, Times %s--%s", Flight,
                         formatTime(DataT$Time[s[startSounding]]),
                         formatTime(DataT$Time[s[endSounding]]))))
  # this is just to illustrate how the sounding-finder works; eventually suppress
  # op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  # plotWAC (DF <- DataT[, c("Time", "GGALT")])
  # lines (DF[r, ], lwd=2, col='red')
  # title ("red: segments selected for Skew-T soundings")
}

