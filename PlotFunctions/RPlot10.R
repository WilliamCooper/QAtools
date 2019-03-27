### plot 10: Schuler oscillation
RPlot10 <- function (data, Flight=NA, Seq=NA, panl=1) {
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  ## EW SCHULER OSCILLATION - plot 15a
  panel11 <- function(data) {
    DF <- data[, c("Time", "GGVEW", "VEW")]
    DF$DifferenceX50 <- (data$GGVEW-data$VEW)*50
    line.colors=c('blue', 'darkorange', 'red', 'skyblue')
    line.widths <- c(1,1,1)
    line.types <- c(1, 9, 1, 2)
    plotWAC(DF, col=line.colors, lwd=line.widths, lty=line.types, 
      ylim = YLMF (1, range (as.matrix (DF[, c('GGVEW', 'VEW', 'DifferenceX50')]),
        finite=TRUE)))
    axis (4, at=c(-100,-50,0,50,100), labels=c("-2", "-1", "0", "1", "2"), 
      col='red', col.axis='red')
    hline (50, 'red'); hline (-50, 'red')
    legend ("bottomleft", legend="dashed-red: +/- 1 m/s, Difference", 
      box.col='red', text.col='red', cex=0.5)
  }
  
  ## NS SCHULER OSCILLATION - plot 15b
  panel12 <- function(data) {
    line.colors=c('blue', 'darkorange', 'red', 'skyblue')
    line.widths <- c(1,1,1)
    line.types <- c(1, 9, 1, 2)
    DF <- data[, c("Time", "GGVNS", "VNS")]
    DF$DifferenceX50 <- (data$GGVNS-data$VNS)*50
    plotWAC(DF, col=line.colors, lwd=line.widths, lty=line.types,
      ylim = YLMF (2, range (as.matrix (DF[, c('GGVNS', 'VNS', 'DifferenceX50')]),
        finite=TRUE)))
    axis (4, at=c(-100,-50,0,50,100), labels=c("-2", "-1", "0", "1", "2"), 
      col='red', col.axis='red')
    hline (50, 'red'); hline (-50, 'red')
    legend ("bottomleft", legend="dashed-red: +/- 1 m/s, Difference", 
      box.col='red', text.col='red', cex=0.5)
  }
  
  ## GGQUAL - plot 15c
  panel13 <- function(data) {
    DF <- data[, c("Time", "GGQUAL")]
    plotWAC(DF, ylim=c(0,10))
  }
  
  ## COMPL FILTER TEST, EW COMPONENT: plot 16a
  panel21 <- function(data) {
    DF <- data.frame(Time=data$Time)
    DF$DVEW <- data$VEWC - data$VEW
    DF$DVEWG <- data$VEWC - data$GGVEW
    plotWAC(DF[, c("Time", "DVEW", "DVEWG")], 
      ylim = YLMF (1, range (as.matrix(DF[, names(DF)[-which ('Time' == names(DF))]]),
        finite=TRUE)))
  }
  
  ## COMPL FILTER TEST, NS COMPONENT: plot 16b  
  panel22 <- function(data) {
    DF <- data.frame(Time=data$Time)
    DF$DVNS <- data$VNSC - data$VNS
    DF$DVNSG <- data$VNSC - data$GGVNS
    plotWAC(DF[, c("Time", "DVNS", "DVNSG")], 
      ylim = YLMF (2, range (as.matrix(DF[, names(DF)[-which ('Time' == names(DF))]]),
        finite=TRUE)))
  }
  
  
  ######################################################
  if (shinyDisplay) {
    nseq <- (Seq-1) * 3 + panl
    switch(nseq, 
      { # nseq == 1
        setMargins (2)
        panel11(data)
      }, 
      { # 2
        setMargins (2)
        panel12(data)
      }, 
      { # 3
        if ('GGQUAL' %in% names(data)) {
          setMargins (3)
          panel13(data)
        }
        AddFooter()
      }, 
      { # 4
        setMargins (2)
        panel21(data)
      }, 
      { # 5
        setMargins (3)
        panel22(data)
        AddFooter()
      }
    )
    ######################################################
  } else {
    ## needs GGVEW, GGVNS, VEW, VNS, GGQUAL
    layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,3))
    setMargins (4)
    panel11 (data)
    panel12 (data)
    if ('GGQUAL' %in% names (data)) {
      setMargins (5)
      panel13 (data)
    }
    AddFooter ()
    if (!is.na(Seq) && (Seq == 1)) {return()}
    layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
    setMargins (4)
    panel21 (data)
    setMargins (5)
    panel22 (data)
    AddFooter()
  }
}
