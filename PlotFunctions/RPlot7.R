
### plot 7: dynamic pressure; also TAS and MACH
RPlot7 <- function (data, Seq=NA, panl=1) {
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  # ## UNCORRECTED QCs: plot10a
  # panel11 <- function(data) {
  #   QC <- VRPlot[[7]][grepl ('^QC', VRPlot[[7]])]
  #   ## use uncorrected QCs only:
  #   QC <- QC[!grepl ('QC_A',QC) & !grepl ('C$', QC)]
  #   ylm <- range (as.matrix (data[, QC]), finite = TRUE)
  #   plotWAC (data[, c("Time", QC)], ylab='QCy [hPa]', 
  #     legend.position='top', 
  #     ylim = YLMF (1, ylm))
  #   labl <- QC
  #   labl <- sub("QC", "", labl)
  #   titl <- "Mean diff, uncorrected QC: "
  #   for (i in 2:length(labl)) {
  #     titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i], labl[1],
  #       mean(data[, QC[i]] - data[, QC[1]], na.rm=TRUE))
  #   }
  #   title(titl, cex.main=0.8)
  # }
  
  ## CORRECTED QCs: plot 10b  -- changed this to be top panel parallel w RPlot6
  panel11 <- function(data) {
    QC <- VRPlot[[7]][grepl ('^QC', VRPlot[[7]])]
    ## corrected QCs only
    QC <- QC[grepl ('QC_A', QC) | grepl ('C$', QC)]
    ylm <- range (as.matrix (data[, QC]), finite = TRUE)
    # left in for old projects; QCXC now absent from VRPlot[[7]]
    ir <- which ('QCXC' == QC)  
    #    if (length (ir) != 1) {
    #      ir <- which ('QCFC' == QC)
    #    }
    if (length(ir) != 1) {ir <- 1}  ## use first in list as reference
    plotWAC (data[, c("Time", QC)], ylab=' corrected QCyC [hPa]',
      legend.position='top', 
      ylim = YLMF (1, ylm))
    # if ('QC_A' %in% QC) {
    #   points (data$Time, (data$QC_A-data[, QC[ir]])*10+120, type='l', col='brown')
    # }
    # axis (4, at=c(100,120,140), labels=c("-2", "0", "2"), col='brown', col.axis='brown', cex.axis=0.7)
    # abline (h=100, col='brown', lty=2); abline (h=140, col='brown', lty=2)
    # ltext <- sprintf("brown: (%s-%s)*10+120", 'QC_A', QC[ir])
    # legend("topleft", legend=c(ltext, 
    #   "dashed brown: +/- 2 hPa [diff]"), cex=0.75)
    labl <- QC
    labl <- sub("QC", "", labl)
    titl <- "Mean diff, corrected QC: "
    for (i in 1:length(labl)) {
      if (i == ir) {next}
      titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[ir],
        mean(data[, QC[i]] - data[, QC[ir]], na.rm=TRUE))
    }
    title(titl, cex.main = cexmain)
  }
  
  panel12 <- function (data) { # corrected pressures only
    # Plot the differences:
    ## corrected QCs only
    QC <- VRPlot[[7]][grepl ('^QC', VRPlot[[7]])]
    QC <- QC[grepl ('QC_A', QC) | grepl ('C$', QC)]
    QCD <- QC[-1]
    DF <- data[, c('Time', QCD)] # use first in list as reference
    DF[, QCD] <- DF[, QCD] - data[, QC[1]]
    plotWAC(DF, ylab=expression(paste (Delta,' [hPa]')), 
      lwd = c(2.5, 2, 1.5, 1),
      ylim = YLMF (2, c(-20, 20)))
    hline (-1); hline (1)
    title (sprintf ('differences vs %s', QC[1]), cex.main = cexmain)
  }
  
  panel13 <- function (data) {
    # uncorrected dynamic pressures
    Q <- VRPlot[[7]][grepl ('^QC', VRPlot[[7]])]
    QC <- Q[c (which (grepl('C$', Q)), which (grepl('_A', Q)))]
    Q <- Q[!(Q %in% QC)]
    DF <- data[, c('Time', Q)] # use QCXC as reference if present; else skip
    if ('QCXC' %in% names (data)) {
      DF[, Q] <- DF[, Q] - data$QCXC
      plotWAC(DF, ylab=expression(paste (Delta,' [hPa]')), 
        lwd = c(2.5, 2, 1.5, 1),
        ylim = YLMF (3, c(-10, 10)))
      hline (-1, col = 'darkorange'); hline (1, col = 'darkorange')
      title ('differences, uncorrected Q vs QCXC', cex.main = cexmain)
    }
  }
  
  
  ## TAS: plot 11a
  panel21 <- function(data) {
    TAS <- VRPlot[[7]]
    TAS <- TAS[which("TAS" == substr(TAS, 1, 3))]
    plotWAC (data[, c("Time", TAS)], ylab='TASy [m/s]', 
      legend.position='bottom', 
      ylim = YLMF (1, range (as.matrix(data[, TAS]), finite=TRUE)))
    tref <- mean(data[, TAS[1]], na.rm=TRUE)
    tref <- (tref %/% 50) * 50
    points(data$Time, (data[, TAS[length(TAS)-1]] - data[, TAS[1]]) *20+tref, type='l',
      col='red')  
    ltext <- sprintf("red: (%s-%s)*20+%.0f", TAS[length(TAS)-1], TAS[1], tref)
    legend("bottomleft", c(ltext, "dashed red: +/- 1 m/s [diff]"), cex=0.75)
    hline(tref+20, 'red'); hline(tref-20, 'red')
    labl <- TAS
    labl <- sub("TAS", "", labl)
    titl <- "Mean TAS diff: "
    for (i in 2:length(labl)) {
      titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[1],
        mean(data[, TAS[i]] - data[, TAS[1]], na.rm=TRUE))
    }
    title(titl, cex.main = cexmain)
  }
  
  panel22 <- function (data) { # differences, TAS
    TAS <- VRPlot[[7]]
    TAS <- TAS[which("TAS" == substr(TAS, 1, 3))]
    TASD <- TAS[-1]
    DF <- data[, c('Time', TASD)] # use first in list as reference
    DF[, TASD] <- DF[, TASD] - data[, TAS[1]]
    plotWAC(DF, ylab=expression(paste (Delta,' [m/s]')), 
      lwd = c(2.5, 2, 1.5, 1),
      ylim = YLMF (2, c(-2, 2)))
    hline (-1); hline (1)
    title (sprintf ('differences vs %s', TAS[1]), cex.main = cexmain)
  }
  
  ## MACH: plot 11c
  panel23 <- function (data) {
    MACH <- VRPlot[[7]]
    MACH <- MACH[which("MACH" == substr(MACH, 1, 4))]
    plotWAC (data[, c("Time", MACH)], ylab='MACHy', 
      legend.position='bottom', 
      ylim = YLMF (3, range (as.matrix (data[, MACH]), finite=TRUE)))
  }
  
  if (shinyDisplay) { 
    nseq <- (Seq-1) * 2 + panl
    if (Seq > 1) {nseq <- nseq + 1}
    switch(nseq, 
      {
        setMargins (2)
        panel11 (data)
      },
      {
        setMargins (2)
        panel12 (data)
      },
      {
        setMargins (3)
        panel13 (data)
        AddFooter ()
      }, 
      {
        setMargins (2)
        panel21 (data)
      },
      {
        setMargins (2)
        panel22 (data)
      },
      {
        setMargins (3)
        panel23 (data)
        AddFooter ()
      }
    )
  } else {
    
    ##########################################################
    layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
    if (is.na (Seq) || Seq == 1) {
      setMargins (4)
      panel11 (data)
      panel12 (data)
      setMargins (5)
      panel13 (data)
      AddFooter ()
      if (!is.na(Seq) && (Seq == 1)) {return()}
    }
    # add TAS and MACH plots:
    setMargins (4)
    panel21 (data)
    panel22 (data)
    setMargins (5)
    panel23 (data)
    AddFooter ()
  }
}
