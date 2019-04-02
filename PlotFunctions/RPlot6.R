### plot 6: ambient pressures
RPlot6 <- function (data, Seq=NA, panl=1, ...) { 
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  panel11 <- function (data) {
    # include only pressures corrected for static defect:
    P <- VRPlot[[6]]
    P <- P[c (which (grepl('C$', P)), which (grepl('_A', P)))]  # no $ on second to include PS_A2
    ylm <- range (as.matrix (data[, P]), finite=TRUE)
    plotWAC (DF <- data[, c("Time", P)], ylab='pressure [hPa]',
      lwd = c(2.5, 2, 1.5, 1),
      ylim = YLMF (1, ylm))  
    titl <- "Mean diff vs PSXC: "
    ## assume PSXC is always present (but check)
    if ('PSXC' %in% names (data)) {
      for (p in VRPlot[[6]]) {
        if (p == 'PSXC') {next}
        pl <- sub("^PS", "", p)
        titl <- sprintf("%s%s-%s: %.2f; ", titl, pl, 'PSXC',
          mean(data[, p] - data$PSXC, na.rm=TRUE))
      }
      title(titl, cex.main = cexmain)  
    }
  }
  
  panel12 <- function (data) { # corrected pressures only
    # Plot the differences:
    P <- VRPlot[[6]]
    PC <- P[c (which (grepl('C$', P)), which (grepl('_A', P)))]
    DF <- data[, c('Time', PC[-1])] # use first in list as reference
    DF <- DF - data[, PC[1]]
    plotWAC(DF, ylab=expression(paste (Delta,' [hPa]')), 
      lwd = c(2.5, 2, 1.5, 1),
      ylim = YLMF (2, c(-5, 5)))
    hline (-1); hline (1)
    title (sprintf ('differences vs %s', PC[1]), cex.main = cexmain)
  }
  
  panel13 <- function (data) {
    # uncorrected pressures
    P <- VRPlot[[6]]
    PC <- P[c (which (grepl('C$', P)), which (grepl('_A', P)))]
    P <- P[!(P %in% PC)]
    if ('PSX' %in% P) {P <- P[-which ('PSX' == P)]}
    DF <- data[, c('Time', P)] # use PSXC as reference if present; else skip
    if ('PSXC' %in% names (data)) {
      DF <- DF - data$PSXC
      plotWAC(DF, ylab=expression(paste (Delta,' [hPa]')), 
        lwd = c(2.5, 2, 1.5, 1),
        ylim = YLMF (3, c(-10, 10)))
      hline (-1, col = 'darkorange'); hline (1, col = 'darkorange')
      title ('differences, uncorrected P vs PSXC', cex.main = cexmain)
    }
  }
  
  
  ######################################################
  if (shinyDisplay) {
    switch (panl,
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
      }
    )
    
    ######################################################
  } else {  # savePDF section
    layout(matrix(1:3, ncol = 1), widths = c(5,5,6))
    setMargins (4)
    panel11 (data)
    panel12 (data)
    setMargins (5)
    panel13 (data)
    AddFooter ()
  }
}
