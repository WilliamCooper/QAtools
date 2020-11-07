### plot 9: wind
RPlot9 <- function (data, Seq = NA, panl = 1) {
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  ## WIND DIRECTION: plot 13a
  panel11 <- function (data) {
    line.widths <- c(1, 1, 2)
    line.types <- c(1, 3, 2)
    # set transparency (180) to avoid obscuring first trace
    tgreen <- rgb (0, 200, 0, 180, maxColorValue = 255)
    cs <- c('blue', tgreen, 'red', 'cyan', 'darkorange', 'violet')
    WD <- VRPlot[[9]][grepl ('^WD', VRPlot[[9]])]
    if ('IWD' %in% VRPlot[[9]]) {
      WD <- c(WD, 'IWD')
    }
    if (any(!is.na(data[, WD]))) {
      plotWAC (
        data[, c("Time", WD)],
        col = cs,
        lwd = line.widths,
        lty = line.types,
        ylab = expression (paste ("WDC [", degree, "]")),
        legend.position = NA,
        cex.axis = 1.5,
        cex.lab = 1.5,
        ylim = YLMF (1, range (as.matrix (data[, WD]), finite = TRUE))
      )
      legend(
        'bottomright',
        WD,
        col = cs,
        text.col = cs,
        lty = line.types,
        lwd = line.widths
      )
      hline (0)
      hline (90)
      hline (180)
      hline (270)
      hline (360)
    } else {
      plot(c(-1,1), c(-1,1), type='n')
      text (0,0,labels='no wind-direction measurements for this flight')
    }
  }
  
  ## WIND SPEED:  plot 13b
  panel12 <- function (data) {
    line.widths <- c(1, 1, 2)
    line.types <- c(1, 3, 2)
    # set high transparency (30) to avoid obscuring first trace
    tgreen <- rgb(0, 200, 0, 180, maxColorValue = 255)
    cs <- c('blue', tgreen, 'red', 'cyan', 'darkorange', 'violet')
    WS <- VRPlot[[9]][grepl ('^WS', VRPlot[[9]])]
    if ('IWS' %in% VRPlot[[9]]) {
      WS <- c(WS, 'IWS')
    }
    plotWAC (
      data[, c("Time", WS)],
      col = cs,
      lwd = line.widths,
      lty = line.types,
      ylab = "WSC [m/s]",
      legend.position = NA,
      cex.axis = 1.5,
      cex.lab = 1.5,
      ylim = YLMF (2, range (as.matrix (data[, WS]), finite = TRUE))
    )
    legend(
      'bottomright',
      WS,
      col = cs,
      text.col = cs,
      lty = c(1, 3),
      lwd = c(1, 1)
    )
  }
  
  ## VERTICAL WIND - plot 13c
  panel13 <- function(data) {
    WI <- VRPlot[[9]][grepl ('^WI', VRPlot[[9]])]
    plotWAC (
      data[, c("Time", WI)],
      ylab = "vertical wind WIC [m/s]",
      cex.axis = 1.5,
      cex.lab = 1.5,
      ylim = YLMF (3, range (as.matrix (data[, WI]), finite = TRUE))
    )
    title (sprintf (
      "average vertical wind: WIC = %.02f",
      mean (data[, WI[1]], na.rm = TRUE)
    ),
    cex.main = 1.2)
    hline (2)
    hline (-2)
    hline (0, 'red')
  }
  
  ## EASTERLY COMPONENT OF THE WIND: plot 14a
  panel21 <- function(data) {
    line.widths <- c(1, 1, 2)
    line.types <- c(1, 3, 2)
    # set high transparency (30) to avoid obscuring first trace
    tgreen <- rgb(0, 200, 0, 180, maxColorValue = 255)
    cs <- c('blue', tgreen, 'red', 'cyan', 'darkorange', 'violet')
    data$IUX <- data$IWS * sin (data$IWD * pi / 180)
    data$UIC <- data$WSC * sin (data$WDC * pi / 180)
    if (any(!is.na(data[, c('IUX', 'UIC')]))) {
      plotWAC (
        data[, c("Time", "UIC", "IUX")],
        col = cs,
        lwd = line.widths,
        lty = line.types,
        ylab = "easterly wind [m/s]",
        legend.position = NA,
        ylim = YLMF (1, range (as.matrix (data[, c('UIC', 'IUX')]), finite =
                                 TRUE))
      )
      legend(
        'bottom',
        c("UIC", "IUX"),
        col = c("blue", tgreen),
        text.col = c("blue", tgreen),
        lty = c(1, 3),
        lwd = c(1, 1),
        cex = 0.75
      )
    } else {
      plot(c(-1,1), c(-1,1), type='n')
      text (0,0,labels='no data for this flight')
    }
    
  }
  
  ## NORTHERLY COMPONENT OF THE WIND - 14b
  panel22 <- function(data) {
    line.widths <- c(1, 1, 2)
    line.types <- c(1, 3, 2)
    # set high transparency (30) to avoid obscuring first trace
    tgreen <- rgb(0, 200, 0, 180, maxColorValue = 255)
    cs <- c('blue', tgreen, 'red', 'cyan', 'darkorange', 'violet')
    data$IVY <- -data$IWS * cos (data$IWD * pi / 180)
    data$VIC <- -data$WSC * cos (data$WDC * pi / 180)
    if (any(!is.na(data[, c('IVY', 'VIC')]))) {
      plotWAC (
        data[, c("Time", "VIC", "IVY")],
        col = cs,
        lwd = line.widths,
        lty = line.types,
        ylab = "northerly wind [m/s]",
        legend.position = NA,
        ylim = YLMF (2, range (as.matrix (data[, c('VIC', 'IVY')]), finite =
                                 TRUE))
      )
      legend(
        'bottom',
        c("VIC", "IVY"),
        col = c("blue", tgreen),
        text.col = c("blue", tgreen),
        lty = c(1, 3),
        lwd = c(1, 1),
        cex = 0.75
      )
    } else {
      plot(c(-1,1), c(-1,1), type='n')
      text (0,0,labels='no data for this flight')
    }
  }
  
  
  ################################################################
  if (shinyDisplay) {
    nseq <- (Seq - 1) * 3 + panl
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
             AddFooter()
           },
           {
             setMargins (2)
             panel21 (data)
           },
           {
             setMargins (3)
             panel22 (data)
             AddFooter()
           })
    ################################################################
  } else {
    ## needs WDC, WSC, WIC, IWD, IWS
    layout(matrix(1:3, ncol = 1),
           widths = 1,
           heights = c(5, 5, 6))
    if (is.na(Seq) || Seq == 1) {
      setMargins (4)
      panel11 (data)
      panel12 (data)
      setMargins (5)
      panel13 (data)
      AddFooter ()
    }
    if (!is.na(Seq) && (Seq == 1)) {
      return()
    }
    
    # Seq == 2 case:
    layout(matrix(1:2, ncol = 1),
           widths = 1,
           heights = c(5, 5.5))
    setMargins (4)
    panel21 (data)
    setMargins (5)
    panel22(data)
    AddFooter ()
  }
}
