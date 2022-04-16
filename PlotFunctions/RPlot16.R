### plot 16: DBAR (mean diameters) and PLWC (liquid water content)
# do 1-min smoothing; otherwise, too noisy
RPlot16 <- function (data, Seq=NA, panl=1) {
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  panel11 <- function(data) {
    if (is.na(VRPlot[[16]][1])) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, 'no LWC or DBAR measurements')
      return()
    }
    DB <- VRPlot[[16]]
    DB <- DB[which(("DBARU" == substr(DB, 1, 5)) | ("DBARP" == substr(DB, 1, 5)))]
    va <- vector()
    for (c in DB) {
      nm <- names(data)[grepl(c, names(data))]
      v <- sub("_.*", "", c)
      data[, v] <- SmoothInterp(data[, nm])
      va <- c(va, v)
    }
    # if (!(any(grepl("DBAR1DC_", VRPlot[[16]])))) {
    #   op <- par (mar=c(5,4,1,1)+0.1)
    # }
    if (length (va) > 0) {
      plotWAC (data[, c("Time", va)], 
        ylim = YLMF (1, c(0, 0.5)), 
        ylab=bquote("aerosol DBAR [" * mu * "m]"), 
        legend.position="topright")
      if (length (va) == 1) {
        legend('topright', legend=va[1], col='blue')
      }
    }
    title ("1-min filter", cex.main = cexmain)
  }
  
  panel12 <- function (data) {
    DB <- VRPlot[[16]]
    DB <- DB[which (("DBAR" == substr(DB, 1, 4)) & ("DBARU" != substr(DB, 1, 5))
      & ("DBARP" != substr(DB, 1, 5)) & ("DBARS" != substr(DB, 1, 5)))]
    va <- vector()
    for (c in DB) {
      nm <- names(data)[grepl(c, names(data))]
      v <- sub("_.*", "", c)
      data[, v] <- SmoothInterp(data[, nm])
      va <- c(va, v)
    }
    if (length (va) > 0) {
      plotWAC(data[, c("Time", va)], 
        ylim = YLMF (2, c(0, 30)), 
        ylab=bquote("cloud-droplet DBAR ]" * mu * "m]"), 
        legend.position="topright")
      if (length (va) == 1) {
        legend('topright', legend=va[1], col='blue')
      }
      title ("1-min filter", cex.main = cexmain) 
    }
  }
  
  panel13 <- function (data) {
    if (any(grepl("DBAR1DC_", VRPlot[[16]])) || any(grepl("DBARS_", VRPlot[[16]]))) {
      nm <- names(data)[grepl("DBAR1DC_", names(data)) | grepl("DBARS_", names(data))]
      va <- vector()
      for (c in nm) {
        v <- sub("_.*", "", c)
        data[, v] <- SmoothInterp(data[, c], .order=1)
        va <- c(va, v)
      }
      plotWAC(data[, c("Time", va)], 
        ylab = bquote('2D DBAR [' * mu * 'm]'), legend_position = 'topright',
        ylim = YLMF (3, range (as.matrix (data[, va]), finite=TRUE)))
      if (length(nm) == 1) {
        legend('topright', legend=va[1], col='blue')
      }
      title ("1-min filter", cex.main = cexmain) 
    }
  }
  
  panel21 <- function (data) {
    LW <- VRPlot[[16]]
    LW <- LW[which(("PLWC" == substr(LW, 1, 4)) & ("PLWC" != LW))]
    if ('RICE' %in% names (data)) {LW <- c(LW, "RICE")}
    va3 <- vector()
    for (c in LW) {
      nm <- names(data)[which(grepl(c, names(data)))]
      v <- sub("_.*", "", c)
      data[, v] <- SmoothInterp(data[, nm])
      va3 <- c(va3, v)
    }
    plotWAC (data[, c("Time", va3)], 
      ylim = YLMF (1, c(0,2)),
      ylab="PLWCy", 
      legend.position="topright")
    if (length(va3) == 1) {
      legend('topright', legend=va3[1], col='blue', lty=1)
    }
    title ("1-min filter", cex.main = cexmain)
  }
  
  panel22 <- function (data) {
    if ("PLWC" %in% names(data) && "RICE" %in% names(data)) {
      plotWAC (data[, c("Time", "PLWC", "RICE")], 
        ylim = YLMF (2, c(0, 25)), 
        ylab="PLWC (Watts)")
      hline (10); hline (15)
    }
  }
  
  panel23 <- function (data) {    
    LW <- VRPlot[[16]]
    LW <- LW[which(("PLWC" == substr(LW, 1, 4)) & ("PLWC" != LW))]
    if ('RICE' %in% names (data)) {LW <- c(LW, "RICE")}
    if (any(grepl("PLWC1DC", LW))) {
      va <- names(data)[which(grepl('PLWC1DC', names(data)))]
      plotWAC(data[, c("Time", va)],
        ylim = YLMF (3, range (as.matrix (data[, va]), finite=TRUE)))
    }
  }
  
  panel31 <- function (data) {
    if (any (grepl ("TCNTD_", VRPlot[[16]])) && any (grepl ("REJDOF_", VRPlot[[16]]))) {
      TCNTD <- data[, names(data)[grepl("TCNTD_", names(data))]]
      REJDOF <- data[, names(data)[grepl("REJDOF_", names(data))]]
      DOFACC <- TCNTD / (TCNTD + REJDOF)
      DOFACC <- SmoothInterp (DOFACC)
      plotWAC (data.frame (Time=data$Time, DOFACC), 
        ylab="DOF acceptance fraction", 
        ylim = YLMF (1, c(0,1)))
      hline (0.2, 'red')
    }
  }
  
  panel32 <- function (data) {
    if (any (grepl ("AVGTRNS_", names (data)))) {
      AVGTRNS <- data[, names(data)[grepl("AVGTRNS_", names(data))]]
      AVT <- SmoothInterp (AVGTRNS)
      plotWAC (data.frame (Time=data$Time, AVT), 
        ylim = YLMF (2, c(0, 2)))
    }
  }
  
  panel33 <- function (data) {
    if (any(grepl ("CDPLSRP_", names (data)))) {
      CDPLSRP <- data[, names(data)[grepl("CDPLSRP_", names(data))]]
      plotWAC (data.frame(Time=data$Time, CDPLSRP), 
        ylab="CDP laser power", 
        ylim = YLMF (3, c(0, 4)))
      hline(3, 'red')
    }
  }
  
  
  ###############################################################
  if (shinyDisplay) {
    op <- par (mfrow=c(1,1), mar=c(5,5,1,1)+0.1,oma=c(1.1,0,0,0))
    nseq <- 3*(Seq-1) + panl
    switch (nseq,
      {
        setMargins (2)
        panel11(data)
      },
      {
        setMargins (2)
        panel12(data)
      },
      {
        setMargins (3)
        panel13(data)
        AddFooter()
      },
      {
        setMargins (2)
        panel21(data)
      },
      {
        setMargins (2)
        panel22(data)
      }, 
      {
        setMargins (3)
        panel23(data)
        AddFooter()
      },
      {
        setMargins (2)
        panel31(data)
      },
      {
        setMargins (2)
        panel32(data)
      },
      {
        setMargins (3)
        panel33(data)
        AddFooter()
      }
    )
    ###############################################################
  } else {
    if (is.na (Seq) || Seq == 1) {
      if (any(grepl("DBAR1DC_", VRPlot[[16]])) || any(grepl("DBARS_", VRPlot[[16]]))) {
        layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
      } else {
        layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
      }
      setMargins (4)
      panel11(data)
      panel12(data)
      setMargins (5)
      panel13(data)
      AddFooter()
      if (!is.na(Seq) && (Seq == 1)) {return()}
    }
    if (is.na(Seq) || Seq == 2) {
      layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
      ## Water measurements:
      setMargins (4)
      panel21(data)
      panel22(data)
      setMargins (5)
      panel23(data)
      AddFooter()
      if (!is.na(Seq) && (Seq == 2)) {return()}
    }
    layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
    setMargins (4)
    panel31(data)
    panel32(data)
    setMargins (5)
    panel33(data)
    AddFooter ()
  }
}
