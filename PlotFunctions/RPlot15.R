### plot 15: CN, FSSP, CDP, F300, CONCP, CONC1DC_LWOO
RPlot15 <- function(data, Seq=NA, panl=1) {
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  panel11 <- function(data) {
    if (is.na(VRPlot[[15]][1])) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, 'no particle or hydrometeor measurements', adj=0.5)
      return()
    } 
    CU <- VRPlot[[15]]
    ## only use CONCU or CONCN for the first plot
    
    ## select those from VRPlot[[15]] needed for the first panel:
    V15a <- c('CONCU_', 'CONCN', 'CONCP_', 'CONCU100_', 'CONCU500_', 'CNCONC_')  ## either CONCN or CONCN_WCN
    CU1 <- vector()
    for (v in V15a) {    ## why not the simpler charmatch? Because it doesn't work for multiple probes
      if (any(grepl(v, CU))) {
        CU1 <- c(CU1, v)
      }
    }
    if (length(CU1) > 0) {
      va <- vector()
      for (c in CU1) {
        nm <- names(data)[which(grepl(c, names(data)))]
        nm <- nm[nm %in% CU]
        if (length (nm) > 1) {nm <- nm[1]}  ## if not explicitly given, select first match
        # v <- sub("_.*", "", c)
        data[, nm] <- SmoothInterp (data[, nm])
        va <- c(va, nm)
      }
      # remove zeroes for log plot:
      for (v in va) {
        data[!is.na(data[, v]) & (data[, v] <= 0), v] <- NA
      }
      
      if (length(va) > 0) {
        plotWAC (data[, c("Time", va)], 
          logxy='y', 
          ylim = YLMF (1, c(1, 1.e5)), 
          ylab=expression (paste ("CONCy [cm"^"-3"*"]")))
        title ("1-min filter", cex.main=0.75)
      } else {
        plot (0,0, xlim=c(data$Time[1], data$Time[nrow(data)]), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (data$Time[1], 0.8, 'no particle measurements', adj=0.5)
      }
    } else {
      plot (0,0, xlim=c(data$Time[1], data$Time[nrow(data)]), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      tc <- data$Time[nrow(data) %/% 2]
      text (tc, 0.8, labels='no particle measurements')
    }
  }
  
  panel12 <- function(data) {
    C <- VRPlot[[15]]
    C <- C[which("CONC" == substr(C, 1, 4) & "CONCU" != substr(C,1,5) & 
        "CONCN" != substr(C,1,5) & "CONCP" != substr(C,1,5))]
    va2 <- vector()
    for (c in C) {
      nm <- names(data)[grepl(c, names(data))]
      # v <- sub("_.*", "", c)
      data[, nm] <- SmoothInterp (data[, nm])
      va2 <- c(va2, nm)
    }
    # print (c("va2", va2))
    for (v in va2) {
      data[!is.na(data[, v]) & (data[, v] <= 0), v] <- NA
    }
    if (length(va2) > 0) {
      plotWAC (data[, c("Time", va2)],
        logxy='y', 
        ylim = YLMF (2, c(1.e-4, 1e4)),  
        ylab=expression(paste("CONCy [cm"^"-3"*"]")))
      title ("1-min filter", cex.main=0.75)
    } 
  }
  
  panel21 <- function(data) {
    nm6 <- names(data)[grepl("USHFLW_", names(data))]
    if (length(nm6) == 0) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, 'no housekeeping variables for the UHSAS')
      return()
    }
    USHFLW <- data[, nm6]
    nm7 <- names(data)[grepl("USMPFLW_", names(data))]
    USMPFLW <- data[, nm7]
    USHF <- USHFLW/10
    plotWAC (data.frame(Time=data$Time, USMPFLW, USHF),
      ylab="flows", legend.position='topright',
      ylim = YLMF (1, c(0, 2.5)))
    hline (0.82, 'blue'); hline (1, 'darkgreen'); hline(0.5, 'red'); hline (1.5, 'red')
    legend ("topleft", legend=c("dashed red: limits for FCNC, XICNC, PFLWC", 
      "dashed blue-green: expected values for corresponding flows"), text.col=c('red', 'blue'), cex=0.55)
    title ("USHF is USHFLW_RWOOU/10", cex.main=0.75)
  }
  
  panel22 <- function (data) {
    nm8 <- names(data)[grepl("UREF_", names(data))]
    UREF <- data[, nm8]
    nm9 <- names(data)[grepl("USCAT_", names(data))]
    USCAT <- data[, nm9]
    plotWAC (data.frame (Time=data$Time, UREF, USCAT), 
      ylab="laser V", legend.position='topright', 
      ylim = YLMF (2, c(0, 10)))
    hline (2.10, 'blue'); hline (1.90, 'darkgreen'); hline(6, 'red'); hline (9.95, 'red')
    title ("dashed-blue: lower limit for UREF; dashed-green: upper limit for USCAT", 
      cex.main=0.65)
  }
  
  
  ############################################################
  if (shinyDisplay) {
    nseq <- 2*(Seq-1) + panl
    switch(nseq,
      {
        setMargins (2)
        panel11 (data)
      },
      {
        setMargins (3)
        panel12 (data)
        AddFooter ()
      },
      {
        setMargins (2)
        panel21 (data)
      },
      {
        setMargins (3)
        panel22 (data)
        AddFooter ()
      }
    )
    ############################################################
  } else {
    layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
    if (is.na(Seq) || (Seq == 1)) {
      setMargins (4)
      panel11 (data)
      setMargins (5)
      panel12 (data)
      AddFooter ()
      if (!is.na(Seq) && (Seq == 1)) {return ()}
    }
    setMargins (4)
    panel21 (data)
    setMargins (5)
    panel22 (data)
    AddFooter ()
  }
}

