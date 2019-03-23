### plot 5: humidity
RPlot5 <- function (data, Seq=NA, panl=1) {
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  # print(sys.status())
  # To maintain consistency between the shiny-app display and the PDF
  # generator (savePDF()) both will use the same "panel" functions.
  
  ## DEW POINTS - plot 5
  panel11 <- function (data) {
    DP <- c(VRPlot[[5]][grepl ('^DP', VRPlot[[5]])], 'ATX')
    ifelse (exists('panel1ylim'),
      plotWAC (data[, c("Time", DP)], 
        ylab=expression (paste ("dew point  DPy  [", degree, "C]")), 
        lty=c(1,1,2,1), lwd=c(2,1.5,1,3), legend.position='bottom', 
        col=c('blue', 'red', 'darkgreen', 'black'), ylim=panel1ylim),
      plotWAC (data[, c("Time", DP)], 
        ylab=expression (paste ("dew point  DPy  [", degree, "C]")), 
        lty=c(1,1,2,1), lwd=c(2,1.5,1,3), legend.position='bottom', 
        col=c('blue', 'red', 'darkgreen', 'black'), ylim=c(-90,30))
    )
    labl <- DP
    labl <- sub("DP_", "", labl)
    titl <- "Mean diff "
    ## assume DPXC is always present:
    for (dp in DP) {
      if (dp == 'ATX') {next}
      dpl <- sub("DP_", "", dp)
      titl <- sprintf("%s%s-%s: %.2f; ", titl, dpl, 'DPXC',
        mean(data[, dp] - data$DPXC, na.rm=TRUE))
    }
    title(titl, cex.main=0.8)   
  }
  
  ## DEW POINT DIFFERENCES - plot 6
  ## (brush is inhibited)
  panel21 <- function(data) {
    DP <- VRPlot[[5]][which (grepl ('^DP', VRPlot[[5]]))]
    ## use DPXC as primary if present, otherwise DP_VXL, otherwise first:
    ir <- which ('DPXC' == DP)
    #   if (length (ir) != 1) {
    #     ir <- which ('DP_VXL' == DP)
    #   }
    ## but don't use it if all-missing
    #   if (!(any (!is.na(data[, DP[ir]])))) {ir <- NULL}
    #   if (length (ir) != 1) {
    #     ir <- which ('DPV_VXL' == DP) ## used in earlier projects
    #   }
    if (length (ir) != 1) {
      ir <- 1
      if (!(any (!is.na(data[, DP[ir]])))) {ir <- 2}
    }
    colr <- c("blue", "darkgreen", "darkorange", "cyan")
    firstPlot <- TRUE
    i <- 1
    for (DPV in DP) {
      if (DPV == DP[ir]) {next}
      if (firstPlot) {
        firstPlot <- FALSE
        plot(data[, c(DP[ir], DPV)], pch=20, col=colr[i], 
          xlab=bquote (.(DP[ir])~'['*degree*C*']'),
          ylab=expression (paste ("dew point  DPy  [", degree, "C]")),
          ylim=c(-90,30))
        lines (c(-90.,30.), c(-85,35), col="darkorange", lwd=2, lty=2)
        lines (c(-90.,30.), c(-95,25), col="darkorange", lwd=2, lty=2)
        i <- i + 1
        DPL <- DPV
      } else {
        points (data[, c(DP[ir], DPV)], pch=20, col=colr[i], cex=0.5)
        i <- i + 1
        DPL <- c(DPL, DPV)
      }
    }
    legend ('bottomright', legend=DPL, col=colr,
      text.col=colr, pt.cex=c(1., 0.5, 0.5, 0.5))
    title("dashed orange lines: +/-5C error bands", cex.main=0.8)
  }
  
  ## DP CAVITY PRESSURES - plot 7a
  panel31 <- function(data) {
    CAVP <- VRPlot[[5]][grepl('^CAVP', VRPlot[[5]])]
    if (any (grepl('P_DPL', VRPlot[[5]]))) {
      if (!("CAVP_DPL" %in% names(data))) {
        data$CAVP_DPL <- with(data, cavcfL[1] + cavcfL[2] * PSXC + cavcfL[3] * QCXC + cavcfL[4] * MACHX + cavcfL[5] * AKRD)
      } else {  # calculate from formula
        data$CAVPF_DPL <- with(data, cavcfL[1] + cavcfL[2] * PSXC + cavcfL[3] * QCXC + cavcfL[4] * MACHX + cavcfL[5] * AKRD)
      }
      # if (!("CAVP_DPR" %in% names (data))) {
      #   data$CAVP_DPR <- data$PSXC*(1.0162 +0.003024*data$QCFC
      #                               - 1.34521*MachNumber (data$PSXC, data$QCFC)^2)
      # } else {
      #   data$CAVPF_DPR <- data$PSXC*(1.0162 +0.003024*data$QCFC
      #                                - 1.34521*MachNumber (data$PSXC, data$QCFC)^2)
      # }
      ## Note: the coefficients that follow, from CAVPcoefficients.Rdata, are GV only
      #    if (!("CAVP_DPR" %in% names(data))) {
      #      data$CAVP_DPR <- with(data, cavcfR[1] + cavcfR[2] * PSXC + cavcfR[3] * QCXC + cavcfR[4] * MACHX + cavcfR[5] * AKRD)
      #    } else {  # calculate from formula
      #      data$CAVPF_DPR <- with(data, cavcfR[1] + cavcfR[2] * PSXC + cavcfR[3] * QCXC + cavcfR[4] * MACHX + cavcfR[5] * AKRD)
      #    }
      ## replacement Feb 2019 (no valid C-130 values before this)
      if ("CAVP_DPR" %in% names(data)) {
        data$CAVPF_DPL <- with(data, PSXC * (1 + cavc[1,1] + 
            cavc[2,1]*QCXC + cavc[3,1]*MACHX + cavc[4,1]*AKRD))
        data$CAVPF_DPR <- with(data, PSXC * (1 + cavc[1,2] + 
            cavc[2,2]*QCXC + cavc[3,2]*MACHX + cavc[4,2]*AKRD))
        CAVP <- c(CAVP, 'CAVPF_DPL', 'CAVPF_DPR')
      }
    } else {
      if ("CAVP_DPB" %in% names(data)) {
        data$CAVPF_DPB <- with(data, PSXC * (1 + cavc[1,1] + 
            cavc[2,1]*QCXC + cavc[3,1]*MACHX + cavc[4,1]*AKRD))
        data$CAVPF_DPT <- with(data, PSXC * (1 + cavc[1,2] + 
            cavc[2,2]*QCXC + cavc[3,2]*MACHX + cavc[4,2]*AKRD))
        
        CAVP <- c(CAVP, 'CAVPF_DPB', 'CAVPF_DPT')
      }
    }
    if (length(CAVP) < 1) {return(0)}
    # DP cavity pressures and VCSEL laser intensity:
    ifelse (exists ('panel1ylim'),
      plotWAC (data[, c("Time", CAVP, "PSXC")], 
        lwd=c(1,1,2,2,1), lty=c(1,1,2,2,1), ylab='CAVP [hPa]',
        legend.position='topleft', ylim=panel1ylim),
      plotWAC (data[, c("Time", CAVP, "PSXC")], 
        lwd=c(1,1,2,2,1), lty=c(1,1,2,2,1), ylab='CAVP [hPa]',
        legend.position='topleft')
    )
    # pulling legend out of plotWAC to increase font size
    # legend('bottomright',c("CAVP_DPR", "CAVP_DPL", "PSXC"),col=c("blue","darkgreen","red"),text.col=c("blue","darkgreen","red"),lty=c(1,2,1),lwd=c(2,1,1),cex=0.75)
    title (sprintf ("mean above PSXC: %.1f (DPL) and %.1f (DPR)", 
      mean (data$CAVP_DPL - data$PSXC, na.rm=TRUE),
      mean (data$CAVP_DPR - data$PSXC, na.rm=TRUE)), cex.main=0.75)
  }
  
  ## VCSEL LASER INTENSITY - plot 7b
  panel32 <- function(data) {
    if ("LSRINT_VXL" %in% names(data)) {
      ifelse (exists ('panel2ylim'),
        plotWAC (data[, c("Time", "LSRINT_VXL")], ylim=c(0,4000),
          ylab="LSRINT_VXL", ylim=panel2ylim),
        plotWAC (data[, c("Time", "LSRINT_VXL")], ylim=c(0,4000),
          ylab="LSRINT_VXL")
      )
      abline (h=1000, col='red', lty=2); abline (h=2700, col='red', lty=2)
    }
  }
  
  ## VAPOR PRESSURES - plot 8a
  panel41 <- function(data) {
    if (!("EW_VXL" %in% names(data))) {
      if ("DP_VXL" %in% names(data)) {data$EW_VXL <- MurphyKoop (data$DP_VXL)}
      if ("DPV_VXL" %in% names(data)) {data$EW_VXL <- MurphyKoop (data$DPV_VXL)}
    }
    ## special in HIPPO-2 to avoid VCSEL spikes
    if (Project == 'HIPPO-2') {
      data$EW_VXL[data$EW_VXL > 20] <- NA
    }
    # vapor pressure and mixing ratio
    ## get EW variables
    VEW <- VRPlot[[5]]
    VEW <- VEW[which ("EW" == substr(VEW, 1, 2))]
    ## the following was useful in some old projects; suppress now
    # if (!("EW_VXL" %in% VEW) && ("EW_VXL" %in% names(data))) {VEW <- c(VEW, "EW_VXL")}
    ifelse (exists ('panel1ylim'),
      plotWAC (data[, c("Time", c(VEW))], ylab="EWy [hPa]", 
        logxy='y', ylim=panel1ylim, legend.position='bottom', 
        cex.lab=1.5, cex.axis=1.5),
      plotWAC (data[, c("Time", c(VEW))], ylab="EWy [hPa]", 
        logxy='y', ylim=c(1e-2, 100), legend.position='bottom', 
        cex.lab=1.5, cex.axis=1.5)
    )
    lines (data$Time, MurphyKoop (data$ATX, data$PSXC), col='cyan', lty=2)
    # pulling legend out of plotWAC to increase font size
    # legend('bottomright',c("EW@ATX","EW_DPL", "EW_DPR", "EW_VXL"),col=c("cyan","blue","darkgreen","red"),text.col=c("cyan","blue","darkgreen","red"),lty=c(2,1,1,1),lwd=c(2,1,1,1))
    title ("cyan line: equilibrium vapor pressure at ATX")
  }
  
  ## MIXING RATIOS - plot 8b
  panel42 <- function(data) {
    if ("MR" %in% names(data)) {
      ## get EW variables
      VEW <- VRPlot[[5]]
      VEW <- VEW[which ("EW" == substr(VEW, 1, 2))]
      MRVAR <- sub ("EW", "MR", VEW)
      for (i in 1:length(MRVAR)) {
        data[, MRVAR[i]] <- 0.622 * data[, VEW[i]] / (data$PSXC-data[, VEW[i]]) * 1000
      }
      if ("H2OMR_GMD" %in% names(data)) {
        MRVAR <- c(MRVAR, "H2OMR_GMD")
        data$H2OMR_GMD <- data$H2OMR_GMD / 1000. * 0.622
      }
      ifelse (exists ('panel2ylim'),
        plotWAC (data[, c("Time", MRVAR)], ylab="mixing ratio [g/kg]",
          logxy='y', ylim=panel2ylim, cex.lab=1.5, cex.axis=1.5),
        plotWAC (data[, c("Time", MRVAR)], ylab="mixing ratio [g/kg]",
          logxy='y', ylim=c(0.01, 100), cex.lab=1.5, cex.axis=1.5)
      )
    } 
  }
  
  ## RELATIVE HUMIDITY: plot 8c
  panel43 <- function(data) {
    ## get EW variables
    VEW <- VRPlot[[5]]
    VEW <- VEW[which ("EW" == substr(VEW, 1, 2))]
    RHVAR <- sub("EW", "RH", VEW)
    for (i in 1:length (RHVAR)) {
      data[, RHVAR[i]] <- 100 * data[, VEW[i]] / MurphyKoop (data$ATX, data$PSXC)
    }
    ifelse (exists ('panel3ylim'), 
      plotWAC (data[, c("Time", RHVAR)], lty=c(1,1,2), lwd=1, 
        ylab="relative humidity [%]",cex.lab=1.5,cex.axis=1.5, 
        legend.position='topright', ylim=panel3ylim),
      plotWAC (data[, c("Time", RHVAR)], lty=c(1,1,2), lwd=1, 
        ylab="relative humidity [%]", cex.lab=1.5, cex.axis=1.5, 
        legend.position='topright', ylim=c(0,150))
    )
    # pulling legend out of plotWAC to increase font size
    # legend('topright',c("RHDPL", "RHDPR", "RHVXL"),col=c("blue","darkgreen","red"),text.col=c("blue","darkgreen","red"),lty=c(1,1,2),lwd=1)
    abline (h=100, col='red', lty=2)
  }
  
  
  
  #################################################
  if (shinyDisplay) { # code here for the shiny app; then code for DataReview.R
    op <- par (mfrow=c(1,1), mar=c(5,5,1,1)+0.1,oma=c(1.1,0,0,0)) #last for Footer
    nseq <- (Seq-1) + panl
    if (Seq == 4) {nseq <- nseq + 1}
    switch(nseq, 
      {
        panel11(data)
        AddFooter ()
      }, 
      {
        panel21(data)
        AddFooter ()
      }, 
      {
        panel31(data)
      }, 
      {
        panel32(data)
        AddFooter ()
      }, 
      {
        op <- par (mar=c(1,5,1,1)+0.1)
        panel41(data)
      }, 
      {
        op <- par (mar=c(1,5,1,1)+0.1)
        panel42(data)
      },
      {
        panel43(data)
        AddFooter ()
      }
    )
    
    ##################################################### 
  } else { # This section is not interactive; it is here for DataReview.R
    if (is.na(Seq) || Seq == 1) {
      op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1,oma=c(1.1,0,0,0))
      panel11(data)
      AddFooter ()
      if (!is.na(Seq) && (Seq == 1)) {return()}
    }
    if (is.na(Seq) || Seq == 2) {
      panel21(data)
      AddFooter ()
      if (!is.na(Seq) && (Seq == 2)) {return()}
    }
    if (is.na(Seq) || Seq == 3) {
      # if (Trace) {print (c('RPlot5: names in data:', names(data)))}
      # DP cavity pressures and VCSEL laser intensity:
      layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,5))
      op <- par (mar=c(2,4,1,2.5)+0.1)
      panel31(data)
      op <- par (mar=c(5,4,1,2.5)+0.1)
      panel32(data)
      AddFooter ()
      if (!is.na(Seq) && (Seq == 3)) {return()}
    }
    
    # vapor pressure and mixing ratio
    op <- par (mar=c(2,5,1,1)+0.1)
    layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
    panel41(data)
    panel42(data)
    op <- par (mar=c(5,5,1,1)+0.1)
    panel43(data)
    AddFooter ()
  }
}

