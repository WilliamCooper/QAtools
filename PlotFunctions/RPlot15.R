### plot 15: CN, FSSP, CDP, F300, CONCP, CONC1DC_LWOO
RPlot15 <- function(data, Seq=NA) {
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  if (is.na(Seq) || (Seq == 1)) {
    op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
    CU <- VRPlot[[15]]
    CU <- CU[which("CONCU" == substr(CU, 1, 5) | "CONCN" == substr(CU, 1, 5))]
    va <- vector()
    for (c in CU) {
      nm <- names(data)[grepl(c, names(data))]
      if (length (nm) > 1) {nm <- nm[1]}  ## handle multiple probes
      v <- sub("_.*", "", c)
      data[, v] <- SmoothInterp (data[, nm])
      va <- c(va, v)
    }
    # remove zeroes for log plot:
    for (v in va) {
      data[!is.na(data[, v]) & (data[, v] <= 0), v] <- NA
    }
    
    plotWAC (data[, c("Time", va)], 
             logxy='y', ylim=c(1,1.e5), 
             ylab=expression (paste ("CONCy [cm"^"-3"*"]")))
    title ("1-min filter", cex.main=0.75)
    op <- par (mar=c(5,4,1,1)+0.1)
    C <- VRPlot[[15]]
    C <- C[which("CONC" == substr(C, 1, 4) & "CONCU" != substr(C,1,5))]
    va2 <- vector()
    for (c in C) {
      nm <- names(data)[grepl(c, names(data))]
      v <- sub("_.*", "", c)
      data[, v] <- SmoothInterp (data[, nm])
      va2 <- c(va2, v)
    }
    for (v in va2) {
      data[!is.na(data[, v]) & (data[, v] <= 0), v] <- NA
    }
    plotWAC (data[, c("Time", va2)],
             logxy='y', ylim=c(0.001,1e4), ylab=expression(paste("CONCy [cm"^"-3"*"]")))
    title ("1-min filter", cex.main=0.75)
    AddFooter ()
    if (!is.na(Seq) && (Seq == 1)) {return ()}
  }
  op <- par (mar=c(5,4,1,1)+0.1)
  nm6 <- names(data)[grepl("USHFLW_", names(data))]
  if (length(nm6) == 0) {return()}
  USHFLW <- data[, nm6]
  nm7 <- names(data)[grepl("USMPFLW_", names(data))]
  USMPFLW <- data[, nm7]
  nm8 <- names(data)[grepl("UREF_", names(data))]
  UREF <- data[, nm8]
  nm9 <- names(data)[grepl("USCAT_", names(data))]
  USCAT <- data[, nm9]
  USHF <- USHFLW/10
  plotWAC (data.frame(data$Time, USMPFLW, USHF),
           ylab="flows", legend.position='topright',
           ylim=c(0,2.5))
  hline (0.82, 'blue'); hline (1, 'darkgreen'); hline(0.5, 'red'); hline (1.5, 'red')
  legend ("topleft", legend=c("dashed red: limits for FCNC, XICNC, PFLWC", 
                              "dashed blue-green: expected values for corresponding flows"), text.col=c('red', 'blue'), cex=0.55)
  title ("USHF is USHFLW_RWOOU/10", cex.main=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data.frame (data$Time, UREF, USCAT), 
           ylab="laser V", legend.position='topright', ylim=c(0,10))
  hline (2.10, 'blue'); hline (1.90, 'darkgreen'); hline(6, 'red'); hline (9.95, 'red')
  title ("dashed-blue: lower limit for UREF; dashed-green: upper limit for USCAT", cex.main=0.65)
  AddFooter ()
}

