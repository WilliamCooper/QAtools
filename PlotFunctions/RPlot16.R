### plot 16: DBAR (mean diameters) and PLWC (liquid water content)
# do 1-min smoothing; otherwise, too noisy
RPlot16 <- function (data, Seq=NA) {
  if (is.na (Seq) || Seq == 1) {
    if (any(grepl("DBAR1DC_", VRPlot[[16]]))) {
      layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
    } else {
      layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
    }
    op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
    DB <- VRPlot[[16]]
    DB <- DB[which(("DBARU" == substr(DB, 1, 5)) | ("DBARP" == substr(DB, 1, 5)))]
    va <- vector()
    for (c in DB) {
      nm <- names(data)[grepl(c, names(data))]
      v <- sub("_.*", "", c)
      data[, v] <- SmoothInterp(data[, nm])
      va <- c(va, v)
    }
    if (!(any(grepl("DBAR1DC_", VRPlot[[16]])))) {
      op <- par (mar=c(5,4,1,1)+0.1)
    }
    if (length (va) > 0) {
      plotWAC (data[, c("Time", va)], ylim=c(0,0.5), ylab="DBARU/P", 
               legend.position="topright")
    }
    title ("1-min filter", cex.main=0.75)
    DB <- VRPlot[[16]]
    DB <- DB[which (("DBAR" == substr(DB, 1, 4)) & ("DBARU" != substr(DB, 1, 5))
                    & ("DBARP" != substr(DB, 1, 5)))]
    va <- vector()
    for (c in DB) {
      nm <- names(data)[grepl(c, names(data))]
      v <- sub("_", "", c)
      data[, v] <- SmoothInterp(data[, nm])
      va <- c(va, v)
    }
    if (length (va) > 0) {
      plotWAC(data[, c("Time", va)], ylim=c(0,30), ylab="DBAR", legend.position="topright")
      title ("1-min filter", cex.main=0.75) 
    }
    op <- par (mar=c(5,4,1,1)+0.1)
    if (any(grepl("DBAR1DC_", VRPlot[[16]]))) {
      nm <- names(data)[grepl("DBAR1DC_", names(data))]
      plotWAC(data[, c("Time", nm)])
    }
    AddFooter()
    if (!is.na(Seq) && (Seq == 1)) {return()}
  }
  if (is.na(Seq) || Seq == 2) {
    layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
    ## Water measurements:
    op <- par (mar=c(2,4,1,1)+0.1)
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
    plotWAC (data[, c("Time", va3)], ylim=c(0,2), ylab="PLWCy", legend.position="topright")
    title ("1-min filter", cex.main=0.75)
    op <- par (mar=c(2,4,1,1)+0.1)
    if ("PLWC" %in% names(data) && "RICE" %in% names(data)) {
      plotWAC (data[, c("Time", "PLWC", "RICE")], ylim=c(0,25), ylab="PLWC (Watts)")
      hline (10); hline (15)
    }
    op <- par (mar=c(5,4,1,1)+0.1)
    if (any(grepl("PLWC1DC", LW))) {
      plotWAC(data[, c("Time", names(data)[which(grepl('PLWC1DC', names(data)))])])
    }
    AddFooter()
    if (!is.na(Seq) && (Seq == 2)) {return()}
  }
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  if (any (grepl ("TCNTD_", VRPlot[[16]])) && any (grepl ("REJDOF_", VRPlot[[16]]))) {
    TCNTD <- data[, names(data)[grepl("TCNTD_", names(data))]]
    REJDOF <- data[, names(data)[grepl("REJDOF_", names(data))]]
    DOFACC <- TCNTD / (TCNTD + REJDOF)
    DOFACC <- SmoothInterp (DOFACC)
    plotWAC (data.frame (data$Time, DOFACC), ylab="DOF acceptance fraction")
    hline (0.2, 'red')
  }
  if (any (grepl ("AVGTRNS_", names (data)))) {
    AVGTRNS <- data[, names(data)[grepl("AVGTRNS_", names(data))]]
    AVT <- SmoothInterp (AVGTRNS)
    plotWAC (data.frame (data$Time, AVT), ylim=c(0, 2))
  }
  op <- par (mar=c(5,4,1,1)+0.1)
  if (any(grepl ("CDPLSRP_", names (data)))) {
    CDPLSRP <- data[, names(data)[grepl("CDPLSRP_", names(data))]]
    op <- par (mar=c(5,4,1,1)+0.1)
    plotWAC (data.frame(data$Time, CDPLSRP), ylab="CDP laser power", ylim=c(0,4))
  }
  AddFooter ()
  hline(3, 'red')
}

