### plot 21: UHSAS size distributions
RPlot21 <- function (data, Seq=NA) {
  ## needs CUHSAS_RWOOU, CPCASP_RWOOP; references fname from calling environment
  if (is.na (VRPlot$PV21) || (length(VRPlot$PV21) < 1)) {return ()}
  kount = 0
  plotTest <- 50
  AddPCASP <- FALSE
  if (!is.na (fname) && file.exists (fname)) {
    netCDFfile <- nc_open (fname)
    namesCDF <- names (netCDFfile$var)
    V <- VRPlot$PV21[1]
    if (substr(V, nchar(V), nchar(V)) == '_') {
      nm1 <- namesCDF[grepl(VRPlot$PV21[1], namesCDF)]
    } else {
      nm1 <- V
    }
    if (any (grepl('CS200', namesCDF))) {
      AddPCASP <- TRUE
      V <- VRPlot$PV21[2]
      if (substr(V, nchar(V), nchar(V)) == '_') {
        nm2 <- namesCDF[grepl(VRPlot$PV21[2], namesCDF)]
      } else {
        nm2 <- V
      }
    }
    if (length (nm1) > 1) {nm1 <- nm1[1]}  ## multiple (e.g., for CVI): choose 1st
    Time <- ncvar_get (netCDFfile, "Time")
    TASX <- ncvar_get (netCDFfile, "TASX")
    CUHSAS <- ncvar_get (netCDFfile, nm1)
    if (AddPCASP) {CPCASP <- ncvar_get (netCDFfile, nm2)}
    time_units <- ncatt_get (netCDFfile, "Time", "units")
    tref <- sub ('seconds since ', '', time_units$value)
    Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
    CellSizes <- ncatt_get (netCDFfile, nm1, "CellSizes")
    CellLimitsU <- CellSizes$value
    if (AddPCASP) {
      CellSizes <- ncatt_get (netCDFfile, nm2, "CellSizes")
      CellLimitsP <- CellSizes$value
    }
  } else {
    fn <- sub ('\\.nc', '.Rdata', fname)
    if (file.exists (fn)) {
      load (file=fn)
      Time <- size.distributions$Time
      TASX <- size.distributions$TASX
      if ('CUHSAS' %in% names (size.distributions)) {
        CUHSAS <- size.distributions$CUHSAS
        CellLimitsU <- attr (CUHSAS, 'CellLimits')
      }
      if ('CPCASP' %in% names (size.distributions)) {
        AddPCASP <- TRUE
        CPCASP <- size.distributions$CPCASP
        CellLimitsP <- attr (CPCASP, 'CellLimits')
      }
    }
  }
  layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(5,5,6))
  op <- par (mar=c(2,2,1,1)+0.1,oma=c(1.1,0,0,0))
  idx1 <- getIndex (Time, StartTime)
  if (idx1 < 1) {idx1 <- 1}
  ## reference to calling environment for StartTime
  jstart <- ifelse (StartTime > 0, idx1, 1)
  # print (sprintf ("start time in RPlot21 is %d and jstart is %d\n",
  #                 StartTime, jstart))
  for (j in jstart:length(Time)) {
    if (is.na(Time[j])) {next}
    if (!is.na(TASX[j]) && (TASX[j] < 90)) {next}
    UHSAS <- CUHSAS[, j]
    if (AddPCASP) {PCASP <- CPCASP[, j]}
    ## convert distributions to number per cm per um
    if (any(!is.na(UHSAS))) {
      for (m in 2:length(UHSAS)) {
        UHSAS[m] <- UHSAS[m] / (CellLimitsU[m] - CellLimitsU[m-1])
      }
    }
    if (AddPCASP) {
      if (any (!is.na(PCASP))) {
        for (m in 2:length(PCASP)) {
          PCASP[m] <- PCASP[m] / (CellLimitsP[m] - CellLimitsP[m-1])
        }
      }
    }
    
    UHSAS[UHSAS <= 0] <- 1e-4
    if (AddPCASP) {PCASP[PCASP <= 0] <- 1e-4}
    if ((any(UHSAS > plotTest, na.rm=TRUE)) || (AddPCASP && any(PCASP > plotTest, na.rm=TRUE)) ) {
      kount <- kount + 1
      if (is.na (Seq) || (!is.na(Seq) && (kount > (Seq-1)*6))) {
        ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
                op <- par (mar=c(5.2,2,1,1)+0.1))
        if (any (!is.na(UHSAS))) {
          plot (CellLimitsU, UHSAS, type='s', ylim=c(1,1.e6),
                xlab="Diameter [um]", log="y", col='blue', lwd=2)
        } 
        if (AddPCASP && any (!is.na (PCASP))) {
          if (any (!is.na(UHSAS))) {
            points (CellLimitsP, PCASP, type='s', col='darkgreen')
          } else {
            plot (CellLimitsP, PCASP, type='s', ylim=c(1,1.e6),
                  xlab='Diameter [um]', log='y', col='darkgreen', lwd=2)
          }
        }
        title(sprintf("size distribution, Time=%s", strftime (Time[j], format="%H:%M:%S", tz='UTC')), 
              cex.main=.75)
        legend ("topright", legend=c("UHSAS", "PCASP"), col=c('blue', 'darkgreen'), 
                lwd=c(2,1), cex=0.75) 
        if (kount%%6==0)   AddFooter ()
      }
    }
    if (!is.na(Seq) && (kount >= (Seq*6))) {break}
    if (kount >= 24) {break}
  }
  if (is.na(Seq) || (Seq == 4)) {
    if (!is.na (fname) && file.exists (fname)) {Z <- nc_close (netCDFfile)}
  }
}

