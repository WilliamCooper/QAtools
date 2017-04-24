### plot 20: CDP/SP100 size distributions
RPlot20 <- function (data, Seq=NA) {
  if (is.na (VRPlot$PV20) || (length(VRPlot$PV20) < 1)) {return ()}
  ## needs CCDP_LWOI; references fname from calling environment
  kount = 0
  if (!is.na (fname) && file.exists (fname)) {
    if (is.null (netCDFfile) || is.na (netCDFfile)) {
      netCDFfile <- nc_open (fname)
    }
    namesCDF <- names (netCDFfile$var)
    if (length (grep ("CCDP_", VRPlot[[20]])) > 0) {
      nm1 <- namesCDF[grepl("CCDP_", namesCDF)]
      if (is.null (CCDP) || (is.na (CCDP))) {
        CCDP <- ncvar_get (netCDFfile, nm1)
        CellSizes <- ncatt_get (netCDFfile, nm1, "CellSizes")
        CellLimitsD <- CellSizes$value
        Time <- ncvar_get (netCDFfile, "Time")
        TASX <<- TASX <- ncvar_get (netCDFfile, "TASX")
        time_units <- ncatt_get (netCDFfile, "Time", "units")
        tref <<- sub ('seconds since ', '', time_units$value)
        Time <<- Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
        attr (CCDP, 'CellLimits') <- CellLimitsD
      } else {
        CellLimitsD <- attr (CCDP, 'CellLimits')
      }
    }
    if (length (grep ("CS100_", VRPlot[[20]])) > 0) {
      nm1 <- namesCDF[grepl("CS100_", namesCDF)]
      if (is.null (CFSSP)) {
        CFSSP <- ncvar_get (netCDFfile, nm1)
        CellSizes <- ncatt_get (netCDFfile, nm1, "CellSizes")
        CellLimitsF <- CellSizes$value
        if (is.null (Time)) {
          Time <- ncvar_get (netCDFfile, "Time")
          TASX <<- TASX <- ncvar_get (netCDFfile, "TASX")
          time_units <- ncatt_get (netCDFfile, "Time", "units")
          tref <<- sub ('seconds since ', '', time_units$value)
          Time <<- Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
        }
        attr (CFSSP, 'CellLimits') <- CellLimitsF
      }
    }
    
  } else {
    fn <- sub ('\\.nc', '.Rdata', fname)
    if (file.exists (fn)) {
      load (file=fn)
      Time <- size.distributions$Time
      TASX <- size.distributions$TASX
      if ('CCDP' %in% names (size.distributions)) {
        CCDP <- size.distributions$CCDP
        CellLimitsD <- attr (CCDP, 'CellLimits')
      }
      if ('CFSSP' %in% names (size.distributions)) {
        CFSSP <- size.distributions$CFSSP
        CellLimitsF <- attr (CFSSP, 'CellLimits')
      }
    }
  }
  layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(5,5,6))
  idx1 <- getIndex (Time, StartTime)
  if (idx1 < 1) {idx1 <- 1}
  ## reference to calling environment for StartTime
  jstart <- ifelse (StartTime > 0, idx1, 1)
  # print (sprintf ("start time in RPlot20 is %d and jstart is %d\n",
  #                 StartTime, jstart))
  op <- par (mar=c(2,2,1,1)+0.1,oma=c(1.1,0,0,0))
  for (j in jstart:length(Time)) {
    if (is.na(Time[j])) {next}
    if (!is.na(TASX[j]) && (TASX[j] < 90)) {next}
    if (exists("CCDP")) {CDP <- CCDP[,j]}
    if (exists("CFSSP")) {FSSP <- CFSSP[,j]}
    ## convert distributions to number per cm per um
    if (exists ("CDP")) {
      CDPtot <- 0
      for (m in 2:length(CDP)) {
        CDP[m] <- CDP[m] / (CellLimitsD[m] - CellLimitsD[m-1])
        CDPtot <- CDPtot + CDP[m]
      }
      CDP[CDP <= 0] <- 1e-4
      if ((any(CDP > 1, na.rm=TRUE))) {
        kount <- kount + 1
        if (is.na (Seq) || (!is.na(Seq) && (kount > (Seq-1)*6))) {
          ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
                  op <- par (mar=c(5.2,2,1,1)+0.1))
          plot (CellLimitsD, CDP, type='s', ylim=c(1.e-1,1.e3), 
                xlab="Diameter [um]", log="y", col='blue', lwd=2)
          title(sprintf("Time=%s CONCD=%.1f", strftime (Time[j], format="%H:%M:%S", tz='UTC'), CDPtot), 
                cex.main=1)
          legend ("topright", legend=c("CDP"), col='blue', 
                  lwd=c(2,1), cex=0.75) 
          if (kount%%6==0)   AddFooter ()
        }
      }
    }
    if (!is.na(Seq) && (kount >= (Seq*6))) {break}
    if (kount >= 24) {break}
  }
  if (is.na(Seq) || (Seq == 4)) {
    if (!is.na (fname) && file.exists (fname)) {Z <- nc_close (netCDFfile)}
    netCDFfile <<- NULL
    CCDP <<- NULL
    CFSSP <<- NULL
  } else {
    netCDFfile <<- netCDFfile
    if (exists ('CCDP')) {CCDP <<- CCDP}
    if (exists ('CFSSP')) {CFFSP <<- CFSSP}
  }
}

