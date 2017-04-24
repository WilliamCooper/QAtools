### plot 22: 2DC size distributions
RPlot22 <- function (data, Seq=NA) {
  ## needs C1DC_LWOI; references fname from calling environment
  if (is.na (VRPlot$PV22) || (length(VRPlot$PV22) < 1)) {return ()}
  kount = 0
  if (!is.na (fname) && file.exists (fname)) {
    netCDFfile <- nc_open (fname)
    namesCDF <- names (netCDFfile$var)
    nm1 <- namesCDF[grepl("C1DC_", namesCDF)][1]
    C1DC <- ncvar_get (netCDFfile, nm1)
    Time <- ncvar_get (netCDFfile, "Time")
    TASX <- ncvar_get (netCDFfile, "TASX")
    time_units <- ncatt_get (netCDFfile, "Time", "units")
    tref <- sub ('seconds since ', '', time_units$value)
    Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
    CellSizes <- ncatt_get (netCDFfile, nm1, "CellSizes")
    CellLimits <- CellSizes$value
  } else {
    fn <- sub ('\\.nc', '.Rdata', fname)
    if (file.exists (fn)) {
      load (file=fn)
      Time <- size.distributions$Time
      TASX <- size.distributions$TASX
      if ('C1DC' %in% names (size.distributions)) {
        C1DC <- size.distributions$C1DC
        CellLimits <- attr (C1DC, 'CellLimits')
      }
    }
  }
  layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(5,5,6))
  idx1 <- getIndex (Time, StartTime)
  if (idx1 < 1) {idx1 <- 1}
  ## reference to calling environment for StartTime
  jstart <- ifelse (StartTime > 0, idx1, 1)
  # print (sprintf ("start time in RPlot23 is %d and jstart is %d\n",
  #                 StartTime, jstart))
  op <- par (mar=c(2,2,1,1)+0.1,oma=c(1.1,0,0,0))
  for (j in jstart:length(Time)) {
    if (is.na(Time[j])) {next}
    if (!is.na(TASX[j]) && (TASX[j] < 110)) {next}
    S1DC <- C1DC[,j]
    ## convert distributions to number per L per um
    for (m in 2:length(S1DC)) {
      S1DC[m] <- S1DC[m] / (CellLimits[m] - CellLimits[m-1])
    }
    S1DC[S1DC <= 0] <- 1e-4
    if ((any(S1DC > 0.1, na.rm=TRUE))) {
      kount <- kount + 1
      if (is.na (Seq) || (!is.na(Seq) && (kount > (Seq-1)*6))) {
        ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
                op <- par (mar=c(5.2,2,1,1)+0.1))
        plot (CellLimits, S1DC, type='s', ylim=c(1.e-2,1.e2), 
              xlab="Diameter [um]", log="y", col='blue', lwd=2)
        title(sprintf("Time=%s", strftime (Time[j], format="%H:%M:%S", tz='UTC')), 
              cex.main=.75)
        legend ("topright", legend=c("1DC"), col='blue', 
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

