loadVRPlot <- function (Project, Production = FALSE, Flight, psq) {
  source ('Configuration.R')
  # print (sprintf ('in loadVRPlot, Project=%s', Project))
  # print (VRPlot)
  # print (str(VRPlot))
  ## this leaves VRPlot defined
  if (length(VRPlot) < 30) {
    nm <- names (VRPlot)
    for (i in (length(VRPlot)+1):30) {
      VRPlot[[i]] <- c('TASX', 'ATX')
      nm[i] <- sprintf ('PV%d', i)
    }
    names(VRPlot) <- nm
  }
  if (grepl ('HIPPO', Project)) {
    if (grepl ('raf_data', DataDir)) {
      ProjectDir <- 'HIPPO/old_nimbus'
    } else {
      ProjectDir <- 'HIPPO'
    }
  } else {
    ProjectDir <- Project
  }
  
  if (Project != 'PREDICT') {
    fn <- sprintf ('%s%s/%srf%02d.nc', DataDirectory (), ProjectDir, Project, Flight)
  } else {
    fn <- sprintf ('%s%s/%srf%02dHW.nc', DataDirectory (), ProjectDir, Project, Flight)
  }
  if (!file.exists (fn)) {
    if (Trace) {print (sprintf ('%s not found', fn))}
    fn <- sub ('\\.nc', '.Rdata', fn)
  }
  if (!file.exists (fn)) {
    if (Trace) {print (sprintf ('%s not found', fn))}
    fn <- sprintf ('%s%s/%stf01.nc', DataDirectory (), Project, Project)
  }
  if (!file.exists (fn)) {
    if (Trace) {print (sprintf ('%s not found', fn))}
    fn <- sub ('\\.nc', '.Rdata', fn)
  }
  ## if Production load production-file info
  if (Production) {
    print (sprintf ('production section in global, Production=%d',
      Production))
    dr <- sprintf ('%s../raf/Prod_Data/%s', DataDirectory (), Project)
    scmd <- sprintf ('ls -lt `/bin/find %s -ipath "\\./movies" -prune -o -ipath "\\./*image*" -prune -o -name %s%s%02d.Rdata`',
      dr, Project, 'rf', Flight)
    fl <- system (scmd, intern=TRUE)[1]
    if ((length (fl) > 0) && (!grepl ('total', fl))) {
      fn <- sub ('.* /', '/', fl[1])
    }
  }
  
  if (!file.exists (fn)) {
    if (Trace) {print (sprintf ('%s not found', fn))}
    warning ('need tf01 or rf01 to initialize')
    return (VRPlot)
  }
  # if (Trace) {print (sprintf ('fnE=%s', fn))}
  # print (sprintf ('setting chp/slp from %s', fn))
  ## see if info for this file is already available:
  if (!exists ('FI') || FI$Project != Project) {
    FI <<- DataFileInfo (fn, LLrange=FALSE)
  }
  
  VLALL <<- FI$Variables
  LAT <- FI$Variables[grepl ('^LAT', FI$Variables)]
  LON <- FI$Variables[grepl ('^LON', FI$Variables)]
  ALT <- FI$Variables[grepl ('ALT', FI$Variables)]
  WD <- FI$Variables[grepl ('WD', FI$Variables)]
  WS <- FI$Variables[grepl ('WS', FI$Variables) & !grepl ('FLOW', FI$Variables)]
  AT <- FI$Variables[grepl ('^AT', FI$Variables) & !grepl ('ATTACK', FI$Variables)]
  DP <- FI$Variables[grepl ('^DP', FI$Variables)]
  EWW <- FI$Variables[grepl ('^EW', FI$Variables)]
  CAVP <- FI$Variables[grepl ('CAVP', FI$Variables)]
  PS <- FI$Variables[grepl ('^PS', FI$Variables)]
  QC <- FI$Variables[grepl ('^QC', FI$Variables) & !grepl ('TEMP', FI$Variables)]
  TAS <- FI$Variables[grepl ('TAS', FI$Variables)]
  MACH <- FI$Variables[grepl ('MACH', FI$Variables)]
  EW <- FI$Variables[grepl ('VEW', FI$Variables)]
  NS <- FI$Variables[grepl ('VNS', FI$Variables)]
  PITCH <- FI$Variables[grepl ('PITCH', FI$Variables)]
  ROLL <- FI$Variables[grepl ('ROLL', FI$Variables)]
  THDG <- FI$Variables[grepl ('THDG', FI$Variables)]
  ACINS <- FI$Variables[grepl ('ACINS', FI$Variables)]
  VSPD <- FI$Variables[grepl ('VSPD', FI$Variables)]
  RAD <- FI$Variables[grepl ('^RS', FI$Variables) | grepl ('VISB', FI$Variables)
    | (grepl ('^IR', FI$Variables) & !grepl ('IRIG', FI$Variables))]
  CONC <- FI$Variables[grepl ('^CONC', FI$Variables)]
  DBAR <- FI$Variables[grepl ('DBAR', FI$Variables)]
  LWC <- FI$Variables[grepl ('LWC', FI$Variables) & !grepl ('UFLWC', FI$Variables)]
  if ('RICE' %in% FI$Variables) {LWC <- c(LWC, 'RICE')}
  THETA <- FI$Variables[grepl ('THETA', FI$Variables)]
  im <- pmatch (c ("TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_"),
    FI$Variables)
  im <- im[!is.na(im)]
  HSKP <- FI$Variables[im]
  im <- match(c("CORAW_AL", "FO3_ACD", "COFLOW_AL", "INLETP_AL"),
    FI$Variables)
  im <- im[!is.na(im)]
  CHEM <- FI$Variables[im]
  im <- pmatch (c ("USHFLW_", "USMPFLW_", "UREF_", "USCAT_"),
    FI$Variables)
  im <- im[!is.na(im)]
  USH <- FI$Variables[im]
  chp <- list ()
  slp <- list ()
  chp[[1]] <- c(LAT,LON,WD,WS)
  chp[[2]] <- c(ALT,'PSXC')
  chp[[3]] <- AT
  chp[[4]] <- chp[[3]]
  chp[[5]] <- c(DP,'ATX',CAVP,EWW)
  chp[[6]] <- chp[[5]]
  chp[[7]] <- chp[[5]]
  chp[[8]] <- chp[[5]]
  chp[[9]] <- PS
  chp[[10]] <- c(QC,TAS,MACH)
  chp[[11]] <- chp[[10]]
  chp[[12]] <- c(PS,QC,'AKRD')
  chp[[13]] <- c(WD,WS,'WIC')
  chp[[14]] <- chp[[13]]
  chp[[15]] <- c(EW,NS,'GGQUAL')
  chp[[16]] <- chp[[15]]
  chp[[17]] <- VRPlot[[11]]
  chp[[18]] <- c(PITCH,ROLL,THDG)
  chp[[19]] <- c(ACINS,VSPD,ALT)
  chp[[20]] <- RAD
  chp[[21]] <- c(CONC)
  if (length (USH) > 0) {chp[[21]] <- c(CONC, USH)}
  chp[[22]] <- chp[[21]]
  chp[[23]] <- c(DBAR,LWC,HSKP)
  chp[[24]] <- chp[[23]]
  chp[[25]] <- chp[[23]]
  chp[[26]] <- c('PSXC','ATX', 'DPXC',PS,AT,DP)
  chp[[27]] <- THETA
  chp[[28]] <- THETA
  chp[[29]] <- FI$Variables[grepl ('CCDP_', FI$Variables) 
    | grepl ('CS100_', FI$Variables)]
  chp[[30]] <- chp[[29]]
  chp[[31]] <- chp[[29]]
  chp[[32]] <- chp[[29]]
  chp[[33]] <- FI$Variables[grepl ("CUHSAS_", FI$Variables)]
  chp[[33]] <- c(chp[[33]], FI$Variables[grepl ('CS200_', FI$Variables)])
  chp[[34]] <- chp[[33]]
  chp[[35]] <- chp[[33]]
  chp[[36]] <- chp[[33]]
  chp[[37]] <- FI$Variables[grepl ('C1DC_', FI$Variables)]
  chp[[38]] <- chp[[37]]
  chp[[39]] <- chp[[37]]
  chp[[40]] <- chp[[37]]
  chp[[41]] <- CHEM
  chp[[42]] <- CHEM
  chp[[43]] <- sort(FI$Variables)
  chp[[44]] <- sort(FI$Variables)
  chp[[45]] <- sort(FI$Variables)
  chp[[46]] <- c('TASX', 'ATX')
  chp[[47]] <- c('TASX', 'ATX')
  chp[[48]] <- c('TASX', 'ATX')
  chp[[49]] <- c('TASX', 'ATX')
  chp <<- chp
  for (i in c(1:20,26:28,41:49)) {
    j <- psq[1, i]
    sl <- as.vector(chp[[i]])
    slp[[i]] <- sl[sl %in% VRPlot[[j]]]
  }
  for (i in c(21:25,29:40)) {
    j <- psq[1, i]
    sl <- as.vector(chp[[i]])
    k <- pmatch (VRPlot[[j]], sl)
    sl <- sl[k]
    sl <- sl[!is.na(sl)]
    slp[[i]] <- sl
  }
  slp <<- slp
  PVar <<- slp[[1]]
  return (VRPlot)
}
