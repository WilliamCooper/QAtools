## construct multi-project all-flight data file

library(Ranadu)
VL <- standardVariables (c('ATH1', 'AT_A', 'PS_A', 'QC_A', 'TAS_A', 'GGVSPD'))
PJ <- c('CONTRAST', 'CSET', 'ORCAS')
Data <- data.frame()

qualifyAT <- function(fnPP, Vars, Flt) {
  suppressMessages (suppressWarnings (
    FI <- DataFileInfo (fnPP)
  ))
  if ('GGVSPD' %in% FI$Variables) {
    GVSPD <- 'GGVSPD'
  } else if ('GGVSPDB' %in% FI$Variables) {
    GVSPD <- 'GGVSPDB'
    Vars[which('GGVSPD' == Vars)] <- 'GGVSPDB'
  } else if ('VSPD_A' %in% FI$Variables) {
    GVSPD <- 'VSPD_A'
    Vars[which('GGVSPD' == Vars)] <- 'VSPD_A'
  } else if ('VSPD_G' %in% FI$Variables) {
    GVSPD <- 'VSPD_G'
    Vars[which('GGVSPD' == Vars)] <- 'VSPD_G'
  }
  D <- getNetCDF(fnPP, Vars, F=Flt)
  D$DTAS <- c(0, diff(D$TASX))
  D$DTAS <- SmoothInterp(D$DTAS, .Length=301)
  r <- (abs(D[, GVSPD]) > 1) | (D$TASX < 90) | (abs(D$DTAS) > 0.1) | (D$QCXC < 60)
  D$PSXC[r] <- NA; D$PS_A[r] <- NA
  D$QCRC[r] <- NA; D$QC_A[r] <- NA
  D$AT_A[r] <- NA
  return (D)
}

for (Project in PJ) {
  if (Project == 'CONTRAST') {
    VL[which(VL == 'ATH1')] <- 'ATHR1'
  }
  Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), Project),
                        sprintf ("%srf...nc$", Project)))
  if (!is.na (Fl[1])) {
    for (Flt in Fl) {
      FltPP <- sub('.*rf', '', sub ('.nc$', '', Flt))
      FltPP <- as.integer (FltPP)
      fname <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
                          Project, Project, FltPP)
      print (fname)
      if (Project == 'CSET') {FltPP <- FltPP + 100}
      if (Project == 'ORCAS') {FltPP <- FltPP + 200}
      Data <- rbind (Data, qualifyAT(fname, VL, FltPP))
    }
  }
  if (Project == 'CONTRAST') {
    VL[which(VL == 'ATHR1')] <- 'ATH1'
    VL[which(VL == 'GGVSPDB')] <- 'GGVSPD'
    N <- names(Data)
    N[which(N == 'ATHR1')] <- 'ATH1'
    N[which(N == 'GGVSPDB')] <- 'GGVSPD'
    names(Data) <- N
  }
}

DataALL <- Data  
c(0.28178716560, 1.01636832046, -0.00012560891)  ## const, AT_A, AT_A^2

