
## compile data from a specified project, all flights:
library(Ranadu)
VL <- standardVariables (c('ATH1', 'ATH2', 'ATH3', 'ATH4', 'AT_A', 'PS_A', 'QC_A', 'TAS_A', 'GGVSPD',
                           'RTX', 'PALT', 'ROLL', 'QCF', 'QCR'))
PJ <- 'HIPPO-5'
# PJ <- 'CSET'
# PJ <- 'CONTRAST'
Project <- PJ
if (grepl ('HIPPO', Project)) {
  ProjectDir <- 'HIPPO'
} else {
  ProjectDir <- Project
}
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

fname <- sprintf ('%s%s/%srf01.nc', DataDirectory (), ProjectDir, Project)
FI <- DataFileInfo (fname)
igg <- 0
if (!('GGVSPD' %in% FI$Variables)) {
  igg <- which(VL == 'GGVSPD')
  if ('VSPD_A' %in% FI$Variables) {
    VL[igg] <- 'VSPD_A'
  } else if ('VSPD_G' %in% FI$Variables) {
    VL[igg] <- 'VSPD_G'
  } else if ('GGVSPDB' %in% FI$Variables) {
    VL[igg] <- 'GGVSPDB'
  }
}


for (Project in PJ) {
  if (Project == 'CONTRAST') {
    VL[which(VL == 'ATH1')] <- 'ATHR1'
  }
  Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjectDir),
                          sprintf ("%srf...nc$", Project)))
  if (!is.na (Fl[1])) {
    for (Flt in Fl) {
      FltPP <- sub('.*rf', '', sub ('.nc$', '', Flt))
      FltPP <- as.integer (FltPP)
      fname <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
                        ProjectDir, Project, FltPP)
      print (fname)
      if (Project == 'CSET') {FltPP <- FltPP + 100}
      if (Project == 'ORCAS') {FltPP <- FltPP + 200}
      Data <- rbind (Data, getNetCDF (fname, VL, F=FltPP))  # qualifyAT(fname, VL, FltPP))
    }
  }
  if (igg != 0) {
    N <- names(Data)
    N[which(N == VL[igg])] <- 'GGVSPD'
    names (Data) <- N
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

Data <- DataALL
Data$ATX <- Data$ATH4

## function for minimization in 'optim':
fr <- function (x, DF) {
  rf <- 0.988 + with(DF, 
                     0.053 * log10(MACHX) + 0.090* (log10(MACHX))^2 
                     + 0.091 * (log10(MACHX))^3)
  At <- (x[1] + (1 + x[2]) * DF$RTX + x[3] * DF$RTX^2 + 273.15) / (1 + rf * DF$MACHX^2 * DF$Ra / (2 * DF$cv))
  dif <- with (DF, (Ra * At * DLP / Grav + DZ)^2)
  DF$Valid[abs(dif) > 20] <- FALSE
  ans <- sum (dif[DF$Valid], na.rm=TRUE) / length (dif[DF$Valid])
  print (c(x, ans))
  dif <<- dif
  return (ans)
}

# Rd <- 287.083
# Ra <- Rd/(1.+(0.6220-1.)*EW_VXL/PSFW)
# cv <- 717.71*Ra/Rd*(1+0.92926*EW_VXL/PSFW)
# Data$ATX <- ShiftInTime(Data$ATX, -2300)
Data$PSXC <- ShiftInTime(Data$PSXC, -1000)
CP <- SpecificHeats (Data$EWX / Data$PSXC)
Data$Grav <- Gravity(Data$LATC, Data$GGALT)
Data$Ra <- CP[, 3]
Data$cv <- CP[, 2]
LOGP <- log (Data$PSXC)
Data$DLP <- c(0, diff (LOGP))
Data$DZ <- c(0, diff (Data$GGALT))
with (Data, {
  Valid <- rep(TRUE, nrow(Data))
  Valid[is.na(PSXC)] <- FALSE
  Valid[is.na(GGALT)] <- FALSE
  Valid[is.na(RTX)] <- FALSE
  Valid[is.na(EWX)] <- FALSE
  Valid[is.na(TASX)] <- FALSE
  Valid[is.na(ROLL)] <- FALSE
  Valid[is.na(MACHX)] <- FALSE
  Valid[is.na(DLP)] <- FALSE
  # Valid[abs(DLP) < 0.0005] <- FALSE
  Valid[is.na(Grav)] <- FALSE
  Valid[is.na(Ra)] <- FALSE
  Valid[TASX < 130] <- FALSE
  Valid[abs(ROLL) > 5] <- FALSE
  Valid[abs(GGVSPD) < 2] <- FALSE
  Valid[abs(GGVSPD) > 7.5] <- FALSE
  Valid[ATX < -80] <- FALSE
  Valid[QCF-QCR > 2 | QCF-QCR < -2] <- NA
  Valid <<- Valid
})
## also remove the first minute of flight, when T sensor lagging
FF <- unique(Data$RF)
for (F in FF) {
  ixf <- which (Data$RF == F & Data$TASX > 130)[1]
  Valid[ixf:(ixf+60)] <- FALSE
}
Valid[is.na(Valid)] <- FALSE
Data$Valid <- Valid
pf <- c(0.1,0.01)
pf <- c(0.1, 0.01, -0.0005)
# result <- optim(pf, fr, gr=NULL, Data, control = list(reltol=1.e-7,parscale=c(1.,0.05, 0.0005)))
# print (result)

Data$PT <- with(Data, -Grav * DZ / (Ra * DLP)-273.15)
dif <- Data$PT - Data$ATX
Valid <- Data$Valid
Valid[abs(dif) > 25] <- FALSE
DBS <- with(Data[Valid, ], data.frame(PT-ATX, ATX))
bs <- binStats (DBS, bins=15)
# plotWAC(bs$ybar, bs$xc, xlab='DT')

bs$sigma[bs$nb > 1] <- bs$sigma[bs$nb > 1] / sqrt(bs$nb [bs$nb > 1])
g <- ggplot(data=bs)
g <- g + geom_errorbarh (aes (y=xc, x=ybar, xmin=ybar-sigma, 
                              xmax=ybar+sigma), na.rm=TRUE) 
xlow <- floor(min (bs$ybar-bs$sigma, na.rm=TRUE))
xhigh <- ceiling(max (bs$ybar+bs$sigma, na.rm=TRUE))
ymin <- min (bs$xc, na.rm=TRUE) - 5
ymax <- max (bs$xc, na.rm=TRUE) + 5
if (xhigh < 4) {xhigh <- 4}
if (xlow > -2) {xlow <- -2}
g <- g + xlim(xlow, xhigh) + ylim (ymax, ymin) + theme_WAC()
g <- g + xlab('ATHE-ATX [deg. C]') + ylab('ATX [deg. C]') 
g <- g + geom_point (aes (x=bs$ybar, y=bs$xc), size=3, colour='blue', na.rm=TRUE)
g <- g + geom_label (aes (x=3.9, y=bs$xc, label=sprintf('%d', bs$nb)))
print (g)
