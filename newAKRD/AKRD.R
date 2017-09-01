## ----initialization, include=FALSE---------------------------------------

# require(knitr)
# opts_chunk$set(echo=FALSE, include=FALSE, fig.lp="fig:", dev='png', dpi=100)
# opts_chunk$set(fig.width=6, fig.height=5, fig.pos="center", digits=4)
# options(digits=5)
thisFileName <- "AKRD"
require(Ranadu, quietly = TRUE, warn.conflicts=FALSE)
require(ggplot2)
require(grid)
require(ggthemes)
cffn <- 19.70547
cff <- 21.481
cfs <- c(4.525341674, 19.933222011, -0.001960992)
CutoffFreq <- 600

getNext <- function(ProjectDir, Project) {
  FlY <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjectDir),
    sprintf ("%srf..Y.nc", Project)))
  Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjectDir),
    sprintf ("%srf...nc", Project)))
  if (is.na (FlY[1])) {
    Flight <- 1
  } else {
    FlZ <- sub ('Y.nc', '.nc', FlY)
    iw <- which (!(Fl %in% FlZ))[1]
    if (!is.na (iw) && length(iw) > 0) {
      Flight <- sub (".*rf", '',  sub (".nc", '', Fl[iw]))
      Flight <- as.numeric(Flight)
    } else {
      print ('** No more files to process **')
      Flight <- 0
    }
  }
  return (Flight)
}

# function to copy attributes from old variable (e.g., WIC) to new one (e.g., WIF)
copy_attributes <- function (atv, v, nfile) {
  for (i in 1:length(atv)) {
    aname <- names(atv[i])
    if (grepl ('name', aname)) {next}  # skips long and standard names
    if (grepl ('units', aname)) {next}
    if (grepl ('Dependencies', aname)) {next}
    if (grepl ('actual_range', aname)) {next}
    if (is.numeric (atv[[i]])) {
      ncatt_put (nfile, v, attname=aname, attval=as.numeric(atv[[i]]))
    } else {
      ncatt_put (nfile, v, attname=aname, attval=as.character (atv[[i]]))
    }
  }
}

## get run arguments: Project, Flight, addWIF, addWIY
## If Flight == 'Next' process next unprocessed flight in project
## If Flight == "All" process all unprocessed flights in project
run.args <- commandArgs (TRUE)
if (length (run.args) > 1) {
  Project <- run.args[1]
  ProjectDir <- Project
  if (grepl ('HIPPO', Project)) {
    ProjectDir <- 'HIPPO'
  }
} else {
  print ("Usage: Rscript AKRD.R Project Flight")
  print ("Example: Rscript AKRD.R DEEPWAVE 12")
  stop ("exiting...")
}
if (run.args[2] == 'NEXT') {
  Flight <- getNext (ProjectDir, Project)
  if (Flight == 0) {stop ("no more files to process")}
} else if (run.args[2] != 'All') {
  Flight <- as.numeric (run.args[2])
} else { 
  Flight <- 'All'
}
# if (length(run.args > 2)) {
#   addWIF <- run.args[3]
# } else {
#   addWIF <- FALSE
# }
# if (length(run.args > 3)) {
#   addWIY <- run.args[4]
# } else {
#   addWIY <- FALSE
# }
# 
SummarizeFit <- function(ft) {
  print (summary(ft)$call)
  print ("Coefficients:")
  print (summary(ft)$coefficients)
  print (sprintf ("Residual standard deviation: %.3f, dof=%d", summary(ft)$sigma, summary(ft)$df[2]))
  print (sprintf ("R-squared %.3f", summary(ft)$r.squared))
}

Directory <- DataDirectory ()	
if (Flight == 'All') {
  files.ALL <- c(list.files (sprintf ('%s%s', Directory, ProjectDir), sprintf ('%srf...nc$', Project)))
  ## eliminate those already processed
  files.A <- files.ALL
  for (f in files.A) {
    ftest <- sub ('.nc', 'Y.nc', sprintf ('%s%s/%s', Directory, ProjectDir, f))
    if (file.exists (ftest)) {
      files.ALL <- files.ALL[-which(files.ALL == f)]
    }
  }
} else {
  files.ALL <- sprintf ('%srf%02d.nc', Project, Flight)
}		
VarList <- c("ADIFR", "GGVSPD", "PITCH", "QCF", "PSF", "AKRD", "WIC", "TASF", "GGALT", "ROLL", "PSXC", "ATX", "QCXC")
## add variables needed to recalculate wind
VarList <- c(VarList, "TASX", "ATTACK", "SSLIP", "GGVEW", "GGVNS", "VEW", "VNS", "THDG")
for (file.name in files.ALL) {
  if (file.name == 'ORCASrf12.nc') {next}  ## this file is bad
  fname <- sprintf ("%s%s/%s", Directory, Project, file.name)
  new.name <- sub ('.nc', 'Y.nc', file.name)
  fnew <- sprintf ('%s%s/%s', Directory, Project, new.name)
  ## beware: overwrites without warning!!
  Z <- file.copy (fname, fnew, overwrite=TRUE)  ## BEWARE: overwrites without warning!!
  
  
  
  ## ----modify-netCDF-file, include=TRUE, eval=FALSE------------------------
  
  print (sprintf (' processing file %s', file.name))
  FI <- DataFileInfo (fname)
  if ('GGVSPD' %in% FI$Variables) {
  } else if ('GGVSPDB' %in% FI$Variables) {
    VarList [which (VarList == 'GGVSPD')] <- 'GGVSPDB'
  } else if ('VSPD_A' %in% FI$Variables) {
    VarList [which (VarList == 'GGVSPD')] <- 'VSPD_A'
  } else if ('VSPD_G' %in% FI$Variables) {
    VarList [which (VarList == 'GGVSPD')] <- 'VSPD_G'
  } else {
    print ('ERROR: no VSPD variable found')
    exit()
  }
  next.flag <- FALSE
  for (Var in VarList) {
    if (!(Var %in% FI$Variables)) {
      print (sprintf (' required variable %s not found in file %s; skipping...', Var, fname))
      next.flag <- TRUE
      break
    }
  }
  if (next.flag) {next}
  print (sprintf ('flight is %s', file.name))
  D <- getNetCDF (fname, VarList)
  netCDFfile <- nc_open (fnew, write=TRUE)
  Rate <- 1
  Dimensions <- attr (D, "Dimensions")
  Dim <- Dimensions[["Time"]]
  if ("sps25" %in% names (Dimensions)) {
    Rate <- 25
    Dim <- list(Dimensions[["sps25"]], Dimensions[["Time"]])
  }
  if ("sps50" %in% names (Dimensions)) {
    Rate <- 50
    Dim <- list(Dimensions[["sps50"]], Dimensions[["Time"]])
  }
  # CutoffFreq <- 600 * Rate
  D$QR <- D$ADIFR / D$QCF
  D$QR[D$QCF < 20] <- NA
  D$QR[is.infinite(D$QR)] <- NA
  if ('GGVSPD' %in% VarList) {
    D$AOAREF <- (D$PITCH - (D$GGVSPD / D$TASF) * (180 / pi))
  } else if ('GGVSPDB' %in% VarList) {
    D$AOAREF <- (D$PITCH - (D$GGVSPDB / D$TASF) * (180 / pi))
  } else if ('VSPD_A' %in% VarList) {
    D$AOAREF <- (D$PITCH - (D$VSPD_A / D$TASF) * (180 / pi))
  } else if ('VSPD_G' %in% VarList) {
    D$AOAREF <- (D$PITCH - (D$VSPD_G / D$TASF) * (180 / pi))
  }
  print ('calculate new variables')
  D$M <- MachNumber (D$PSF, D$QCF)
  D$WICS <- SmoothInterp (D$WIC)
  # CutoffFreq <- 600 * Rate
  D$AOAREFS <- zoo::na.approx (as.vector(D$AOAREF), maxgap=1000*Rate, na.rm = FALSE)
  D$AOAREFS[is.na(D$AOAREFS)] <- 0
  D$AOAREFS2 <- signal::filtfilt (signal::butter (3, 2/(CutoffFreq*Rate)), D$AOAREFS)
  D$AOAREFS1 <- signal::filter (signal::butter (3, 2/(CutoffFreq*Rate)), D$AOAREFS)
  D$AOAREFF <-  D$AOAREF - D$AOAREFS2
  D$AOAREFF1 <-  D$AOAREF - D$AOAREFS1
  D$QRS <- zoo::na.approx (as.vector(D$QR), maxgap=1000*Rate, na.rm = FALSE)
  D$QRS[is.na(D$QRS)] <- 0
  D$QRS2 <- signal::filtfilt (signal::butter (3, 2/(CutoffFreq*Rate)), D$QRS)
  D$QRS1 <- signal::filter (signal::butter (3, 2/(CutoffFreq*Rate)), D$QRS)
  D$QRS <- D$QRS2
  D$QRF <-  D$QR - D$QRS2
  D$QRF1 <- D$QR - D$QRS1
  D$QCFS <- zoo::na.approx (as.vector(D$QCF), maxgap=1000*Rate, na.rm = FALSE)
  D$QCFS[is.na(D$QCFS)] <- 0
  D$QCFS2 <- signal::filtfilt (signal::butter (3, 2/(CutoffFreq*Rate)), D$QCFS)
  D$QCFS1 <- signal::filter (signal::butter (3, 2/(CutoffFreq*Rate)), D$QCFS)
  D$QCFS <- D$QCFS2
  D$MS <- zoo::na.approx (as.vector(D$M), maxgap=1000*Rate, na.rm = FALSE)
  D$MS[is.na(D$MS)] <- 0
  D$MS <- signal::filtfilt (signal::butter (3, 2/(CutoffFreq*Rate)), D$MS)
  D$MF <-  D$M - D$MS
  D$AK <- cff * D$QRF + cfs[1] + cfs[2] * D$QRS + cfs[3] * D$QCFS
  D$AK1 <- cff * D$QRF1 + cfs[1] + cfs[2] * D$QRS1 + cfs[3] * D$QCFS1
  DataW <- D
  DataW$ATTACK <- D$AK
  DataW <- Ranadu::WindProcessor (DataW)
  D$WIY <- DataW$WIN
  DataW <- D
  DataW$ATTACK <- D$AK1
  DataW <- Ranadu::WindProcessor (DataW)
  D$WIS <- DataW$WIN
  d <- zoo::na.approx (as.vector(D$WIY), maxgap=100*Rate, na.rm = FALSE)
  d[is.na(d)] <- 0
  D$WIF <- D$WIY - signal::filtfilt( signal::butter (3, 2/(CutoffFreq*Rate)), d)
  ## now have WIF, WIY, WIS and AK, variables with new values to add
  print ('add variables to netCDF file')
  varF <- ncvar_def ("WIF",
    units="m/s",
    dim=Dim,
    missval=as.single(-32767.), prec='float',
    longname="WIY, high-pass-filtered")
  varX <- ncvar_def ("WIY",
    units="m/s",
    dim=Dim,
    missval=as.single(-32767.), prec='float',
    longname="vertical wind via complementary filter")
  varS <- ncvar_def ("WIS",
    units="m/s",
    dim=Dim,
    missval=as.single(-32767.), prec='float',
    longname="vertical wind like WIY but single-pass")
  varY <- ncvar_def ("AKY",
    units="degrees",
    dim=Dim,
    missval=as.single(-32767.), prec='float',
    longname="angle of attack, comp-filter")
  newfile <- ncvar_add (netCDFfile, varF)
  newfile <- ncvar_add (newfile, varX)
  newfile <- ncvar_add (newfile, varS)
  newfile <- ncvar_add (newfile, varY)
  
  print ('add attributes to new variables')
  ATV <- ncatt_get (netCDFfile, "WIC")
  V <- "WIF"
  copy_attributes (ATV, V, newfile)
  ncatt_put (newfile, V, attname="standard_name",
    attval="filtered_vertical_wind")
  ncatt_put (newfile, V, attname="Dependencies",
    attval="1 WIY")
  ncatt_put (newfile, V, attname="filter_time_constant",
    attval=sprintf("%d s", CutoffFreq))
  
  V <- "WIY"
  copy_attributes (ATV, V, newfile)
  ncatt_put (newfile, V, attname="standard_name",
    attval="vertical_wind_comp_filter")
  ncatt_put (newfile, V, attname="Dependencies",
    attval="1 WIC AKY")
  V <- "WIS"
  copy_attributes (ATV, V, newfile)
  ncatt_put (newfile, V, attname="standard_name",
    attval="vertical_wind_comp_filter_1")
  ncatt_put (newfile, V, attname="Dependencies",
    attval="1 WIC AKY")
  
  V <- "AKY"
  ATV <- ncatt_get (netCDFfile, "AKRD")
  copy_attributes (ATV, V, newfile)
  ncatt_put (newfile, V, attname="standard_name",
    attval="angle_of_attack_comp_filter")
  ncatt_put (newfile, V, attname="Dependencies",
    attval="1 ADIFR QCF")
  ncatt_put (newfile, V, attname="filter_time_constant",
    attval=sprintf("%d s", CutoffFreq))
  ncatt_put (newfile, V, attname="CalibrationCoefficients",
    # attval="c(20.986, 4.6245, 18.9965, -0.0034135)")
    attval=sprintf('c(%f.3, %f.4, %f.4, %f.7)', cff, cfs[1], cfs[2], cfs[3]))
  
  if (Rate == 1) {
    ncvar_put (newfile, varF, D$WIF)
    ncvar_put (newfile, varX, D$WIY)
    ncvar_put (newfile, varS, D$WIS)
    ncvar_put (newfile, varY, D$AK)
  } else if (Rate == 25) {
    ncvar_put (newfile, varF, D$WIF, count=c(25, nrow(D)/25))
    ncvar_put (newfile, varX, D$WIY, count=c(25, nrow(D)/25))
    ncvar_put (newfile, varS, D$WIS, count=c(25, nrow(D)/25))
    ncvar_put (newfile, varY, D$AK, count=c(25, nrow(D)/25))
  } else if (Rate == 50) {
    ncvar_put (newfile, varF, D$WIF, count=c(50, nrow(D)/50))
    ncvar_put (newfile, varX, D$WIY, count=c(50, nrow(D)/50))
    ncvar_put (newfile, varS, D$WIS, count=c(50, nrow(D)/50))
    ncvar_put (newfile, varY, D$AK, count=c(50, nrow(D)/50))
  }
  nc_close (newfile)
  print ('DONE with this flight')
}
print ('All Done with this run')


