## ----initialization, include=FALSE---------------------------------------

# require(knitr)
# opts_chunk$set(echo=FALSE, include=FALSE, fig.lp="fig:", dev='png', dpi=100)
# opts_chunk$set(fig.width=6, fig.height=5, fig.pos="center", digits=4)
# options(digits=5)
require(Ranadu, quietly = TRUE, warn.conflicts=FALSE)
require(ggplot2)
require(grid)
require(ggthemes)

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

## get run arguments: Project, Flight, addAKY, addGP, addTC, addROC
## If Flight == 'Next' process next unprocessed flight in project
## If Flight == "All" process all unprocessed flights in project
run.args <- commandArgs (TRUE)
print (run.args)
if (length (run.args) >= 2) {
  Project <- run.args[1]
  ProjectDir <- Project
  if (grepl ('HIPPO', Project)) {
    ProjectDir <- 'HIPPO'
  }
  if (run.args[2] == 'NEXT') {
    Flight <- getNext (ProjectDir, Project)
    if (Flight == 0) {stop ("no more files to process")}
    FlightWIF <- Flight    ## this doesn't work; need to change input$FlightWIF, can't here
  } else if (run.args[2] != 'All') {
    Flight <- as.numeric (run.args[2])
  } else { 
    Flight <- 'All'
  }
} else {
  print ("Usage: Rscript newWind.R Project Flight")
  print ("Example: Rscript newWind.R DEEPWAVE 12")
  stop ("exiting...")
}
addAKY <- FALSE
addGP <- FALSE
addTC <- FALSE
addROC <- FALSE
print (run.args)
if (length(run.args) >= 3) {
  if (any(grepl ('AKY', run.args))) {addAKY <- TRUE}
  if (any(grepl ('GustPod', run.args))) {addGP <- TRUE}
  if (any(grepl ('pitot', run.args))) {addTC <- TRUE}
  if (any(grepl ('ROC', run.args))) {addROC <- TRUE}
}

# SummarizeFit <- function(ft) {
#   print (summary(ft)$call)
#   print ("Coefficients:")
#   print (summary(ft)$coefficients)
#   print (sprintf ("Residual standard deviation: %.3f, dof=%d", summary(ft)$sigma, summary(ft)$df[2]))
#   print (sprintf ("R-squared %.3f", summary(ft)$r.squared))
# }

source ('AddWind.R')
source ('SplitDV.R')

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
# VarList <- c("ADIFR", "GGVSPD", "PITCH", "QCF", "PSF", "AKRD", "WIC", "TASF", "GGALT", "ROLL", "PSXC", "ATX", "QCXC")
## add variables needed to recalculate wind
# VarList <- c(VarList, "TASX", "ATTACK", "SSLIP", "GGVEW", "GGVNS", "VEW", "VNS", "THDG")
## Recommended variables in data.frame (to provide comparison variables also):
FI_Y <<- DataFileInfo (sprintf ("%s%s/%s", Directory, Project, files.ALL[1]), LLrange = FALSE)
if (grepl ('130', FI_Y$Platform)) {
  VR <- c("ACINS", "ADIFR", "AKRD", "ATTACK", "ATX",
    "EWX",  "GGALT",  "GGLAT", "GGVEW", "GGVNS",  "GGVSPD",
    "PITCH",  "PSFRD", "PSXC", "QCFR", "QCF",
    "ROLL", "SSLIP",  "TASX", "THDG", "UXC",  "VEW",  "VNS",
    "VYC",  "WDC",  "WIC",  "WSC" )
} else {
  VR <- c("ACINS", "ADIFR", "AKRD", "ATTACK", "ATX",
    "EWX",  "GGALT",  "GGLAT", "GGVEW", "GGVNS",  "GGVSPD",
    "PITCH",  "PSF", "PSXC", "QCF",
    "ROLL", "SSLIP",  "TASX", "THDG", "UXC",  "VEW",  "VNS",
    "VYC",  "WDC",  "WIC",  "WSC" )
}
if (addGP) { # This is undeveloped for the C-130 as yet; left here as placeholder, will fail.
  VR <- c(VR, 'ADIF_GP', 'BDIF_GP', 'CPITCH_GP', 'CROLL_GP', 'CTHDG_GP', 
    'CVEW_GP', 'GVNS_GP', 'CVSPD_GP', 'PS_GP', 'QC_GP')
}
if (addTC) { # Also not developed for the C-130
  VR <- c(VR, 'PSTF', 'QCTF')
}
VR <<- VR  # Save it for debug purposes
VarList <- VR
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
  Rate <- 1  # Although QAtools isn't intended for 25-Hz files, it can add wind variables to them.
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
  print ('calculate new variables')
  Data <- AddWind(D, Rate, addAKY=addAKY, addGP=addGP, addTC=addTC, addROC=addROC)
  print (sprintf ('arguments to AddWind are %s %s %s %s', addAKY, addGP, addTC, addROC))
  print ('returned from AddWind')
  print (sort(names(Data)))
  CutoffFreq <- 600
  if (addAKY) {
    d <- zoo::na.approx (as.vector(Data$WIY), maxgap=100*Rate, na.rm = FALSE, rule=2)
    d[is.na(d)] <- 0
    Data$WIF <- Data$WIY - signal::filtfilt( signal::butter (3, 2/(CutoffFreq*Rate)), d)
  }
  print ('set up variables for netCDF file')
  #######################################################
  source ('copyAttributes.R')
  
  netCDFfile <- nc_open (fnew, write=TRUE) 
  Dimensions <- attr (Data, "Dimensions")
  Dim <- Dimensions[["Time"]]
  Rate <- 1
  if ("sps25" %in% names (Dimensions)) {
    Rate <- 25
    Dim <- list(Dimensions[["sps25"]], Dimensions[["Time"]])
  }
  if ("sps50" %in% names (Dimensions)) {
    Rate <- 50
    Dim <- list(Dimensions[["sps50"]], Dimensions[["Time"]])
  }
  DATT <- Data  ## save to ensure that attributes are preserved
  
  ## variables to add to the netCDF file:
  VarNew <- c('AKY', 'WIY', 'WIF', 'AK_GP', 'SS_GP', 'WIG', 'WDG', 'WSG', 'TASG', 'UXG', 'VYG', 
    'ROC', 'TASTC', 'WDTC', 'WSTC', 'WITC', 'UXTC', 'VYTC')
  VarOld <- c('AKRD', 'WIC', 'WIC', 'AKRD', 'SSRD', 'WIC', 'WDC', 'WSC', 'TASX', 'UXC', 'VYC', 
    'GGVSPD', 'TASX', 'WDC', 'WSC', 'WIC', 'UXC', 'VYC') 
  VarUnits <- c('degrees', 'm/s', 'm/s', 'degrees', 'degrees', 'm/s', 'm/s', 'm/s', 'm/s', 'm/s', 'm/s', 'm/s', 'm/s', 'degrees', 'm/s', 'm/s', 'm/s', 'm/s')
  VarStdName <- c('angle-of-attack, CF', 'vertical wind, CF', 'vertical wind, filtered', 'angle-of-attack, GP', 'sideslip angle, GP', 
    'vertical wind, GP', 'wind direction, GP', 'wind speed, GP', 'true airspeed, GP', 'wind longitudinal component, GP', 
    'wind lateral component, GP', 'rate of climb', 'true airspeed, pitot-static', 'wind direction, pitot-static', 
    'wind speed, pitot-static', 'vertical wind, pitot-static', 'wind longitudinal component, pitot-static', 
    'wind lateral component, pitot-static')
  VarLongName <- c('angle of attack, complementary-filter',
    'vertical wind using comp-filter angle of attack',
    'vertical wind, high-pass-filtered',
    'angle of attack from the gustpod',
    'sideslip angle from the gustpod',
    'vertical wind from the gustpod',
    'horizontal wind direction from the gustpod',
    'horizontal wind speed from the gustpod',
    'true airspeed from the gustpod',
    'horizontal wind, longitudinal component, gustpod',
    'horizontal wind, lateral component, gustpod',
    'rate of climb of the aircraft from pressure',
    'true airspeed from the pitot-static sensor',
    'wind direction based on the pitot-static airspeed',
    'wind speed based on the pitot-static airspeed',
    'vertical wind based on TASTC and AKY',
    'horizontal wind, longitudinal component, pitot-static',
    'horizontal wind, lateral component, pitot-static')
  
  ## create the new variables
  varCDF <- list ()
  for (i in 1:length(VarNew)) {
    if (!addAKY && (i %in% 1:3)) {next}
    if (!addGP && (i %in% 4:11)) {next}
    if (!addTC && (i %in% 13:18)) {next}
    if (!addROC && (i == 12)) {next}
    print (sprintf ('new-netCDF %d%% done', as.integer(100*(i-1)/length(VarNew))))
    varCDF[[i]] <- ncvar_def (VarNew[i],  
      units=VarUnits[i], 
      dim=Dim, 
      missval=as.single(-32767.), prec='float', 
      longname=VarLongName[i])
    if (!exists ('newfile')) {
      newfile <- ncvar_add (netCDFfile, varCDF[[i]])
    } else {
      newfile <- ncvar_add (newfile, varCDF[[i]])
    }
    ATV <- ncatt_get (netCDFfile, VarOld[i])
    copy_attributes (ATV, VarNew[i], newfile)
    ncatt_put (newfile, VarNew[i], attname="standard_name", 
      attval=VarStdName[i])
    if (Rate == 1) {
      ncvar_put (newfile, varCDF[[i]], Data[, VarNew[i]])
    } else if (Rate == 25) {
      ncvar_put (newfile, varCDF[[i]], Data[, VarNew[i]], count=c(25, nrow(Data)/25))
    }
  }
  nc_close (newfile)
  print ('DONE with this flight')
}
print ('Done with this run')


