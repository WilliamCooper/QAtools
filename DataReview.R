## ----initialization,echo=FALSE,include=FALSE-----------------------------

require(Ranadu, quietly = TRUE, warn.conflicts=FALSE)
require(ggplot2)
require(grid)
require(ggthemes)
require(stringr)
source("global.R")
ProjectX <- NA
FlightX <- NA
StartX <- NA
EndX <- NA
PlotX <- NA
if (!interactive()) {  ## can run interactively or via Rscript with or without arguments.
  run_args <- commandArgs (TRUE)
  if (length (run_args) > 0) {
    if (nchar(run_args[1]) > 1) {
      ProjectX <- run_args[1]
    }
    if (length (run_args) > 1) {
      FlightX <- run_args[2]
      ALL <- FlightX == 'ALL'
      if (length (run_args) > 3) {
        StartX <- as.integer(run_args[3])
        if (StartX == 0) {StartX <- NA}
        if (length (run_args) > 3) {
          EndX <- as.integer(run_args[4])
          if (EndX == 400000) {EndX <- NA}
          if (length (run_args) > 4) {
            PlotX <- as.integer (run_args[5])
          }
        }
      }
    }
  }
} else {
  x <- readline (sprintf ("Project name [cr for detault=WECAN]:"))
  if (nchar(x) > 1) {
    ProjectX <- x
  } else {
    ProjectX <- 'WECAN'
  }
  x <- readline (sprintf ("Flight in format like rf07, or cr for latest, or ALL: "))
  if (nchar(x) > 1) {
    FlightX <- x
    ALL <- FlightX == 'ALL'
  }
  x <- readline (sprintf ("Start and end times, HHMMSS format (or cr for all): "))
  if (nchar(x) > 1) {
    StartX <- as.integer(sub('([0-9]*)[ ,-]*.*', '\\1', x))
    EndX <-   as.integer(sub('[0-9]*[ ,-]*([0-9]*).*', '\\1', x))
  }
  x <- readline (sprintf('Plot numbers desired (cr for standard): '))
  if (nchar(x) > 1) {
    Plotx <- as.integer(x)
  }
}
DataDir <- DataDirectory ()
if (is.na(ProjectX)) {
  ## find the first project that is defined in the Configuration.R file
  lines <- readLines ('Configuration.R')
  line <- lines[which (grepl('Project', lines))][1]
  Project <- sub("....$", "", sub(".*Project.....", "", line))
  rm("lines")  ## will have to re-read to configure the project
} else {
  Project <- ProjectX
}
ProjectDir <- Project
Trace <- FALSE

## find the last flight in the project directory
getNext <- function(Project) {
  Flt <- list.files (sprintf ("%s%s/", DataDirectory (), Project),
    sprintf ("%s.f...nc", Project))
  if (is.na(Flt[1])) {return ()}
  ## which is last modified?
  mtm <- vector()
  for (Fl in Flt) {
    mtm <- c(mtm, file.mtime (sprintf ('%s%s/%s', DataDirectory(), Project, Fl)))
  }
  Flight <- Flt[which(max(mtm) == mtm)]
  return (Flight)
}

if (is.na (FlightX)) {
  Flight <- getNext (Project)
  ALL <- FALSE
} else {
  ALL <- FlightX == 'ALL'
  if (!ALL) {Flight <- sprintf('%s%s.nc', Project, FlightX)}
}

## This loop is for Flight == "ALL" and loops through the project files
if (ALL) {
  ## get list of netCDF files in the project directory:
  Flt <- sort (list.files (sprintf ("%s%s/", DataDirectory (), Project),
    sprintf ("%s.f...nc", Project)), decreasing = TRUE)
} else {
  Flt <- Flight
}

source('transferAttributes.R')
source('loadVRPlot.R')
source('DRfunctions.R')
## for the cavity-pressure-check section
load('CAVPcoefficients.Rdata')  ## loads cavcfL and cavcfR for fits
## ----dataReview, include=TRUE, echo=TRUE---------------------------------
## set up plot definitions as in QAtools:
nplots <- c(1, 3:17, 19:23)    # default
psq <- c(1,1, 1,2, 3,1, 4,1, 5,1, 5,2, 5,3, 5,4, 6,1, 7,1, 7,2,  #11
  8,1, 9,1, 9,2, 10,1, 10,2, 11,1, 12,1, 13,1, 14,1, 15,1, 15,2, #22
  16,1, 16,2, 16,3, 17,1, 19,1, 19,2, #28
  20,1, 20,2, 20,3, 20,4, 21,1, 21,2, 21,3, 21,4, #36
  22,1, 22,2, 22,3, 22,4, 23,1, 23,2, #42
  24,1, 25,1, 26,1, 27,1, 28,1, 29,1, 30,1) #49
L <- length (psq)/2
dim(psq) <- c(2,L)
testPlot <- function (k) {
  return(k %in% nplots || nplots == 0)
}

## make plot functions available
for (np in 1:2) {
  if (testPlot(np)) {
    eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
  }
}
for (np in 3:30) {
  if (file.exists (sprintf ("./PlotFunctions/RPlot%d.R", np))) {
    eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
  }
}

## Have Project and psq definition

for (Flight in Flt) {
  fname = sprintf("%s%s/%s", DataDir, ProjectDir, Flight)
  print (sprintf ('processing file %s', Flight))
  ## quit/exit if selected file does not exist:
  if (!file.exists (fname)) {
    print (sprintf ('selected file %s does not exist, so skipping', fname))
  } else {
    ## also skip if output file already exists, unless specific flight is specified
    spf <- sprintf("%sPlots.pdf", sub('.nc', '', Flight))
    if ((ALL || is.na(FlightX)) && file.exists (spf)) {
      print (sprintf ('output file for flight %s already exists; skipping (purge to replace it)', spf))
    } else {

      netCDFfile <- NULL
      CCDP <- NULL
      CFSSP <- NULL
      CUHSAS <- NULL
      C1DC <- NULL
      if (!grepl("[a-zA-Z]*(.f)[0-9]*.nc", Flight)) {
      # if (!(str_detect(Flight, "[a-zA-Z]*(.f)[0-9]*.nc"))) {
          print (sprintf ('nonstandard flight string in %s', Flight))
          print ('Flight string must match [rt]f[0-9][0-9]')
          quit()
      } else {
          fnumber <- as.numeric (sub('[a-zA-Z]*([0-9]*).nc', '\\1', Flight))
          ftype <- sub('[A-Za-z]*(.f)[0-9]*.nc', '\\1', Flight)
      }

#      if (!(str_detect(Flight, "[a-zA-Z]*(.f)[0-9]*.nc"))) {
#          print (sprintf ('nonstandard flight string in %s', Flight))
#          print ('Flight string must match [rt]f[0-9][0-9]')
#          quit()
#      } else {
##          fnumber <- as.numeric (sub('[a-zA-Z]*([0-9]*).nc', '\\1', Flight))
##          ftype <- sub('[A-Za-z]*(.f)[0-9]*.nc', '\\1', Flight)
# 210712, BBS: the above was not handling hyphens in project names (e.g. WCR-TEST or ASPIRE-TEST), added hyphens:
##            fnumber <- as.numeric (sub('[A-Za-z-]*([0-9]*).nc', '\\1', Flight))
##            ftype <- sub('[A-Za-z-]*(.f)[0-9]*.nc', '\\1', Flight)
# 210729, BBS: now causing problems for MethaneAIR21 because of the numbers in the project name. . .
# 210802, TMT: this fix relies on a flight argument, but it will work for MethaneAIR
#        flight_info <- strsplit(FlightX, 0)
#        ftype <- flight_info[[1]][1]
#        fnumber <- as.numeric(flight_info[[1]][2])
#      }
      ## next statement needs to be inside "ALL" loop, in case available variables change
      VRPlot <- loadVRPlot (Project, FALSE, fnumber, psq)  ## get VRPlot list for this project
      Cradeg <- pi/180
      ## next load the data.frame
      VarList <- vector()
      for (i in 1:length(VRPlot)) {
        for (j in 1:length (VRPlot[[i]])) {
          VarList <- c(VarList, VRPlot[[i]][j])
        }
      }
      VarList <- unique (VarList)
      VarList <- VarList[!is.na(VarList)]
      if ('' %in% VarList) { # '' in VRPlot needs to be removed
        VarList <- VarList[-which(VarList == '')]
      }

      Data <- getNetCDF(fname, VarList)
      ## set the start and end indices to the first and last time TASX > 65
      itx <- which(Data$TASX > 65)
      if (is.na (StartX)) {
        it1 <- itx[1]
      } else {
        it1 <- getIndex (Data, StartX)
        if (it1 < 1) {it1 <- 1}
      }
      if (is.na (EndX)) {
        it2 <- itx[length(itx)]
      } else {
        it2 <- getIndex (Data, EndX)
        if (it2 < 0 || it2 > nrow(Data)) {it2 <- nrow(Data)}
      }
      if (is.na (it1)) {
         print("Could not find time period where TASX > 65")
         quit()
      }

      DataL <- Data[it1:it2, ]
      Data <- transferAttributes(DataL, Data)  ## retain attributes
      times <- c(Data$Time[1], Data$Time[nrow(Data)])
      ## now ready to make plots
      ## include some items to make it possible to use savePDF() from QAtools:
      inp <- list(Project=Project, Flight = fnumber, typeFlight=ftype)
      ## dummy replacement for QAtools version; called in savePDF()
      limitData <- function (Data, inp) {return(Data)}
      ## function used in many plot routines:
      AddFooter <- function() {
        CallingFunction <- sub ("\\(.*\\)", "", deparse (sys.call (-1)))
        mtext(paste(FigFooter,'generated by ', CallingFunction,
          FigDatestr, GitRepo, GitBranch, GitTag),1,outer=T,cex=0.75)
      }
      SE <- getStartEnd (Data$Time)
      i <- getIndex (Data$Time, SE[1])
      GitRepo=sprintf(system("git config --get remote.origin.url", intern = TRUE))
#      GitCommit=sprintf(system("git rev-parse --short HEAD", intern = TRUE))
      GitBranch=sprintf(system("git rev-parse --abbrev-ref HEAD", intern = TRUE))
      GitTag=sprintf(system("git describe --tags", intern = TRUE))
      FigFooter=sprintf("%s %s%02d %s %s-%s UTC,", Project, ftype,
        fnumber, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
        strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),
        strftime(Data$Time[getIndex(Data$Time,SE[2])],
          format="%H:%M:%S", tz='UTC'))
      FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
      StartTime <- SE[1]
      source('savePDF.R')
      if (!is.na (PlotX)) {nplots <- PlotX}
      oldw <- getOption("warn")
      options(warn = -1)  ## suppress ggplot warning messages
      savePDF(Data, inp)
      options(warn = oldw)
      print (sprintf ('generated plot file %s', plotfile))
    }
  }
}
