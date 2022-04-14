

## clear global environment that might be left from the last run
rm(list=ls( ))
while (exists('panel1ylim')) {rm(panel1ylim, inherits=TRUE)}
while (exists('panel2ylim')) {rm(panel2ylim, inherits=TRUE)}
while (exists('panel3ylim')) {rm(panel3ylim, inherits=TRUE)}
while (exists('panel4ylim')) {rm(panel4ylim, inherits=TRUE)}

# Size of the data-review plot window:
WHeight <- 680 
WWidth <- 1200
cexmain <- 1  # standard size of the title text.

# This function is used to define consistent margins for multiple-panel
# plots. The meaning of 'panel' is:
#     1 - only one panel in this plot.
#     2 - this is one of a series of panels, but not the last, shiny display.
#     3 - this is the last in a series of panels, shiny display.
#     4 - one of a series, savePDF
#     5 - last of a series, savePDF
# The outer margin (oma) is included for the plot footer.
# The two-line margin below the panel (cases 2 and 3) clips
# the abscissa title "Time", so using this sequence gives
# stacked panels where the abscissa title appears only with
# the bottom panel but all panels have abscissa labels.
setMargins <- function(panel = 1) {
  switch (panel,
          op <- par (mar = c(5, 5, 1, 1) + 0.1, oma = c(1.1, 0, 0, 0)), 
          op <- par (mar = c(2, 5, 1, 1) + 0.1),
          op <- par (mar = c(5, 5, 1, 1) + 0.1, oma = c(1.1, 0, 0, 0)),
          op <- par (mar = c(2, 5, 1, 1) + 0.1, oma = c(1.1, 0, 0, 0)),
          op <- par (mar = c(5, 5, 1, 1) + 0.1)
  )
}

# The next function is used to set ordinate limits including "brush" changes:
YLMF <- function (panel, defaultRange) {
  s <- sprintf ('panel%dylim', panel)
  if (exists(s)) {
    return (get (s, inherits = TRUE))
  } else {
    if (any(is.infinite(defaultRange))) {
      defaultRange <- c(0.1, 1)
    }
    return (defaultRange)
  }
}

## for these plots, disable "brush":
noBrush <- c(1, 4, 6, 26, 28, 29, 30, 31, 32, 
             33, 34, 35, 36, 37, 38, 39, 40, 45)
#setwd('~/RStudio/QAtools')
suppressMessages (
  library(shiny, quietly=TRUE, warn.conflicts=FALSE)
)
library(shinyBS, quietly=TRUE, warn.conflicts=FALSE)

suppressMessages (suppressWarnings (
  library(Ranadu, quietly=TRUE, warn.conflicts=FALSE))
)
options (stringsAsFactors=FALSE)
## if this is set TRUE then messages will print in the console
## indicating which functions are entered, to trace the sequence
## of interactions when window entries are changed.
Trace <- FALSE
# Trace <- TRUE

library(tictoc)
if (Trace) {tic('global')}
require(numDeriv, quietly = TRUE, warn.conflicts=FALSE) ## needed, KalmanFilter

## temporary, pending update of Ranadu package:
# source ("./correctT.R")
# source ("../Ranadu/R/getNetCDF.R")
# source ('getNetCDF.R')
minT <- as.POSIXct(0, origin='2012-05-29', tz='UTC')
maxT <- as.POSIXct(3600*8, origin='2012-05-29', tz='UTC')
step <- 60

PJ <- c('TI3GER', 'ASPIRE', 'MethaneAIR21', 'ASPIRE-TEST', 'SPICULE', 'WCR-TEST', 'MethaneAIR', 'ACCLIP-TEST', 'OTREC', 'ECLIPSE2019', 'OTREC-TEST', 'WECAN', 'SOCRATES', 'WECAN-TEST', 'ECLIPSE', 'ARISTO2017', 'ORCAS', 'CSET', 'NOREASTER', 'HCRTEST',
  'DEEPWAVE', 'CONTRAST', 'SPRITE-II', 'MPEX', 'DC3', 'HEFT10', 'IDEAS-4', 'FRAPPE',
  'TORERO', 'HIPPO-5', 'HIPPO-4', 'HIPPO-3', 'HIPPO-2', 'DC3-TEST',
  'HIPPO-1','PREDICT', 'START08', 'PACDEX', 'TREX', 'WINTER', 'NOMADSS')
### Replace this by constructing a list of available projects
## by searching the DataDirectory(). That way a new project will
## be incorporated automatically.
PJ <- list.dirs(path = DataDirectory(), full.names = FALSE, recursive = FALSE)
PJ <- PJ[-which('lost+found' == PJ)]
if ('lost+found' %in% PJ) {
  PJ <- PJ[-which('lost+found' == PJ)]
} 
## Leave in alphabetical order, except for the first which is the latest modified.
FullPJ <- paste0(DataDirectory(), PJ)
iw <- which.max(file.mtime(FullPJ))
PJ <- c(PJ[iw], PJ[-iw])
## This branch supports a single-HIPPO-directory structure,
## as on /scr/raf_data. The code also accepts individual directories HIPPO-1, etc.
singleHIPPO <- ifelse(('HIPPO' %in% PJ) && !('HIPPO-1' %in% PJ), TRUE, FALSE)
if (singleHIPPO) {
  PJ <- c(PJ[-which(PJ %in% 'HIPPO')], 'HIPPO-1', 'HIPPO-2', 
                          'HIPPO-3', 'HIPPO-4', 'HIPPO-5')
} else if ('HIPPO' %in% PJ) {
  PJ <- PJ[-which(PJ == 'HIPPO')]
}
PJ <- unique(PJ)
## Keep only directories with a rf01 or tf01
for (P in PJ) {
  if (grepl('HIPPO-', P) && singleHIPPO) {
    fn <- sprintf ('%sHIPPO/%srf01.nc', DataDirectory (), P)
  } else {
    fn <- sprintf ('%s%s/%srf01.nc', DataDirectory (), P, P)
  }
  if (!file.exists (fn)) {
    fn <- sub('rf', 'tf', fn)
  }
  if (!file.exists (fn)) {
    fn <- sub ('\\.nc', '.Rdata', fn)
  }
  if (!file.exists (fn)) {
    fn <- sub ('tf', 'rf', fn)
    fn <- sub ('\\.nc', '.Rdata', fn)
  }
  if (!file.exists (fn)) {PJ[PJ==P] <- NA}
}
PJ <- PJ[!is.na(PJ)]

Cradeg <- pi/180
Project <- PJ[1]
ProjectPP <- PJ[1]
ProjectKF <- PJ[3]
Flight <- 1
FlightKF <- 1
ProjectKP <- PJ[1]
FlightKP <- 1
ProjectHOT <- PJ[1]
FlightHOT <- 1
ProjectWIF <- PJ[1]
FlightWIF <- 1
newnames <- ' '
## This special version is needed for 'specialData' vars; 
## don't substitute the version in Ranadu::selectTime
source('transferAttributes.R')

## for the 'frozen' section:   #################################
slctd <- 1
qualifyIFX <- function (ifx) {
  if (!is.integer (ifx)) {
    ifx <- as.integer (ifx)
  }
  ifx <- min (ifx, nrow(frozenRange))
  ifx <- max (ifx, 1)
}

chBAD <- 'none'

## convenience replacements to avoid all the times I have to type "na.rm=TRUE"
mean <- function(x, ..., na.rm = TRUE) {
  suppressWarnings (base::mean(x, ..., na.rm = na.rm))
}

sd <- function (x, ..., na.rm = TRUE) {
  suppressWarnings (stats::sd(x, ..., na.rm = na.rm))
}

min <- function (x, ..., na.rm = TRUE) {
  suppressWarnings (base::min(x, ..., na.rm = na.rm))
}

max <- function (x, ..., na.rm = TRUE) {
  suppressWarnings (base::max(x, ..., na.rm = na.rm))
}

load('frozen/frozenRange.Rdata')  # loads frozenRange data.frame
# load('NDORCAS.Rdata')           # loads variable list ND
# ND <- ND[-1]
ND <- frozenRange$Var
ND <- ND[-which(ND == 'RF')]
fname <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), ProjectKP, ProjectKP, FlightKP)
if (file.exists(fname)) {
  Data <- getNetCDF(fname, ND)
} else {
  fname <- sprintf ('%s%s/%stf%02d.nc', DataDirectory(), ProjectKP, ProjectKP, FlightKP)
  if (file.exists(fname)) {
    Data <- getNetCDF(fname, ND)
  }
}

setNA <- function (.x, .v) {
  X <- zoo::na.approx (as.vector (.x), maxgap=1000, na.rm=FALSE)
  X[is.na(X)] <- .v
  return (X)
}
saveFrozenEvents <- function () {
  save(frozenRange, file='frozen/frozenRange.Rdata')
}
saveBADEvents <- function () {
  if (exists ('BAD')) {
    iskip <- which(BAD$Variable == 'none')
    if (length(iskip) > 0) {BAD <- BAD[-iskip, ]}
    save(BAD, file='frozen/BAD.Rdata')
  }
}

if (file.exists ('frozen/BAD.Rdata')) {
  load('frozen/BAD.Rdata')
} else {
  BAD <- data.frame(Project='none', Flight=1, Variable='none', Start=minT, End=maxT, Type='range', Flag=0)
}
if (file.exists('frozen/frozenEvents.Rdata')) {
  load('frozen/frozenEvents.Rdata')
  if (nrow(frozenEvents) > 0) {
    chFrozen <- vector('character', nrow(frozenEvents))
    for (i in 1:nrow(frozenEvents)) {
      u <- ifelse (frozenEvents$Rej[i], 'Y', 'N')
      print (s <- sprintf ('%s %d %s rf%02d %s-%s', u, i, frozenEvents$Project[i], frozenEvents$Flight[i],
                           formatTime(frozenEvents$Start[i]), formatTime(frozenEvents$End[i])))
      chFrozen[i] <- sprintf ('%d', i)
      names(chFrozen)[i] <- s
    }
  } else {
    chFrozen <- 'none'
  }
} else {
  chFrozen <- 'none'
}

searchFrozen <- function (Data, useH, whichFrozen, sdLimit, pcntLimit) {
  frozenQ <- data.frame()
  dd <- Data[Data$WOW_A == 0, ]
  for (i in 1:nrow(frozenRange)) {
    if (frozenRange$keep[i] && (frozenRange$Raw[i] || useH)) {
      v <- frozenRange$Var[i]
      if (grepl('range', whichFrozen)) {
        minv <- min (dd[, v])
        maxv <- max (dd[, v])
        if (is.infinite (minv) || is.infinite (maxv) || minv < frozenRange$vlow[i] || maxv > frozenRange$vhigh[i]) {
          frozenQ <- rbind (frozenQ, frozenRange[i,])
        }
      } else if (grepl('frozen', whichFrozen)) {
        if (any(rle(as.vector(dd[, v]))$lengths > 60)) {
          frozenQ <- rbind (frozenQ, frozenRange[i, ])
        }
      } else if (grepl('spikes', whichFrozen) && any(!is.na(dd[, v]))) {
        L <- 60
        y <- c(rep(dd[1, v], L), dd[, v], rep(dd[nrow(dd), v], L))
        y <- setNA(y, mean(y))
        vf <- signal::filtfilt (signal::butter (3, 2/L), y)
        vf <- vf[(L+1):(length(vf)-L)]
        y <- y[(L+1):(length(y)-L)]
        sdf <- sd (vf)
        if (any (abs (y - vf) > sdLimit * sdf)) {
          frozenQ <- rbind (frozenQ, frozenRange[i, ])
        }
      } else {  ## for jumps
        yj <- c(0, diff(dd[, v]))
        yj <- setNA (yj, mean(yj))
        if (any (abs (yj) > pcntLimit * 0.01 * (frozenRange$vhigh[i] - frozenRange$vlow[i]))) {
          frozenQ <- rbind (frozenQ, frozenRange[i, ])
        }
      }
    }
  }
  return(frozenQ)
}
## end of the 'frozen' section #################################

## for the cavity-pressure-check section
load('CAVPcoefficients.Rdata')  ## loads cavcfL and cavcfR for fits

## for the Resolution exercise:
xp <- (-600:600)/100
xpp <- (0:1000)
ypp <- pnorm(xpp/500-1, sd=.3)

BadList <- list('ORCASrf12', 'ORCASrf13', 'WINTERrf10', 'NOMADSSrf01')

checkBad <- function (fn) {
  if (any(grepl(fn, BadList))) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

## all possible temperature variables
ATVARS <- c('ATX', 'AT_A', 'AT_A2', 'ATH1', 'ATH2', 'ATH3', 'ATH4', 'ATF1', 'ATF2',
            'ATHR1', 'ATHR2', 'ATHL1', 'ATHL2', 'ATFH1', 'ATFH2')

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

AddT <- function (Time, ATime=0) {
  S <- Time %% 100 + ((Time %/% 100) %% 100) * 60 + (Time %/% 10000) * 3600
  S <- S + ATime
  S <- S %% 60 + 100 * ((S %% 3600) %/% 60) + 10000 * (S %/% 3600) 
  return (S)
}

dataDPM <- function(ProjDir, ProjectPP, Flight, VL, START, END) {
  fname <- sprintf ('%s%s/%s%s.nc', DataDirectory (), ProjDir, ProjectPP, Flight)
  if (fname != fnameDPM || START != STARTDPM || END != ENDDPM) {
    DPM <- getNetCDF (fname, VL, Start=START, End=END)
    ## note: have checked that ROC determined this way matches GGVSPD when present
    DPM$ROC <- ShiftInTime(c(0, diff(DPM$GGALT)), .shift=-500)
    DPM <<- DPM
    fnameDPM <<- fname
    STARTDPM <<- START
    ENDDPM <<- END
  }
  return (DPM)
}

dataDYM <- function(ProjDir, ProjectPP, Flight, VL, START, END) {
  fname <- sprintf ('%s%s/%s%s.nc', DataDirectory (), ProjDir, ProjectPP, Flight)
  if (fname != fnameDYM || START != STARTDYM || END != ENDDYM) {
    if (Trace) {print (sprintf ('in dataDYM, file is %s, Start=%d, End=%d', fname, START, END))}
    if (Trace) {print (VL)}
    VL <<- VL
    DYM <- getNetCDF (fname, VL, Start=START, End=END)
    DYM <<- DYM
    fnameDYM <<- fname
    STARTDYM <<- START
    ENDDYM <<- END
  }
  return (DYM)
}

dataDCR <- function(ProjDir, ProjectPP, Flight, VL, START, END) {
  fname <- sprintf ('%s%s/%s%s.nc', DataDirectory (), ProjDir, ProjectPP, Flight)
  if (Trace) {
    print (sprintf ('in dataDCR, file name to load is %s %d %d', fname, START, END))
  }
  if (!file.exists(fname)) {
    return (NA)
  }
  FI <- DataFileInfo (fname, LLrange = FALSE)
  if ('GGVSPD' %in% FI$Variables) {
  } else {
    if ('GVSPD' %in% FI$Variables) {
      VL[which(VL == 'GGVSPD')] <- 'GVSPD'
    } else if ('GGVSPD_NVTL' %in% FI$Variables) {
      VL[which(VL == 'GGVSPD')] <- 'GGVSPD_NVTL'
    }
  }
  if (fname != fnameDCR || START != STARTDCR || END != ENDDCR) {
    DCR <- getNetCDF (fname, VL, Start=START, End=END)
    if (Trace) {print (sprintf (' called getNetCDF, nrows in file is %d', nrow(DCR)))}
    ## add the drifting coordinates to the data.frame, for use in brush4:
    data.rate <- 1
    if (DCR$Time[2] - DCR$Time[1] <= 0.04) {data.rate <- 25}
    if (DCR$Time[2] - DCR$Time[1] <= 0.02) {data.rate <- 50}
    # DCR <- DCR[.Range, ]
    with(DCR, {
      ## try to interpolate for missing values
      TAS <<- zoo::na.approx (as.vector(TASX), maxgap=1000, na.rm = TRUE)
      HDG <<- (pi/180) * zoo::na.approx (as.vector(THDG+SSLIP), maxgap=1000, na.rm = TRUE)      
    })
    DCR$xa <- 0.001 * cumsum (TAS * sin(HDG)) / data.rate
    DCR$ya <- 0.001 * cumsum (TAS * cos(HDG)) / data.rate
    
    DCR <<- DCR
    fnameDCR <<- fname
    STARTDCR <<- START
    ENDDCR <<- END
  }
  return (DCR)
}

dataDRH <- function(ProjDir, ProjectPP, Flight, VL, START, END) {
  fname <- sprintf ('%s%s/%s%s.nc', DataDirectory (), ProjDir, ProjectPP, Flight)
  if (Trace) {print (sprintf ('in dataDRH, file name to load is %s %d %d', fname, START, END))}
  if (fname != fnameDRH || START != STARTDRH || END != ENDDRH) {
    DRH <- getNetCDF (fname, VL, Start=START, End=END)
    if (Trace) {print (sprintf (' called getNetCDF, nrows in file is %d', nrow(DRH)))}
    ## add the drifting coordinates to the data.frame, for use in brush4:
    data.rate <- 1
    if (DRH$Time[2] - DRH$Time[1] <= 0.04) {data.rate <- 25}
    if (DRH$Time[2] - DRH$Time[1] <= 0.02) {data.rate <- 50}
    # DRH <- DRH[.Range, ]
    with(DRH, {
      ## try to interpolate for missing values
      TAS <<- zoo::na.approx (as.vector(TASX), maxgap=1000, na.rm = TRUE)
      HDG <<- (pi/180) * zoo::na.approx (as.vector(THDG+SSLIP), maxgap=1000, na.rm = TRUE)      
    })
    DRH$xa <- 0.001 * cumsum (TAS * sin(HDG)) / data.rate
    DRH$ya <- 0.001 * cumsum (TAS * cos(HDG)) / data.rate
    
    DRH <<- DRH
    fnameDRH <<- fname
    STARTDRH <<- START
    ENDDRH <<- END
  }
  return (DRH)
}

SeekManvrs <- function (Data, manL) {
  # print (c('in SeekManvrs, manL = ', manL))
  source ("./PlotFunctions/SpeedRunSearch.R")
  source ("./PlotFunctions/CircleSearch.R")
  source ("./PlotFunctions/PitchSearch.R")
  source ("./PlotFunctions/YawSearch.R")
  source ("./PlotFunctions/ReverseHeadingSearch.R")
  # print ('list of maneuvers:')
  lst <- vector('character')
  if ('pitch' %in% manL) {
    lt <- PitchSearch (Data)
    if (!is.na(lt[1]))  {lst <- lt}
  }
  if ('yaw' %in% manL) {
    lt <- YawSearch (Data)
    if (!is.na(lt[1])) {lst <- c(lst, lt)}
  }
  if ('speed run' %in% manL) {
    lt <- SpeedRunSearch (Data) 
    if (!is.na(lt[1])) {lst <- c(lst, lt)}
  }
  if ('circle' %in% manL) {
    lt <- CircleSearch (Data)
    if (!is.na(lt[1])) {lst <- c(lst, lt)}
  }
  if ('reverse heading' %in% manL) {
    lt <- ReverseHeadingSearch (Data)
    if (!is.na(lt[1])) {lst <- c(lst, lt)}
  }
  # print ('end of maneuver list')
  return (lst)
}

SeekManeuvers <- function (Data) {
  source ("./PlotFunctions/SpeedRunSearch.R")
  source ("./PlotFunctions/CircleSearch.R")
  source ("./PlotFunctions/PitchSearch.R")
  source ("./PlotFunctions/YawSearch.R")
  source ("./PlotFunctions/ReverseHeadingSearch.R")
  print ('list of maneuvers:')
  PitchSearch (Data)
  YawSearch (Data)
  SpeedRunSearch (Data)
  CircleSearch (Data)
  ReverseHeadingSearch (Data)
  print ('end of maneuver list')
}

ProjectSeekManeuvers <- function (inp) {
  ## for this project, this replaces the data.frame Maneuvers with a new temporary one
  Project <- inp$ProjectPP
  ProjDir <- Project
  if(grepl('HIPPO-', Project) && singleHIPPO) {ProjDir <- 'HIPPO'}
  Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjDir),
                          sprintf ("%srf...nc$", Project)))
  Fl <- c(Fl, sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjDir),
                                sprintf ("%stf...nc$", Project))))
  # print (Fl)
  if (!is.na (Fl[1])) {
    print (sprintf ('Maneuvers for project %s', Project))
    lst <- vector('character')
    for (Flt in Fl) {
      print (sprintf('checking %s', Flt))
      if (grepl ('tf', Flt)) {
        Flight <- sub (".*tf", '', sub(".nc", '', Flt))
        Type <- 'tf'
      } else if (grepl ('rf', Flt)) {
        Flight <- sub (".*rf", '', sub(".nc", '', Flt))
        Type <- 'rf'
      }
      Flight <- as.integer(Flight)
      # print (Flt)
      if (!checkBad(sprintf('%s%s%02d', Project, Type, Flight))) {
        Data <- getNetCDF (sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), ProjDir, 
          Project, Type, Flight),
          (c('ATX', 'GGALT', 'LATC', 'LONC', 'PSXC', 'QCXC', 'TASX', 'WDC', 'WSC', 'WIC',
            'PITCH', 'SSRD', 'THDG', 'ROLL')))
        lt <- SeekManvrs (Data, inp$manS)
        if(!is.na(lt[1])) {lst <- c(lst,lt)}
      }
    }
    print (lst)
    print (sprintf ('End of list for project %s', Project))
    save(lst, file=sprintf('maneuvers/maneuvers%s', Project))
    ## replace Maneuvers data.frame with one created here:
    Maneuvers.Save <- Maneuvers
    lst <- gsub(' maneuver', '', lst)
    lst <- gsub('speed run', 'speed-run', lst)
    lst <- gsub('reverse heading', 'reverse-heading', lst)
    lst <- gsub('--', ' ', lst)
    lst <- gsub('  ', ' ', lst)
    lst <- gsub('  ', ' ', lst)
    lst <- gsub('  ', ' ', lst)
    lst <- gsub('  ', ' ', lst)
    newDF <- data.frame()
    for (l in lst) {
      s <- strsplit(l, ' ')
      lp <- s[[1]][1]
      lf <- s[[1]][2]
      lt <- s[[1]][3]
      ls <- as.integer(s[[1]][4])
      le <- as.integer(s[[1]][5])
      if (length(s[[1]]) > 5) {
        lo1 <- as.numeric(s[[1]][6])
      } else {
        lo1 <- 0
      }
      if (length(s[[1]]) > 6) {
        lo2 <- as.numeric(s[[1]][7])
      } else {
        lo2 <- 0
      }
      newDF <- rbind(newDF, data.frame(Project=lp, Flight=lf, Type=lt, Start=ls, End=le, Other1=lo1, Other2=lo2))
    }
    newDF$Type[newDF$Type == 'speed-run'] <- 'speed run'
    newDF$Type[newDF$Type == 'reverse-heading'] <- 'reverse heading'
    if (exists ('Maneuvers')) {
      newDF <- rbind (Maneuvers, newDF)
    }
    Maneuvers <<- Maneuvers <- newDF
    file.copy('Maneuvers.Rdata', sprintf('Maneuvers.backup%s', gsub(' ', '', date())))
    save(Maneuvers, file='Maneuvers.Rdata')
    return (1)
  }
  
}

fnamePPS <- ''
# ProjectPP <- ''
STARTDPM <- 0
ENDDPM <- 0
fnameDPM <- ''
countPM <- 0
itemPM <- -1
STARTDYM <- 0
ENDDYM <- 0
fnameDYM <- ''
countYM <- 1
itemYM <- -1
fnameDRH <- ''
fnameDCR <- ''
countRH <- 1
countCR <- 1
itemRH <- -1
itemCR <- -1

n <- 50000
X1R <- rnorm(n); X2R <- rnorm(n)
DX <- X2R - X1R
ddd <- (-600:600)/100

## for the Kalman filter:
ALL <- FALSE
NEXT <- FALSE
newAK <- TRUE
newSS <- TRUE
simple <- FALSE
NSTEP=10
showPlots <- TRUE
viewPlot <- 1
genPlot <- TRUE
firstRun <- TRUE
messg <- 'waiting for run'
progressExists <- FALSE

## for HeightOfTerrain:
NEXTHOT <- FALSE
ALLHOT <- FALSE
genPlotHOT <- TRUE
viewPlotHOT <- 1

NEXTWIF <- FALSE
ALLWIF <- FALSE

getNext <- function(Project) {
  Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), Project),
                          sprintf ("%srf..KF.nc", Project)), decreasing = TRUE)[1]
  if (is.na (Fl)) {
    Flight <- 1
  } else {
    Flight <- sub (".*rf", '',  sub ("KF.nc", '', Fl))
    Flight <- as.numeric(Flight)+1
  }
  return (Flight)
}


getNextHOT <- function(Project) {
  Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), Project),
                          sprintf ("%srf..Z.nc", Project)), decreasing = TRUE)[1]
  if (is.na (Fl)) {
    Flight <- 1
  } else {
    Flight <- sub (".*rf", '',  sub ("Z.nc", '', Fl))
    Flight <- as.numeric(Flight)+1
  }
  return (Flight)
}

ShowProgress <- function(NSTEP, progress, Flight) {
  PLOOP <- 1
  TimeEstimate <- 1.5 * 60 * 9 * 10 / NSTEP  ## for 9-h flight
  while (PLOOP) {
    Sys.sleep (1)
    PLOOP <- PLOOP + 1
    if (PLOOP > TimeEstimate) {break}
    M <- system('tail -n 1 ../KalmanFilter/KFlog', intern=TRUE)
    if (grepl ('main loop', M)) {
      PLOOP <- FALSE
      progress$set(message = 'main-loop progress',
                   detail = sprintf('flight %d', Flight), value=1)
      next
    }
    if (grepl('IRU loop', M)) {
      P <- sub ('.*loop ', '', sub ('% do.*', '', M))
      # print (sprintf ('progress is %d', as.integer(P)))
      progress$set(message = 'retrieve IRU msrmts',
                   detail = sprintf('flight %d', Flight), value=as.integer (P))
    }
  }
  PLOOP <- 1
  while (PLOOP) {
    Sys.sleep (1)
    PLOOP <- PLOOP + 1
    if (PLOOP > TimeEstimate) {break}
    M <- system('tail -n 1 ../KalmanFilter/KFlog', intern=TRUE)
    if (grepl ('main loop is done', M) || grepl ('generating plots', M) || grepl ('making new', M)) {
      PLOOP <- FALSE
      next
    }
    if (grepl('Kalman-loop', M)) {
      P <- sub ('.*loop ', '', sub ('% do.*', '', M))
      # print (sprintf ('progress is %d', as.integer(P)))
      progress$set(message = 'main-loop progress',
                   detail = sprintf('flight %d', Flight), value=as.integer (P))
    }
  }
  PLOOP <- 1
  while (PLOOP) {
    Sys.sleep (0.5)
    PLOOP <- PLOOP + 1
    if (PLOOP > 120) {break}
    M <- system('tail -n 1 ../KalmanFilter/KFlog', intern=TRUE)
    if (grepl ('plots generated', M) || grepl ('making new', M)) {
      PLOOP <- FALSE
      next
    }
    if (grepl('figures', M)) {
      P <- sub ('.*figures ', '', sub ('% do.*', '', M))
      # print (sprintf ('progress is %d', as.integer(P)))
      progress$set(message = 'generating figures',
                   detail = sprintf('flight %d', Flight), value=as.integer (P))
    }
  }
  progress$set(message = 'creating new netCDF file', value=0)
  PLOOP <- 1
  while (PLOOP) {
    Sys.sleep (1)
    PLOOP <- PLOOP + 1
    if (PLOOP > 250) {break}
    M <- system('tail -n 1 ../KalmanFilter/KFlog', intern=TRUE)
    if (grepl ('finished', M)) {
      PLOOP <- FALSE
      next
    }
    if (grepl ('new-netcdf', M)) {
      P <- sub ('.*new-netcdf ', '', sub ('% do.*', '', M))
      # print (sprintf ('progress is %d', as.integer(P)))
      progress$set(message = 'creating new netCDF file',
                   detail = sprintf('flight %d', Flight), value=as.integer (P))
    }
  }
  # PLOOP <- 1
  # while (PLOOP) {
  #   Sys.sleep (1)
  #   PLOOP <- PLOOP + 1
  #   if (PLOOP > 150) {break}
  #   M <- system('tail -n 1 ../KalmanFilter/KFlog', intern=TRUE)
  #   if (grepl ('finished', M)) {
  #     PLOOP <- FALSE
  #     next
  #   }
  # }
}

ShowProgressWIF <- function(progress, Flight) {
  AllDone <- ifelse (Flight == 'All', FALSE, TRUE)
  repeat {
    PLOOP <- 1
    TimeEstimate <- 600
    Fl <- 0
    P <- 0
    while (PLOOP) {
      Sys.sleep (1)
      PLOOP <- PLOOP + 1
      if (PLOOP > TimeEstimate) {break}
      M <- system('tail -n 3 newWind/newWindlog', intern=TRUE)
      print (M)
      if (length (M) < 1) {next}
      if (any (grepl ('No more', M)) || any(grepl ('All Done', M))) {
        progress$set (message = 'no more files to process', value=0)
        return ()
      }
      iw <- which (grepl ('flight is', M))
      if (length(iw) > 0) {
        Fl <- as.integer (sub ('.*rf', '', sub ('.nc.*', '', M[iw[1]])))
        P <- P +  4
        progress$set(message = 'reading data',
                     detail = sprintf('flight %d', Fl), value=P)
      }
      if (any (grepl ('calculate new', M))) {
        PLOOP <- FALSE
        P <- 30
        progress$set(message = 'calculate new variables',
                     detail = sprintf('flight %d', Fl), value=P)
      } 
    }
    PLOOP <- 1
    while (PLOOP) {
      Sys.sleep (1)
      PLOOP <- PLOOP + 1
      if (PLOOP > TimeEstimate) {break}
      M <- system('tail -n 3 newWind/newWindlog', intern=TRUE)
      print (M)
      if (any (grepl ('netCDF', M))) {
        PLOOP <- FALSE
        P <- 60
        progress$set(message = 'set up netCDF variables',
                     detail = sprintf('flight %d', Fl), value=P)
      } else {
        P <- P + 2
        progress$set(message = 'calculate new variables',
                     detail = sprintf('flight %d', Fl), value=P)
      }
    }
    PLOOP <- 1
    while (PLOOP) {
      Sys.sleep(1)
      PLOOP <- PLOOP + 1
      if (PLOOP > TimeEstimate) {break}
      M <- system('tail -n 3 newWind/newWindlog', intern=TRUE)
      print (M)
      if (any (grepl ('new-netCDF', M))) {
        PLOOP <- FALSE
        P <- 65
        progress$set (message = 'write new netCDF file',
                      detail = sprintf ('flight %d', Fl), value=P)
      }
    }
    PLOOP <- 1
    while (PLOOP) {
      Sys.sleep (1)
      PLOOP <- PLOOP + 1
      if (PLOOP > TimeEstimate) {break}
      M <- system('tail -n 3 newWind/newWindlog', intern=TRUE)
      print (M)
      if (any (grepl ('DONE', M)) || any (grepl ('Done', M))) {
        PLOOP <- FALSE
        P <- 99
        progress$set(message = 'Finished',
                     detail = sprintf('flight %d', Fl), value=P)
      } else {
        P <- P + 2
        P <- sub ('.*netCDF ', '', M[3])
        P <- sub ('%.*', '', P)
        P <- as.integer(P)
        P <- 65 + 35 * P/100
        progress$set(message = 'write new netCDF file',
                     detail = sprintf('flight %d', Fl), value=P)
      }
    }
    
    progress$set(message = '  -- DONE -- ',
                 detail = sprintf('flight %d', Fl), value=100)
    ## read again (time to write all-done message)
    M <- system('tail -n 5 newWind/newWindlog', intern=TRUE)
    if (any (grepl ('Done', M))) {
      AllDone <- TRUE
      progress$set(message = 'newWind script has finished',
                   detail = sprintf ('flight %s', Fl))
    }
    if (AllDone) {break}
  }
}

ShowProgressHOT <- function(progress, Flight) {
  PLOOP <- 1
  TimeEstimate <- 600
  while (PLOOP) {
    Sys.sleep (1)
    PLOOP <- PLOOP + 1
    if (PLOOP > TimeEstimate) {break}
    M <- system('tail -n 5 ../HeightOfTerrain/HOTlog', intern=TRUE)
    print (M)
    if (any (grepl ('download done', M))) {
      PLOOP <- FALSE
      progress$set(message = 'calc. new variables',
                   detail = sprintf('flight %d', Flight), value=80)
    } else if (any (grepl('SRTM data', M))) {
      MM <- which (grepl ('SRTM data', M))
      M <- M[MM[length(MM)]]
      P <- sub ('.*data ', '', sub ('% do.*', '', M))
      # print (sprintf ('progress is %d', as.integer(P)))
      progress$set(message = 'retrieve SRTM',
                   detail = sprintf('flight %d', Flight), value=as.integer (0.8 * as.integer (P)))
    }
  }
  PLOOP <- 1
  TimeEstimate <- 20
  while (PLOOP) {
    Sys.sleep (1)
    PLOOP <- PLOOP + 1
    if (PLOOP > TimeEstimate) {break}
    M <- system('tail -n 5 ../HeightOfTerrain/HOTlog', intern=TRUE)
    if (any (grepl ('modify netCDF', M))) {
      progress$set(message = 'write new netCDF',
                   detail = sprintf('flight %d', Flight), value=96)
      break
    }
    if (any (grepl ('find SFC', M))) {
      MM <- which (grepl ('find SFC', M))
      M <- M[MM[length(MM)]]
      P <- sub ('.*SFC ', '', sub ('% do.*', '', M))
      progress$set(message = 'calculate SFC',
                   detail = sprintf('flight %d', Flight), 
                   value=80 + as.integer (0.16 * as.integer(P)))
    }
  }
  PLOOP <- 1
  while (PLOOP) {
    Sys.sleep (1)
    PLOOP <- PLOOP + 1
    if (PLOOP > TimeEstimate) {break}
    M <- system('tail -n 5 ../HeightOfTerrain/HOTlog', intern=TRUE)
    if (any (grepl ('DONE', M))) {
      PLOOP <- FALSE
    }
  }
  progress$set(message = '  -- DONE -- ',
               detail = sprintf('flight %d', Flight), value=100)
}

runScriptWIF <- function (ssn, ipADDW) {
  print (sprintf ('entered runScriptWIF, %srf%02d', ProjectWIF, FlightWIF))
  system ('rm newWind/newWindlog')
  
  ProjectDir <- ProjectWIF
  if(grepl('HIPPO-', ProjectWIF) && singleHIPPO) {ProjectDir <- 'HIPPO'}
  if (ALLWIF) {
    Flight <- 'All'
    progress$set(message = 'read data, initialize',
                 detail = sprintf('flight %s', Flight),
                 value=0)
    cmd <- sprintf ('cd newWind;Rscript newWind.R %s %s %s %s %s %s| tee -a newWindlog',
                    ProjectWIF, Flight, ipADDW[1], ipADDW[2], ipADDW[3], ipADDW[4])
  } else if (NEXTWIF) {
    Flight <- 'NEXT'
    progress$set(message = 'read data, initialize',
                 detail = sprintf('flight %s', Flight),
                 value=0)
    cmd <- sprintf ('cd newWind;Rscript newWind.R %s %s %s %s %s %s| tee -a newWindlog',
                    ProjectWIF, Flight, ipADDW[1], ipADDW[2], ipADDW[3], ipADDW[4])
  } else {
    Flight <- FlightWIF
    progress$set(message = 'read data, initialize',
                 detail = sprintf('flight %d', Flight),
                 value=0)
    cmd <- sprintf ('cd newWind;Rscript newWind.R %s %d %s %s %s %s| tee -a newWindlog',
                    ProjectWIF, Flight, ipADDW[1], ipADDW[2], ipADDW[3], ipADDW[4])
    print (ipADDW)
  }
  system (cmd, wait=FALSE)
  ShowProgressWIF (progress, Flight)
  return()
}

runScriptHOT <- function (ssn) {
  print (sprintf ('entered runScriptHOT, %srf%02d', ProjectHOT, FlightHOT))
  if (file.exists ('../HeightOfTerrain/HOTplots/track.png')) {
    system('rm ../HeightOfTerrain/HOTplots/*png')
  }
  system ('rm ../HeightOfTerrain/HOTlog')
  
  ProjectDir <- ProjectHOT
  if(grepl('HIPPO-', ProjectHOT) && singleHIPPO) {ProjectDir <- 'HIPPO'}
  if (ALLHOT) {
    ## get list of files to process:
    Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjectDir),
                            sprintf ("%srf...nc", ProjectHOT)))
    if (!is.na (Fl[1])) {
      for (Flt in Fl) {
        FltHOT <- sub ('.nc$', 'Z.nc', Flt)
        if (file.exists (sprintf ("%s%s/%s",
                                  DataDirectory (), ProjectDir, FltHOT))) {next}
        if (file.exists ('../HeightOfTerrain/HOTplots/Track.png')) {
          system('rm ../HeightOfTerrain/HOTplots/*png')
        }
        Flight <- sub('.*rf', '', sub ('.nc$', '', Flt))
        Flight <- as.numeric (Flight)
        updateNumericInput (ssn, 'FlightHOT', value=Flight)
        progress$set(message = 'read data, initialize',
                     detail = sprintf('flight %d', Flight),
                     value=0)
        cmd <- sprintf('cd ../HeightOfTerrain;Rscript HeightOfTerrain.R %s %d | tee -a HOTlog',
                       ProjectHOT, Flight)
        # print (sprintf ('run command: %s', cmd))
        system (cmd, wait=FALSE)
        # print (sprintf ('returned from command %s', cmd))
        ShowProgressHOT (progress, Flight)
      }
    }
  } else if (NEXTHOT) {
    Flight <- getNextHOT(ProjectHOT)
    updateNumericInput (ssn, 'FlightHOT', value=Flight)
    progress$set(message = 'read data, initialize',
                 detail = sprintf('flight %d', Flight),
                 value=0)
    cmd <- sprintf('cd ../HeightOfTerrain;Rscript HeightOfTerrain.R %s %d %s | tee -a HOTlog',
                   ProjectHOT, Flight, genPlotHOT)
    system (cmd, wait=FALSE)
    ShowProgressHOT (progress, Flight)
  } else {
    progress$set(message = 'read data, initialize',
                 detail = sprintf('flight %d', FlightHOT),
                 value=0)
    cmd <- sprintf('cd ../HeightOfTerrain;Rscript HeightOfTerrain.R %s %d %s | tee -a HOTlog',
                   ProjectHOT, FlightHOT, genPlotHOT)
    system (cmd, wait=FALSE)
    ShowProgressHOT (progress, FlightHOT)
  }
  return()
}

runScript <- function (ssn) {
  if (file.exists ('../KalmanFilter/KFplots/Position.png')) {
    system('rm ../KalmanFilter/KFplots/*png')
  }
  
  ProjectDir <- ProjectKF
  if(grepl('HIPPO-', ProjectKF) && singleHIPPO) {ProjectDir <- 'HIPPO'}
  if (ALL) {
    ## get list of files to process:
    Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjectDir),
                            sprintf ("%srf...nc", ProjectKF)))
    if (!is.na (Fl[1])) {
      for (Flt in Fl) {
        FltKF <- sub ('.nc$', 'KF.nc', Flt)
        if (file.exists (sprintf ("%s%s/%s",
                                  DataDirectory (), ProjectDir, FltKF))) {next}
        if (file.exists ('~/RStudio/KalmanFilter/KFplots/Position.png')) {
          system('rm ~/RStudio/KalmanFilter/KFplots/*png')
        }
        Flight <- sub('.*rf', '', sub ('.nc$', '', Flt))
        Flight <- as.numeric (Flight)
        updateNumericInput (ssn, 'FlightKF', value=Flight)
        progress$set(message = 'read data, initialize',
                     detail = sprintf('flight %d', Flight),
                     value=0)
        cmd <- sprintf('cd ~/RStudio/KalmanFilter;Rscript KalmanFilter.R %s %d %s %s %s %d %s | tee -a KFlog',
                       Project, Flight, newAK, newSS, simple, NSTEP, genPlot)
        print (sprintf ('run commnad: %s', cmd))
        system (cmd, wait=FALSE)
        ShowProgress (NSTEP, progress, Flight)
      }
    }
  } else if (NEXT) {
    Flight <- getNext(ProjectKF)
    updateNumericInput (ssn, 'FlightKF', value=Flight)
    progress$set(message = 'read data, initialize',
                 detail = sprintf('flight %d', Flight),
                 value=0)
    cmd <- sprintf('cd ~/RStudio/KalmanFilter;Rscript KalmanFilter.R %s %d %s %s %s %d %s | tee -a KFlog',
                   ProjectKF, Flight, newAK, newSS, simple, NSTEP, genPlot)
    system (cmd, wait=FALSE)
    ShowProgress (NSTEP, progress, Flight)
  } else {
    progress$set(message = 'read data, initialize',
                 detail = sprintf('flight %d', FlightKF),
                 value=0)
    cmd <- sprintf('cd ~/RStudio/KalmanFilter;Rscript KalmanFilter.R %s %d %s %s %s %d %s | tee -a KFlog',
                   ProjectKF, FlightKF, newAK, newSS, simple, NSTEP, genPlot)
    system (cmd, wait=FALSE)
    ShowProgress (NSTEP, progress, FlightKF)
  }
  return()
}


qualifyPS <- function(fnPP, Vars, Flt) {
  if (Trace) {print (sprintf ('fnPP=%s', fnPP))}
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
  D <- qualifyData (D)
  return (D)
}

qualifyData <- function(D) {
  D$DTAS <- c(0, diff(D$TASX))
  D$DTAS <- SmoothInterp(D$DTAS, .Length=301)
  GVSPD <- SmoothInterp (c(0, diff(D$GGALT)), .Length=5)
  
  r <- (abs(GVSPD) > 1) | (D$TASX < 90) | (abs(D$DTAS) > 0.1) | (abs(D$ROLL) > 5) | 
    (D$QCXC < 60)
  r[is.na(r)] <- TRUE
  # D$PSXC[r] <- NA; D$PS_A[r] <- NA
  # D$QCRC[r] <- NA; D$QC_A[r] <- NA
  # D$AT_A[r] <- NA; D$ATX[r] <- NA
  D <- D[r == FALSE, ]
  return (D)
}

makeDataFile <- function(Proj, Flt, Vars) {
  ProjDir <- Proj
  if(grepl('HIPPO-', Proj) && singleHIPPO) {ProjDir <- 'HIPPO'}
  if (Flt == 'ALL') {
    Ft <- 1
  } else {
    Ft <- Flt
  }
  if (Flt != 'ALL' && checkBad(sprintf ('%srf%02d', Proj, Ft))) {
    print ('bad flight -- skipping')
    return(data.frame())
  }
  fname <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), ProjDir, Proj, Ft)
  if (!file.exists (fname)) {
    fname <- sprintf ('%s%s/%stf%02d.nc', DataDirectory(), ProjDir, Proj, Ft)
  }
  if (Trace) {print (sprintf ('fnameG=%s', fname))}
  suppressMessages (suppressWarnings (
    FI <- DataFileInfo (fname, LLrange=FALSE)
  ))
  
  subVar <- function (AL, FI) {
    RTN <- NA
    for (i in 1:length(AL)) {
      if (AL[i] %in% FI$Variables) {
        RTN <- AL[i]
        break
      }
    }
    return (RTN)
  }
  
  ## variable substitutions
  VarsS <- Vars
  ALTGGVSPD <- c('GGVSPD', 'GGVSPDB', 'VSPD_A', 'VSPD_G')
  ALTATH1 <- c('ATH1', 'ATHR1', 'ATHL1')
  ALTATF1 <- c('ATF1', 'ATFH1')
  ALTATH2 <- c('ATH2', 'ATHR2', 'ATHL2')
  ALTATF2 <- c('ATF2', 'ATFH2')
  if (ALTGGVSPD[1] %in% Vars) {
    GVSPD <- subVar (ALTGGVSPD, FI)
    Vars[which(Vars == ALTGGVSPD[1])] <- GVSPD
  }
  if (ALTATH1[1] %in% Vars) {
    ATH1 <- subVar (ALTATH1, FI)
    Vars[which(Vars == ALTATH1[1])] <- ATH1
  }
  if (ALTATH2[1] %in% Vars) {
    ATH2 <- subVar (ALTATH2, FI)
    Vars[which(Vars == ALTATH2[1])] <- ATH2
  }
  if (ALTATF1[1] %in% Vars) {
    ATF1 <- subVar (ALTATF1, FI)
    Vars[which(Vars == ALTATF1[1])] <- ATF1
  }
  if (ALTATF2[1] %in% Vars) {
    ATF2 <- subVar (ALTATF2, FI)
    Vars[which(Vars == ALTATF2[1])] <- ATF2
  }
  
  if (Flt == 'ALL') {
    ## loop through all the flights in this project:
    Flrf <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjDir),
                              sprintf ("%srf...nc$", Proj)))
    Fltf <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjDir),
                              sprintf ("%stf...nc$", Proj)))
    D <- data.frame()
    if (!is.na (Fltf[1])) {
      ## eliminate any variables that are not present for all flights:
      FltPP1 <- as.integer (sub('.*tf', '', sub ('.nc$', '', Fltf)))
      for (Ft in FltPP1) {
        if (checkBad (Ftemp <- sprintf ('%stf%02d', Proj, Ft))) {
        } else {
          FII <- DataFileInfo (sprintf ('%s%s/%stf%02d.nc', DataDirectory(), ProjDir, 
                                        Proj, Ft), LLrange=FALSE)
          im <- match (Vars, FII$Variables)
          if (any(is.na(im))) {
            Vars <- Vars[-which(is.na(im))]
          }
        }
      }
    }
    if ((length(Flrf) > 0) && !is.na (Flrf[1])) {
      ## eliminate any variables that are not present for all flights:
      FltPP2 <- as.integer (sub('.*rf', '', sub ('.nc$', '', Flrf)))
      for (Ft in FltPP2) {
        if (checkBad (Ftemp <- sprintf ('%srf%02d', Proj, Ft))) {
        } else {
          FII <- DataFileInfo (sprintf ('%s%s/%srf%02d.nc', DataDirectory(), ProjDir, 
                                        Proj, Ft), LLrange=FALSE)
          im <- match (Vars, FII$Variables)
          if (any(is.na(im))) {
            Vars <- Vars[-which(is.na(im))]
          }
        }
      }
    }
    for (Ft in FltPP1) {
      if (Trace) {print (sprintf ('ProjDir %s Proj %s Ft %d', ProjDir, Proj, Ft))}
      if (checkBad(Ftemp <- sprintf ('%stf%02d', Proj, Ft))) {
        print (sprintf('bad flight %s -- skipping', Ftemp))
        next
      }
      fname <- sprintf ('%s%s/%stf%02d.nc', DataDirectory(),
                        ProjDir, Proj, Ft)
      D <- rbind (D, getNetCDF (fname, Vars, F=Ft+50))
    }
    if (exists ('FltPP2')) {
      for (Ft in FltPP2) {
        if (Trace) {print (sprintf ('ProjDir %s Proj %s Ft %d', ProjDir, Proj, Ft))}
        if (checkBad(Ftemp <- sprintf ('%srf%02d', Proj, Ft))) {
          print (sprintf('bad flight %s -- skipping', Ftemp))
          next
        }
        fname <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(),
                          ProjDir, Proj, Ft)
        D <- rbind (D, getNetCDF (fname, Vars, F=Ft))
      }
    }
  }
  # Fltf <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjDir),
  #   sprintf ("%stf...nc$", Proj)))
  # if (!is.na(Fltf[1])) {
  #   for (Ft in Fltf) {
  #     FltPP <- sub('.*tf', '', sub ('.nc$', '', Ft))
  #     FltPP <- as.integer (FltPP)
  #     if (checkBad(Ftemp <- sprintf ('%stf%02d', Proj, FltPP))) {
  #       print (sprintf('bad flight %s -- skipping', Ftemp))
  #       next
  #     }
  #     fname <- sprintf ('%s%s/%stf%02d.nc', DataDirectory(),
  #       ProjDir, Proj, FltPP)
  #     D <- rbind (D, getNetCDF (fname, Vars, F=FltPP+50))
  #   }
  # } else {
  #   D <- getNetCDF(fname, Vars, F=Ft)
  # }
  ## rename if necessary
  N <- names (D)
  if (!(ALTGGVSPD[1] %in% N) && match (ALTGGVSPD, N, nomatch=0)) {
    N[which(N == GVSPD)] <- ALTGGVSPD[1]
  }
  if (!(ALTATH1[1] %in% N) && match (ALTATH1, N, nomatch=0)) {
    N[which(N == ATH1)] <- ALTATH1[1]
  }
  if (!(ALTATH2[1] %in% N) && match (ALTATH2, N, nomatch=0)) {
    N[which(N == ATH2)] <- ALTATH2[1]
  }
  if (!(ALTATF1[1] %in% N) && match (ALTATF1, N, nomatch=0)) {
    N[which(N == ATF1)] <- ALTATF1[1]
  }
  if (!(ALTATH1[1] %in% N) && match (ALTATH1, N, nomatch=0)) {
    N[which(N == ATF2)] <- ALTATF2[1]
  }
  names(D) <- N
  return (D)
}


nplots <- c(1, 3:17, 19:23)    # project default
# The pairs of items in psq refer to the plot routine and the
# separate plots generated by that routine. The "plot" control
# refers to the sum up to that point, so for example RPlot7.R
# generates the two plots numbered 10 and 11.
# The "dpan" array contains the number of panels in each plot,
# so for example pan[11] refers to the number of panels generated
# for the second plot produced by RPlot7.R
psq <- c(1,1, 1,2, 3,1, 4,1, 5,1, 5,2, 5,3, 5,4, 6,1, 7,1, 7,2,  #11
         8,1, 9,1, 9,2, 10,1, 10,2, 11,1, 12,1, 13,1, 14,1, 15,1, 15,2, #22
         16,1, 16,2, 16,3, 17,1, 19,1, 19,2, #28
         20,1, 20,2, 20,3, 20,4, 21,1, 21,2, 21,3, 21,4, #36
         22,1, 22,2, 22,3, 22,4, 23,1, 23,2, #42
         24,1, 25,1, 26,1, 27,1, 28,1, 29,1, 30,1) #49
dpan <- c(1, 3, 2, 1, 2, 1, 2, 3, 3, 3, 3, #11
          2, 3, 2, 3, 2, 4, 3, 3, 4, 2, 2,        #22
          3, 3, 3, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  # 40
          4, 1, 2, 1, 1, 1, 1, 1, 1)  #49
PlotTypes <- c(      
  'flight track' = 1,
  'altitude, heading'= 2,
  'temperature' = 3,
  'humidity' = 5,
  'static pressure' = 9,
  'dynamic pressure' = 10,
  'airspeed, Mach' = 11,
  'total pressure' = 12,
  'wind' = 13,
  'angles, radome' = 17,
  'IRU comparisons' = 18,
  'radiation' = 20,
  'concentrations' = 21, 
  'instr. monitor' = 22,
  'LWC, dbar' = 23,
  'instr. checks' = 25,
  'skew-T' = 26,
  'potential T' = 27,
  'droplet size dist.' = 29,
  'particle size dist.' = 33,
  'precip size dist.' = 37,
  'trace gases' = 41,
  'extras' = 43
)
L <- length (psq)/2
dim(psq) <- c(2,L)
netCDFfile <- NULL
CCDP <- NULL
CFSSP <- NULL
CUHSAS <- NULL
C1DC <- NULL

testPlot <- function (k) {
  return(k %in% nplots || nplots == 0)
}

## assemble a list of projects for which an appropriately named rf01 or tf01
## exists in the data directory:

CHP <- c('recovery factor', 'angle of attack', 'airspeed dependence')
DataDir <- DataDirectory ()

## Test that there is an entry in the Configuration.R file for PJ,
## and eliminate any for which there is no Configuration entry:
lines <- readLines ('Configuration.R')
for (P in PJ) {
  if (!any (grepl (P, lines))) {PJ[PJ == P] <- NA}
}
PJ <- PJ[!is.na(PJ)]
rm (lines)

times <- c(as.POSIXct(0, origin='2012-05-29', tz='UTC'),
           as.POSIXct(3600*8, origin='2012-05-29', tz='UTC'))

## make plot functions available
# for (np in 1:2) {
#   if (testPlot(np)) {
#     eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
#   }
# }
# for (np in 3:30) {
#   if (file.exists (sprintf ("./PlotFunctions/RPlot%d.R", np))) {
#     eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
#   }
# }
# functions used later:
hline <<- function(y, col='black', lwd=1, lty=2) {
  abline(h=y, col=col, lwd=lwd, lty=lty)
}

formatTime <- function (time) {
  t <- as.POSIXlt (time)
  tt <- sprintf ("%d:%02d:%02d", t$hour, t$min, t$sec)
  return (tt)
}

saveConfig <- function() {
  print ('entered saveConfig')
  file.copy('Configuration.R','Configuration.R.backup', overwrite=TRUE)
  lines <- readLines ('Configuration.R')
  unlink ('Configuration.R')
  startLine <- which (grepl (sprintf ('Project == "%s"', Project), lines))
  endLine <- which (grepl ('Project', lines[(startLine+1):length(lines)]))[1]
  if (is.na(endLine)) {
    endLine <- length (lines)
  } else {
    endLine <- endLine + startLine - 1
    # print (sprintf ('start and end lines are %d %d', startLine, endLine))
  }
  linesOut <- lines[1:startLine]
  while (grepl ('offset',lines[startLine+1])) {
    startLine <- startLine + 1
    linesOut <- c(linesOut, lines[startLine])
  }
  v <- VRPlot$PV1
  linesOut <- c(linesOut, paste('  VRPlot <- list(PV1 = ',list(v), ')', sep=''))
  for (i in 2:length(VRPlot)) {
    v <- as.character(VRPlot[[i]])
    if (i %in% c(15:16,20:22)) {
      v <- sub ('_.*', '_', v)
    }
    if (length (v) > 1) {
      linesOut <- c(linesOut, 
                    paste(sprintf ('  VRPlot$PV%d <- ',i),list(v),sep=''))
    } else {
      if (is.na(v) || (length (v) == 0)) {
        linesOut <- c(linesOut, sprintf ('  VRPlot$PV%d <- c(NA)', i))
      } else {
        linesOut <- c(linesOut, sprintf ('  VRPlot$PV%d <- "%s"', i, v))
      }
    }
  }
  linesOut <- c(linesOut, '}\n ')
  linesOut <- c(linesOut, lines[endLine:(length(lines))])
  writeLines(linesOut, 'Configuration.R')
  print (sprintf ('saved configuration for %s in Configuration.R', Project))
  source ('Configuration.R')
  print (str(VRPlot))
}

source('savePDF.R')

savePNG <- function(Data, inp) {
  print ('entered savePNG')
  #   plotfile = sprintf("%s%sPlots.pdf", inp$Project, inp$Flight)
  #   unlink (plotfile)
  # cairo_pdf (filename = plotfile, onefile=TRUE)
  ## enable something like the next to get individual png files instead of one large pdf
  png (file = sprintf ("./PNG/%s%s%02dPlot%%02d.png", inp$Project, 
                       inp$typeFlight, inp$Flight), width=800, height=800)
  print (sprintf ("saving png plots to subdirectory PNG"))
  DataV <- limitData (Data, inp)
  t1 <- times[1]
  t <- as.POSIXlt (t1)
  StartTime <<- as.integer (10000*t$hour+100*t$min+t$sec)
  DataV <- DataV[(DataV$Time > times[1]) & (DataV$Time < times[2]), ]
  ## transfer the attributes to DataV (for now, main use is CDP sizes)
  DataV <- transferAttributes (DataV, Data)  
  for (np in 1:30) {
    if (file.exists (sprintf ("./PlotFunctions/RPlot%d.R", np))) {
      if (testPlot(np) && (length(VRPlot[[np]]) > 0)) {
        print(paste('Plot',np))
        ## eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
        if (np == 1) {
          RPlot1 (DataV, sprintf ('%s%02d', inp$typeFlight, inp$Flight))
        } else {
          eval(parse(text=sprintf("RPlot%d(DataV)", np)))
        }
      }
    }
  }
  dev.off()
  #   suppressWarnings(if (length (system ('which evince', intern=TRUE)) > 0) {
  #     system (sprintf ('evince %s', plotfile))
  #   })
  print ('finished savePNG')
}

saveRdata <- function (Data, inp) {
  print ('entered saveRdata')
  netCDFfile <- nc_open (sprintf ('%s%s/%s%s%02d.nc', DataDirectory (),
                                  inp$Project, inp$Project, inp$typeFlight,
                                  inp$Flight))
  nms <- c('Time', 'TASX')
  Time <- ncvar_get (netCDFfile, "Time")
  TASX <- ncvar_get (netCDFfile, "TASX")
  time_units <- ncatt_get (netCDFfile, "Time", "units")
  tref <- sub ('seconds since ', '', time_units$value)
  Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
  namesCDF <- names (netCDFfile$var)
  if (length (grep ("CCDP_", namesCDF)) > 0) {
    nm <- namesCDF[grepl("^CCDP_", namesCDF)]
    nms <- c(nms, 'CCDP')
    CCDP <- ncvar_get (netCDFfile, nm)
    CellSizes <- ncatt_get (netCDFfile, nm, "CellSizes")
    CellLimitsD <- CellSizes$value
    attr (CCDP, 'CellLimits') <- CellLimitsD
  }
  if (length (grep ("CS100_", namesCDF)) > 0) {
    nm <- namesCDF[grepl("^CS100_", namesCDF)]
    nms <- c(nms, 'CS100')
    CFSSP <- ncvar_get (netCDFfile, nm)
    CellSizes <- ncatt_get (netCDFfile, nm, "CellSizes")
    CellLimitsF <- CellSizes$value
    attr (CFSSP, 'CellLimits') <- CellLimitsF
  }
  if (length (grep ("CUHSAS_", namesCDF)) > 0) {
    nm <- namesCDF[grepl("^CUHSAS_", namesCDF)]
    nms <- c(nms, 'CUHSAS')
    CUHSAS <- ncvar_get (netCDFfile, nm)
    CellSizes <- ncatt_get (netCDFfile, nm, "CellSizes")
    CellLimitsU <- CellSizes$value
    attr (CUHSAS, 'CellLimits') <- CellLimitsU
  }
  if (length (grep ("CPCASP_", namesCDF)) > 0) {
    nm <- namesCDF[grepl("^CPCASP_", namesCDF)]
    nms <- c(nms, 'CPCASP')
    CUHSAS <- ncvar_get (netCDFfile, nm)
    CellSizes <- ncatt_get (netCDFfile, nm, "CellSizes")
    CellLimitsP <- CellSizes$value
    attr (CUHSAS, 'CellLimits') <- CellLimitsP
  }
  if (length (grep ("C1DC_", namesCDF)) > 0) {
    nm <- namesCDF[grepl("^C1DC_", namesCDF)]
    nms <- c(nms, 'C1DC')
    C1DC <- ncvar_get (netCDFfile, nm)
    CellSizes <- ncatt_get (netCDFfile, nm, "CellSizes")
    CellLimits <- CellSizes$value
    attr (C1DC, 'CellLimits') <- CellLimits
  }
  fn <- sprintf ('%s%s/%s%s%02d.Rdata', DataDirectory (),
                 inp$Project, inp$Project, inp$typeFlight,
                 inp$Flight)
  size.distributions <- mget (nms)
  save (Data, size.distributions, file=fn)
  print (sprintf ('saved data.frame and size distributions to %s', fn))
}

SeekManeuvers <- function (Data) {
  source ("./PlotFunctions/SpeedRunSearch.R")
  source ("./PlotFunctions/CircleSearch.R")
  source ("./PlotFunctions/PitchSearch.R")
  source ("./PlotFunctions/YawSearch.R")
  source ("./PlotFunctions/ReverseHeadingSearch.R")
  print ('list of maneuvers:')
  PitchSearch (Data)
  YawSearch (Data)
  SpeedRunSearch (Data)
  CircleSearch (Data)
  ReverseHeadingSearch (Data)
  print ('end of maneuver list')
}

seeManual <- function () {
  URL <- 'QAtoolsUserGuide.pdf'
  # Use next line to get the repository version instead:
  # URL <- 'https://github.com/NCAR/aircraft_QAtools/blob/main/QAtoolsUserGuide.pdf'
  # browseURL(URL)
  viewer <- getOption ("viewer")
  viewer ('QAtoolsUserGuide.pdf', height='maximize')
}


## get VRPlot and chp/shp:
## load a starting-point version
Project <- 'TI3GER'
source('loadVRPlot.R')

load ('CalibrationExercise/CalData.Rdata')

chooseQVar <- function (fname, inp) {
  quickPlotVar <<- setVariableList (fname, single=TRUE)
}

options("digits"=4)
SummarizeFit <- function(ft) {
  options("digits"=4)
  # print (summary(ft)$call)
  print ("Coefficients:")
  print (summary(ft)$coefficients)
  print (sprintf ("Residual standard deviation: %.3f, dof=%d<br>", summary(ft)$sigma, summary(ft)$df[2]))
  print (sprintf ("R-squared %.3f", summary(ft)$r.squared))
}
with (CalData, {
  fm1 <<- lm (M ~ x);
  fm2 <<- lm (x ~ M);
  fm3 <<- lm (M ~ x + I(x^2));
  fm4 <<- lm (x ~ M + I(M^2))
})
cf1 <- coef(fm1)
cf2 <- coef(fm2)
cf3 <- coef(fm3)
cf4 <- coef(fm4)


fn <- sprintf ('%s%s/%srf01.nc', DataDirectory (), Project, Project)
if (!file.exists (fn)) {
  fn <- sub ('\\.nc', '.Rdata', fn)
}
if (!file.exists (fn)) {
  fn <- sprintf ('%s%s/%stf01.nc', DataDirectory (), Project, Project)
}
if (!file.exists (fn)) {
  fn <- sub ('\\.nc', '.Rdata', fn)
}
if (!file.exists (fn)) {warning ('need tf01 or rf01 to initialize')}
# if (Trace) {print (sprintf ('fnG=%s', fn))}
FI <- DataFileInfo (fn, LLrange=FALSE)

limitData <- function (Data, input) {
  DataV <- Data
  namesV <- names(DataV)
  namesV <- namesV[namesV != "Time"]
  if (input$limits) {
    t <- !is.na (DataV$TASX) & (DataV$TASX < input$minTAS)
    t <- t | (abs(DataV$ROLL) > input$maxROLL)
    t <- t | (DataV$GGALT/1000 < input$minZ)
    t <- t | (DataV$VSPD > input$maxROC)
    t[is.na(t)] <- FALSE
    DataV[t, namesV] <- NA
  }
  return (DataV)
}

makeVRPlot <- function (slp, psq) {
  VR <- list(PV1=c(slp[[1]], slp[[2]]))
  VR$PV2 <- VR$PV1
  for (j in 3:30) {
    V <- vector()
    for (i in j:(length(psq)/2)) {
      if (psq[1,i] == j) {
        V <- (c (V, as.vector(slp[[i]])))
      }
    }
    if (j %in% 15:16) {
      V <- sub ('_.*$', '_', V)
    }
    VR[j] <- list(V)
  }
  names(VR) <- sprintf ('PV%d', 1:30)
  return (VR)
}

## DPcheck additions:
VRPlot <- loadVRPlot(Project, FALSE, 1, psq)

DataHR <- data.frame()

VarListTDP <- standardVariables (c('CAVP_DPL', 'BALNC_DPL', 'MIRRTMP_DPL', 'DP_DPL', 'DP_VXL', 'EW_DPL', 'EW_VXL', 'ATX'))
setNA <- function (.x, .v) {
  X <- zoo::na.approx (as.vector (.x), maxgap=1000, na.rm=FALSE)
  X[is.na(X)] <- .v
  return (X)
}
saveICEvents <- function () {
  save(BadCloudEvents, file='inCloud/BadCloudEvents.Rdata')
}
if (file.exists('BadCloudEvents.Rdata')) {
  load('BadCloudEvents.Rdata')
  if (nrow(BadCloudEvents) > 0) {
    chIC <- vector('character', nrow(BadCloudEvents))
    for (i in 1:nrow(BadCloudEvents)) {
      u <- ifelse (BadCloudEvents$Rej[i], 'Y', 'N')
      print (s <- sprintf ('%s %d %s rf%02d %s-%s', u, i, BadCloudEvents$Project[i], BadCloudEvents$Flight[i],
                           formatTime(BadCloudEvents$Start[i]), formatTime(BadCloudEvents$End[i])))
      chIC[i] <- sprintf ('%d', i)
      names(chIC)[i] <- s
    }
  } else {
    chIC <- 'none'
  }
} else {
  chIC <- 'none'
}
itmL <- 0
addDPERR <- function (Data) {
  ## from the VXL vapor pressure, predict the DPL mirror temperature
  Data$ecav <- Data$EW_VXL * Data$CAVP_DPL / Data$PSXC
  mfind <- function (t, pr, e) {
    return (abs(MurphyKoop(t, pr) - e))
  }
  Data$MT_DPL <- rep(0, nrow(Data))
  # for (i in 1:nrow(Data)) {
  #   if (!is.na(Data$ecav[i]) && !is.na(Data$DP_DPL[i]) && !is.na(Data$PSXC[i]) && !is.na(Data$CAVP_DPL[i])) {
  #     Data$MT_DPL[i] <- nlm (mfind, Data$DP_DPL[i], Data$PSXC[i], Data$ecav[i])$estimate
  #   } else {
  #     Data$MT_DPL[i] <- NA
  #   }
  # }
  ftest <- function (D) {return (nlm (f=mfind,p= D$DP_DPL, pr=D$PSXC, e=D$ecav)$estimate)}
  # ftest <- function (D) {return (newtn (FUNC=mfind, X0=D$DP_DPL, pr=D$PSXC, e=D$ecav))}
  rej <- with(Data, is.na(DP_DPL) | is.na(PSXC) | is.na(ecav) | is.na(CAVP_DPL))
  Data$DP_DPL <- setNA (Data$DP_DPL, 0)
  Data$PSXC <- setNA (Data$PSXC, 1000)
  Data$ecav <- setNA (Data$ecav, 1.e-3)
  Data$CAVP_DPL <- setNA (Data$CAVP_DPL, 500)
  Data$MT_DPL <- plyr::adply(Data, .margins=1, .fun=ftest, .expand=FALSE)$V1
  Data$MT_DPL[rej] <- NA
  Data$R1 <- c(0,diff(Data$MIRRTMP_DPL))
  Data$R2 <- c(0, diff (Data$MT_DPL))
  Data$DDPL <- Data$MT_DPL - Data$MIRRTMP_DPL
  # Data$DDPL <- Data$DP_VXL-Data$MIRRTMP_DPL
  Data$DDPL[is.na(Data$DDPL)] <- 0
  Data$CDDPL <- 10 * zoo::rollmean (Data$DDPL, 30, fill=0, align='right')
  Data$R1[is.na(Data$R1)] <- 0
  Data$RM1 <- zoo::rollmean (Data$R1, 120, fill=0, align='right')
  
  ## try to simulate MIRRTMP_DPL on the basis of MT_DPL response?
  MS <- 0; DS <- 0
  Data$TSIM <- rep(0, nrow(Data))
  hlim <- 0.75; clim <- -1
  for (i in 1:nrow(Data)) {
    if (!is.na(Data$MT_DPL[i])) {
      DS <- f1simTDP * DS + f2simTDP * (Data$MT_DPL[i] - MS)
      if (DS > 0) {
        if (asimTDP * DS < hlim) {
          MS <- MS + asimTDP * DS
        } else {
          MS <- MS + hlim
        }
      } else {
        if (asimTDP * DS > clim) {
          MS <- MS + asimTDP * DS
        } else {
          MS <- MS + clim
        }
      }
    }
    Data$TSIM[i] <- MS
  }
  return(Data)
}
asimTDP <- 0.14
f1simTDP <- 0.97; f2simTDP <- 1 - f1simTDP

constructDQF <- function (project, flight) {
  projectDir <- project
  if(grepl('HIPPO-', project) && singleHIPPO) {projectDir <- 'HIPPO'}
  Data <- getNetCDF (sprintf ('%s%s/%srf%02d.nc', DataDirectory(), projectDir, project, flight), VarListTDP)
  Data$MIRRTMP_DPL <- setNA (Data$MIRRTMP_DPL, 0)
  Data$DERIV2 <- -signal::filter(signal::sgolay(3,17,2),Data$MIRRTMP_DPL)
  Data$DPERR <- zoo::rollmean (setNA (abs(Data$DERIV2) / 
                                        (asimTDP * f2simTDP), 0), 60, fill=0, align='center')
  Data$CBAL <- zoo::rollmean (setNA (Data$BALNC_DPL, 0), 60, fill=0, align='center') / 500
  Data$DPLQUAL <- ifelse (abs(Data$CBAL) > 3, -10, 0)
  Data$DPLQUAL[abs(Data$CBAL) > 10] <- -20
  ## find candidates for overshooting, but skip if data.frame already exists:
  fileDQF <- sprintf ('Problems/DQF%srf%02d.Rdata', project, flight)
  if (file.exists(fileDQF)) {
    load(fileDQF)
  } else {
    DQF <- data.frame ()
  }
  i1 <- i2 <- 1
  iL <- nrow(Data)
  Elim=20
  while (!is.na(i1)) {
    i1 <- which(Data$DPERR[i2:iL] > Elim & Data$TASX[i2:iL] > 130)[1]+i2-1
    if (is.na(i1)) {break}
    i2 <- which (Data$DPERR[i1:iL] <= Elim)[1]-1+i1
    if (mean(Data$MIRRTMP_DPL[i1:i2], na.rm=TRUE) > -20) {
      print (sprintf ('overshoot candidate %s--%s', 
                      Data$Time[i1], Data$Time[i2]))
      DQF <- rbind (DQF, data.frame(Start=Data$Time[i1], End=Data$Time[i2], 
                                    qfStart=Data$Time[i1], qfEnd=Data$Time[i2], 
                                    Use=FALSE, Flag=0, Type='overshoot'))
      # with(Data[(i1-120):(i2+120),], plotWAC (data.frame (Time, MIRRTMP_DPL, MT_DPL,
      #                                                     DPERR, ATX)))
      # abline(h=15, lty=3, lwd=2, col='magenta')
      # abline(v=Data$Time[i1], lwd=0.5, lty=2); abline(v=Data$Time[i2], 
      #                                                 lwd=0.5, lty=2)
    }
  }
  ## add supersaturation events:
  i1 <- i2 <- 1
  iL <- nrow(Data)
  Data$SS <- Data$DP_DPL - Data$ATX
  while (!is.na(i1)) {
    i1 <- which(Data$SS[i2:iL] > 5 & Data$TASX[i2:iL] > 100)[1]+i2-1
    if (is.na(i1)) {break}
    i2 <- which (Data$SS[i1:iL] <= 2)[1]-1+i1
    if (mean(Data$MIRRTMP_DPL[i1:i2], na.rm=TRUE) > -20) {
      print (sprintf ('supersaturation candidate %s--%s', 
                      Data$Time[i1], Data$Time[i2]))
      DQF <- rbind (DQF, data.frame(Start=Data$Time[i1], End=Data$Time[i2], 
                                    qfStart=Data$Time[i1], qfEnd=Data$Time[i2], 
                                    Use=FALSE, Flag=0, Type='supersaturation'))
    }
  }
  ## sort and remove duplicates, keeping 1st (from file)
  DQF <- DQF[with(DQF, order(Start,End)), ]
  DQF <- DQF[!duplicated(DQF[,1:2]),]
  DQF <<- DQF
  DQFsave <<- DQF
}

getDataTDP <- function (project, flight, typeFlight) {
  projectDir <- project
  if(grepl('HIPPO-', project) && singleHIPPO) {projectDir <- 'HIPPO'}
  # if (grepl ('HIPPO', project)) {projectDir <- 'HIPPO'}
  DataTDP <- getNetCDF (sprintf ('%s%s/%s%s%02d.nc', DataDirectory(), projectDir, project, 
                                 typeFlight, flight), VarListTDP)
  DataTDP$MIRRTMP_DPL <- setNA (DataTDP$MIRRTMP_DPL, 0)
  DataTDP$DERIV2 <- -signal::filter(signal::sgolay(3,17,2),DataTDP$MIRRTMP_DPL)
  DataTDP$DPERR <- DataTDP$DERIV2 / (asimTDP * f2simTDP)
  DataTDP$CBAL <- zoo::rollmean (setNA (DataTDP$BALNC_DPL, 0), 60, fill=0, align='right') / 500
  DataTDP$DPLQUAL <- ifelse (abs(DataTDP$CBAL) > 3, -10, 0)
  DataTDP$DPLQUAL[abs(DataTDP$CBAL) > 10] <- -20
  DataTDP <<- DataTDP
}
if (Trace) {toc()}
