 
PJ <- c('ARISTO2017', 'ORCAS', 'CSET', 'HCRTEST',
        'DEEPWAVE', 'CONTRAST', 'MPEX', 'DC3',
        'TORERO', 'HIPPO-5', 'HIPPO-4', 'PREDICT')
PJ <- c('DC3',
        'TORERO', 'HIPPO-5', 'HIPPO-4', 'PREDICT')
library(Ranadu)
source ("./PlotFunctions/SpeedRunSearch.R")
source ("./PlotFunctions/CircleSearch.R")
source ("./PlotFunctions/PitchSearch.R")
source ("./PlotFunctions/YawSearch.R")
source ("./PlotFunctions/ReverseHeadingSearch.R")
SeekManvrs <- function (Data) {
  # print ('list of maneuvers:')
  lst <- vector('character')
  lt <- PitchSearch (Data)
  if (!is.na(lt[1]))  {lst <- lt}
  lt <- YawSearch (Data)
  if (!is.na(lt[1])) {lst <- c(lst, lt)}
  lt <- SpeedRunSearch (Data) 
  if (!is.na(lt[1])) {lst <- c(lst, lt)}
  lt <- CircleSearch (Data)
  if (!is.na(lt[1])) {lst <- c(lst, lt)}
  lt <- ReverseHeadingSearch (Data)
  if (!is.na(lt[1])) {lst <- c(lst, lt)}
  # print ('end of maneuver list')
  return (lst)
}
ProjectSeekManeuvers <- function (Project) {
  if (grepl ('HIPPO', Project)) {
    ProjDir <- 'HIPPO'
  } else {
    ProjDir <- Project
  }
  # print (ProjectPP)
  Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjDir),
                          sprintf ("%srf...nc$", Project)))
  Fl <- c(Fl, sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjDir),
                                sprintf ("%stf...nc$", Project))))
  Fl <- c(Fl, sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjDir),
                                sprintf ("%sff...nc$", Project))))
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
                           standardVariables (c('PITCH', 'SSRD', 'THDG', 'ROLL')))
        lt <- SeekManvrs (Data)
        if(!is.na(lt[1])) {lst <- c(lst,lt)}
      }
    }
    print (lst)
    print (sprintf ('End of list for project %s', Project))
    save(lst, file=sprintf('maneuvers/maneuvers%s', Project))
  }
}

for (Project in PJ) {
  ProjectSeekManeuvers (Project)
}

# lstAll <- vector('character')
# for (Project in PJ) {
#   fn <- sprintf('maneuvers/maneuvers%s', Project)
#   load(fn)
#   lstAll <- c(lstAll, lst)
# }
# 
# lstAll <- gsub('  *', ';', lstAll)
# sink(file='ManeuversGV.csv')
# print (lstAll)
# sink()