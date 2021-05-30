e# Construct Configure.R for a new project

# Start with a sample netCDF file.
# Specify the new project here; 'WECAN' should be changed to the new project.
NewProject <- 'SPICULE'
Flight <- 'tf01'  # Specify the model netCDF file
fname <- sprintf ('%s%s/%s%s.nc', Ranadu::DataDirectory(),
                  NewProject, NewProject, Flight)
FI <- Ranadu::DataFileInfo (fname)
# get all variables to be able to reference attributes
Data <- Ranadu::getNetCDF(fname, 'ALL') 

# Now read the existing Configuration.R and, for the required platform,
# find all variables that have been used for each plot:

PJC130 <- c('WECAN',
  'WECAN-TEST',
  'WINTER',
#  'NOMADSS',
  'FRAPPE')
PJGV  <- c(
  'ECLIPSE2019',
  'OTREC-TEST',
  'SOCRATES',
  'ECLIPSE',
  'ARISTO2017',
  'ORCAS',
  'CSET',
  'NOREASTER',
  'HCRTEST',
  'DEEPWAVE',
  'CONTRAST',
  'SPRITE-II',
  'MPEX',
  'DC3',
  'TREX',
  'PACDEX',
  'START08',
  'TORERO',
  'HIPPO-1',
  'HIPPO-2',
  'HIPPO-3',
  'HIPPO-4',
  'HIPPO-5',
  'PREDICT',
  'HEFT10',
  'IDEAS-4',
  'DC3-TEST'
)

Platform <- 'GV'
Platform <- ifelse (grepl ('130', FI$Platform), 'C130', 'GV')

if (Platform == 'C130') {
  PJ <- PJC130
} else {
  PJ <- PJGV
}
rm('VRX')
rm('VRPlot')
for (Project in PJ) {
  if (Project == NewProject) {next}  # when testing, don't use existing config.
  source ('./Configuration.R')  # Must do this repeatedly because VRPlot is loaded
                                # to match Project.
  if (exists ('VRX')) {
    for (i in 1:length(VRPlot)) {
      if (!is.na(VRPlot[[i]][1])) {
        VRX[[i]] <- unique (c(VRPlot[[i]], VRX[[i]]))
      }
    }
  } else {
    VRX <- VRPlot
  }
  if ('ATX' %in% VRX[[23]]) {print (sprintf('project %s', Project))}
}
# eliminate NAs
for (i in 1:length(VRX)) {
  VRX[[i]] <- c(VRX[[i]][!is.na(VRX[[i]])])
}

# Complete items that end with underscore:
# (This replaces an entry like CONCD_ with the appropriate variable(s).)
# The variable like CONCD_ is left, but it will be removed at the end in
# the test for matches to the variables in the provided netCDF file.
for (i in 1:length(VRX)) {
  for (j in 1:length(VRX[[i]])) {
    if (grepl ('.*_$', VRX[[i]][j])) {
      if (any (grepl (VRX[[i]][j], FI$Variables))) {
        V <- FI$Variables[grepl (VRX[[i]][j], FI$Variables)]
        V <- V[grepl(sprintf ('^%s', VRX[[i]][j]), V)]
        VRX[[i]] <- unique (c(VRX[[i]], V))
      }
    }
  }
}

# VRX[[1]] and VRX[[2]] are always the same and will already be present,
# as c("LATC", "LONC",  "WDC", "WSC", "GGALT", "PALT",  "PSXC")

# RPlot3 and RPlot4: temperature
# Add appropriate measurands from the attributes:
VRX[[3]] <- unique (c(VRX[[3]], FI$Measurands$air_temperature))
VRX[[3]] <- VRX[[3]][-which ('ATX' == VRX[[3]])]  # don't include ATX
# but put variable that is ATX first because it is reference used for others
firstT <- sub('. ', '', attr(Data$ATX, 'Dependencies'))
VRX[[3]] <- unique(c(firstT, VRX[[3]]))
# Differences in RPlot3.R and RPlot4.R are calculated relative to first in list
VRX[[4]] <- VRX[[3]]

# RPlot5: humidity
VRX[[5]] <- unique (c(FI$Measurands$dew_point_temperature, VRX[[5]]))
VRX[[5]] <- VRX[[5]][-which ('DPXC' == VRX[[5]])]  # don't include DPXC
# Place variable used for DPXC first
firstDP <- sub('. ', '', attr(Data$DPXC, 'Dependencies'))
VRX[[5]] <- unique (c(firstDP, VRX[[5]]))
VRX[[5]] <- unique (c(VRX[[5]], FI$Measurands$water_vapor_pressure))
VRX[[5]] <- VRX[[5]][-which ('EWX' == VRX[[5]])]  # exclude EWX
# Add cavity pressures if available:
if (any (grepl('^CAVP', FI$Variables))) {
  VRX[[5]] <- c(VRX[[5]], FI$Variables[grepl('^CAVP', FI$Variables)])
  # these are used for empirical CAVP
  VRX[[5]] <- c(VRX[[5]], 'PSXC', 'QCXC', 'MACHX', 'AKRD')
}
# Also add mixing ratio and LSRINT_VXL; they will be removed later if
# not present.
VRX[[5]] <- c(VRX[[5]], 'MR', 'LSRINT_VXL')

# set the appropriate cavity-pressure coefficients
# (This is a change from the previous representation, formerly unavailable for C-130.)
# These are written into the new configuration-file section.
# See "FitCavityPressure.pdf".
if (Platform == 'GV') {
  cavc <- c(2.7427, 0.2500, -34.7423, 1.153, -1.4725, -0.003128, 0.5424, 2.079299)
  cavc <- c(0.3073, 1.7557e-4, -0.9981, 0.0023, 0.1858, 0.0018, -0.9702, 0.0139) # new, OTREC
  dim(cavc) <- c(4, 2)
} else {
  # QCXC, MACHX, AKRD
  cavc <- c(0.3072830, 0.0001756, -0.9981261, 0.0022720, 
            0.185774, 0.001836, -0.970169, 0.013895)  # needs revision for C-130
  dim(cavc) <- c(4, 2)
}
# Add H2OMR_GMD if available
if ('H2OMR_GMD' %in% FI$Variables) {
  VRX[[5]] <- c(VRX[[5]], 'H2OMR_GMD')
}
# RHUM is not needed because RPlot5.R recalculates it from each sensor

# RPlot6: pressure
# Find primary pressure:
firstPR <- sub('. ', '', attr(Data$PSXC, 'Dependencies'))
VRX[[6]] <- unique(c(firstPR, VRX[[6]], FI$Measurands$air_pressure))
# Eliminate cavity and cabin pressures and PSXC (the last replaced by first in list)
VRX[[6]] <- VRX[[6]][-which(grepl('^CAVP', VRX[[6]]) |
                            grepl('PCAB', VRX[[6]]))]
VRX[[6]] <- VRX[[6]][-which(grepl('PSXC', VRX[[6]]))]
# also omit pressures in the aerosol ducts:
if (any(grepl('DUMPP', VRX[[6]]))) {
  VRX[[6]] <- VRX[[6]][-which(grepl('DUMPP', VRX[[6]]))]
}
# These have incorrect Measurands so remove them:
if ('QCTF' %in% VRX[[6]]) {
  VRX[[6]] <- VRX[[6]][-which('QCTF' == VRX[[6]])]
}
if ('QCTFC' %in% VRX[[6]]) {
  VRX[[6]] <- VRX[[6]][-which('QCTFC' == VRX[[6]])]
}

# for RPlot7.R:
# add Mach numbers (eventually using standard_name instead)
firstMACH <- sub('. ', '', attr(Data$MACHX, 'Dependencies'))
VRX[[7]] <- unique(c(firstMACH, VRX[[7]], FI$Measurands$mach_number))
# temporary surrogate for standard_name == mach_number:
VRX[[7]] <- unique(VRX[[7]], FI$Variables[which('MACH' == substr(FI$Variables, 1, 4))])
if ('MACHX' %in% VRX[[7]]) { # Remove MACHX if present
  VRX[[7]] <- VRX[[7]][-which(grepl('MACHX', VRX[[7]]))]
}
# add airspeeds:
firstTAS <- sub('. ', '', attr(Data$TASX, 'Dependencies'))
VRX[[7]] <-
  unique (c(firstTAS, VRX[[7]], FI$Measurands$plotform_speed_wrt_air))
if ('TASX' %in% VRX[[7]]) {
  VRX[[7]] <- VRX[[7]][-which(grepl('TASX', VRX[[7]]))]
}
# dynamic pressures: first find primary
firstQC <- sub('. ', '', attr(Data$QCXC, 'Dependencies'))
VRX[[7]] <-
  unique(c(firstQC, VRX[[7]], FI$Measurands$air_pressure_dynamic))
if ('QCXC' %in% VRX[[7]]) { # Remove QCXC if present
  VRX[[7]] <- VRX[[7]][-which(grepl('QCXC', VRX[[7]]))]
}
# QCTF and QCTFC don't have the right measurand name yet, so add them:
if ('QCTF' %in% FI$Variables) {
  VRX[[7]] <- c(VRX[[7]], 'QCTF')
}
if ('QCTFC' %in% FI$Variables) {
  VRX[[7]] <- c(VRX[[7]], 'QCTFC')
}
VRX[[7]] <- unique (VRX[[7]])  # in case the measurand is corrected someday

# for RPlot8.R
# These are ambient-dynamic pairs to add for total pressures:
if (grepl ('677', FI$Platform)) {
  VRX[[8]] <- unique(c(VRX[[8]], 'PSF', 'QCF', 'PS_A', 'QC_A', 'PSTF', 'QCTF'))
} else {
  VRX[[8]] <- unique(c(VRX[[8]], 'PSF', 'QCF', 'PS_A', 'QC_A',
  'PSFD', 'QCF', 'PSFRD', 'QCFR'))
}

# RPlot9.R: (wind)
VRX[[9]] <- unique (
  c(VRX[[9]], 'IWD', 'IWS',
    FI$Measurands$wind_from_direction,
    FI$Measurands$wind_speed,
    FI$Measurands$upward_air_velocity
  )
)
# But eliminate WI (generally, bad variable):
VRX[[9]] <- VRX[[9]][-which('WI' == VRX[[9]])]

# RPlot10: (Schuler oscillation)
# (These don't have standard_names now; change this if they are added.
#  For now, include specified names.)
VRX[[10]] <-
  unique (c(VRX[[10]], 'GGVEW', 'GGVNS', 'VEW', 'VNS', 'GGQUAL'))

# RPlot11.R: (attack and sideslip angles)
VRX[[11]] <- unique (c(VRX[[11]], 'AKRD', 'PITCH', 'SSRD', 'WSC', 'ADIFR', 'BDIFR',
                       'WDC', 'GGVEW', 'GGVNS', 'TASX', 'THDG', 'GGVSPD', 'VSPD_A'))

# RPlot12.R and RPlot13.R: (IRU comparisons)
VRX[[12]] <-
  unique (c(
      VRX[[12]],
      FI$Measurands$platform_orientation,
      FI$Measurands$platform_pitch_angle,
      FI$Measurands$platform_roll_angle
    )
  )
VRX[[13]] <-
  unique (c(VRX[[13]], FI$Variables[grepl('ACINS', FI$Variables)],
    FI$Variables[grepl('VSPD', FI$Variables)],
    FI$Measurands$altitude)) # includes unused GEOPTH

# RPlot14.R (Radiation)
VRX[[14]] <-
  unique (c(VRX[[14]], FI$Variables[grepl('Radiom', FI$LongNames)],
    FI$Variables[grepl('RSTB', FI$LongNames)])) # last: RSTB heater etc

# RPlot15.R (Hydrometeor and particle concentrations)
# Start with all concentrations
V <- FI$Variables[grepl('Concentration', FI$LongNames)]
# Remove ones that are 'per cell'
VX <- FI$Variables[which(
  grepl('per cell', FI$LongNames) |
    grepl('per Channel', FI$LongNames) |
    grepl('Monoxide', FI$LongNames) |
    grepl('Count', FI$LongNames) |
    grepl('CVI', FI$LongNames) |
    grepl('Water', FI$LongNames)
)]
VX <- c(VX, FI$Variables[grepl('CVI', FI$Variables)])
V <- V[!(V %in% VX)]
if ('CONCN' %in% FI$Variables) {
  V <- c(V, 'CONCN')
}
# eliminate 2D variables (leaving 1D)
V <- V[!grepl('2D', V)]
# and eliminate 1DC100, 1DC150 variables
V <- V[!grepl('1DC100', V) & !grepl('1DC150', V)]
# get UHSAS monitoring
VHA <- FI$Variables[grepl('UHSAS', FI$LongNames)] # all UHSAS
VHT <- FI$Variables[grepl('Temperature', FI$LongNames)] # all Temperature
VHF <- FI$Variables[grepl('Flow', FI$LongNames)] # all Flow
VHI <- FI$Variables[grepl('Intensity', FI$LongNames)] # all Intensity
VHS <- FI$Variables[grepl('Scatter', FI$LongNames)] # all Scatter
VH <- VHA[VHA %in% VHT]         # UHSAS temperatures
VH <- c(VH, VHA[VHA %in% VHF])  # Add flows
VH <- c(VH, VHA[VHA %in% VHI])  # Add intensities
VH <- c(VH, VHA[VHA %in% VHS])  # Add scatter
VH <- VH[!(VH %in% FI$Variables[grepl('CVI', FI$Variables)])]
VRX[[15]] <-
  VRX[[15]][!(VRX[[15]] %in% FI$Variables[grepl('CVI', FI$Variables)])]
VRX[[15]] <-
  VRX[[15]][!(VRX[[15]] %in% FI$Variables[grepl('1DC100', FI$Variables)])]
VRX[[15]] <- unique(c(VRX[[15]], V, VH))

# RPlot16.R: (DBAR and LWC)
VRX[[16]] <- unique (c(
      VRX[[16]],
      FI$Measurands$atmosphere_cloud_liquid_water_content,
      'RICE',
      'PLWC',
      FI$Variables[grepl('DBAR', FI$Variables)]
    )
  )
VRX[[16]] <-
  VRX[[16]][!(VRX[[16]] %in% FI$Variables[grepl('2DC', FI$Variables)])]
# add CDP monitoring
VCA <- FI$Variables[grepl('CDP', FI$LongNames)] # all CDP
VHCT <-
  FI$Variables[grepl('Total Count', FI$LongNames)] # all totals
VHR <- FI$Variables[grepl('Rejected', FI$LongNames)] # rejected
VHTR <- FI$Variables[grepl('Transit', FI$LongNames)] # avg transit
VHL <- FI$Variables[grepl('Laser', FI$LongNames)] # laser
VC <- VCA[VCA %in% VHT]         # CDP temperatures
VC <- c(VC, VCA[VCA %in% VHCT])
VC <- c(VC, VCA[VCA %in% VHR])
VC <- c(VC, VCA[VCA %in% VHTR])
VC <- c(VC, VCA[VCA %in% VHL])
VRX[[16]] <- unique(c(VRX[[16]], VC))
VRX[[16]] <-
  VRX[[16]][!(VRX[[16]] %in% FI$Variables[grepl('CVI', FI$Variables)])]

# RPlot17 and RPlot18: skew-T sounding
VRX[[17]] <- unique (c(VRX[[17]], 'ATX', 'DPXC', 'PSXC'))
VRX[[18]] <- unique (c(VRX[[17]], 'GGALT'))

# RPlot19, RPlot20: (potential temperatures)
VRX[[19]] <- unique (c(
      VRX[[19]],
      FI$Measurands$air_potential_temperature,
      'EWX',
      FI$Measurands$equivelent_potential_temperature,
      'PSXC', 'THETAV', 'PLWCC', 'ATX',
      FI$Measurands$pseudo_equivalent_potential_temperature
    )
  )

# RPlot20: CDP size distributions
# Get all size distributions:
VSD <-   FI$Variables[(grepl('per cell', FI$LongNames) |
      grepl('per Channel', FI$LongNames)) &
      grepl('Concentration', FI$LongNames)] # size distributions
VRX[[20]] <- unique (c(VRX[[20]], 'TASX', VSD[grepl('CDP', VSD)],
  VSD[grepl('CS100', VSD)],
  V[grepl('CONCD', V)], V[grepl('CONCF', V)]))

# RPlot21: UHSAS and CS200 (PCASP) size distributions
VRX[[21]] <- unique (c(VRX[[21]], 'TASX', VSD[grepl('UHSAS', VSD)],
  VSD[grepl('CS200', VSD)],
  V[grepl('CONCU', V)], V[grepl('CONCP', V)]))
VRX[[21]] <- VRX[[21]][!grepl('CVI', VRX[[21]])]
VRX[[21]] <-
  VRX[[21]][!grepl('U100', VRX[[21]]) & !grepl('U500', VRX[[21]])]

# RPlot22: 2D size distributions, variable C1DC
VRX[[22]] <- unique (c('TASX', V[grepl('CONC1DC', V)],
  FI$Variables[grepl('^C1DC', FI$Variables)]))

# RPlot23: Air chemistry
VAC <- c(FI$Variables[grepl('Monoxide', FI$LongNames)])
VAC <- c(VAC, FI$Variables[grepl('PIC', FI$Variables)])
VAC <- c(VAC, FI$Variables[grepl('ARI', FI$Variables)])
VAC <- c(VAC, FI$Variables[grepl('Aerolaser', FI$LongNames)])
VRX[[23]] <- unique (c(VRX[[23]], VAC))

# RPlot24, RPlot25: (spares)
VRX[[24]] <- c('ATX', 'TASX')
VRX[[25]] <- c('ATX', 'TASX')


# Now keep the items that are present in the netCDF file:
for (i in 1:length(VRX)) {
  VRX[[i]] <- VRX[[i]][VRX[[i]] %in% FI$Variables]
}

# Write out the result
outfile = './NewConfig'
cat (sprintf ('# Version generated by makeConfiguration.R\n'), file = outfile)
cat (sprintf ("if (Project == '%s') {\n", NewProject),
  file = outfile,
  append = TRUE)
cat ("  cavc <- c(",
  paste(cavc, collapse = ', '),
  ")\n",
  file = outfile,
  append = TRUE)
cat ("  dim(cavc) <- c(4,2)\n", file = outfile, append = TRUE)
cat (
  sprintf ("  VRPlot <- list(PV1 <- c('%s'))\n",
    paste(VRX[[1]], collapse = "', '")),
  file = outfile,
  append = TRUE
)
for (i in 2:length(VRX)) {
  cat(sprintf("  VRPlot$PV%d <- c('%s')\n", i,
    paste(VRX[[i]], collapse = "', '")),
    file = outfile,
    append = TRUE)
}
cat('}\n', file = outfile, append = TRUE)
sink()

