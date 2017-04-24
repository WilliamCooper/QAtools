Project <- 'CONTRAST'
Data <- data.frame()
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
    Data <- rbind (Data, getNetCDF (fname))
  }
  DataS <- Data
}
# Data <- getNetCDF(fname)  ## standard variables OK
Data$ATX <- ShiftInTime(Data$ATX, .shift=-2300)
Data$GGALT <- ShiftInTime(Data$GGALT, .shift=500)
re <- is.na(Data$EWX)
Data$EWX[re] <- 0.5*MurphyKoop(Data$ATX[re], Data$PSXC[re])
Data$Grav <- Gravity(Data$LATC, Data$GGALT)
Data$DZ <- c(0, diff(Data$GGALT))
Data$DGZ <- SmoothInterp (Data$DZ, .Length=5)
Data$DGZ2 <- SmoothInterp (Data$DZ, .Length=1801)
Data$LP <- log(Data$PSXC)
Data$DLP <- c(0, diff(Data$LP))
Data$DLP <- SmoothInterp (Data$DLP, .Length=5)
Data$Ra <- SpecificHeats(Data$EWX / Data$PSXC)[,3]
Data$ATHE <- -Data$Grav * Data$DGZ / (Data$Ra * Data$DLP) - 273.15
Data$ATHE <- zoo::na.approx(as.vector(Data$ATHE), maxgap=1000, na.rm=FALSE)
Data$ATHE[is.na(Data$ATHE)] <- 0
Data$ATHE[abs(Data$DLP) < 0.0003] <- 0
Data$ATHE <- SmoothInterp (Data$ATHE)
Data$ATHE[abs(Data$DLP) < 0.0001] <- NA
bs <- binStats(data.frame(Data$ATHE-Data$ATX, Data$PSXC), bins=15)
bs$sigma[bs$nb > 1] <- bs$sigma/sqrt(bs$nb)
g <- ggplot(data=bs)
g <- g + geom_errorbarh (aes (y=xc, x=ybar, xmin=ybar-sigma, 
                              xmax=ybar+sigma), na.rm=TRUE) 
xlow <- floor(min (bs$ybar-bs$sigma, na.rm=TRUE))
xhigh <- ceiling(max (bs$ybar+bs$sigma, na.rm=TRUE))
if (xhigh < 2) {xhigh <- 2}
if (xlow > -2) {xlow <- -2}
xlow <- -20; xhigh <- 20
g <- g + xlim(xlow, xhigh) + theme_WAC()
g <- g + xlab('ATX-ATHE [deg. C]') + ylab('PSXC [hPa]') + ylim(1050, 80) 
g <- g + geom_point (aes (x=bs$ybar, y=bs$xc), size=3, colour='blue', na.rm=TRUE)
g <- g + geom_label (aes (x=9.9, y=bs$xc, label=sprintf('%d', bs$nb)))
print (g)
