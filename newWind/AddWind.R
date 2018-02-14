
## variables needed in input data.frame for all additions:
## "ACINS", "PSXC", "ATX", "GGLAT", "GGALT", "PSTF, "QCTF",
## "PSXC", "TASX", "ATTACK", "SSLIP", "GGVEW", "GGVNS", "EWX", "GGVSPD", 
## "VEW", "VNS", "THDG", "ROLL", "PITCH", "ADIFR", "QCF", "PSF", "ADIF_GP",
## "BDIF_GP", "QC_GP", "PS_GP", "CROLL_GP", "CPITCH_GP", "CTHDG_GP" 
## Recommended variables in data.frame (to provide comparison variables also):
VR <- c("ACINS", "ADIF_GP", "ADIFR", "AKRD", "ATTACK", "ATX", "BDIF_GP",
  "CPITCH_GP", "CROLL_GP", "CTHDG_GP", "CVEW_GP", "CVNS_GP",  
  "CVSPD_GP", "EWX",  "GGALT",  "GGLAT", "GGVEW", "GGVNS",  "GGVSPD",
  "PITCH",  "PS_GP",  "PSF",  "PSTF", "PSXC", "QC_GP",  "QCF",
  "QCTF", "ROLL", "SSLIP",  "TASX", "THDG", "UXC",  "VEW",  "VNS",
  "VYC",  "WDC",  "WIC",  "WSC" )
## construct function to add variables to a data.frame:
AddWind <- function (DF, Rate=1, addAKY=TRUE, addGP=TRUE, addTC=TRUE, addROC=TRUE) {
  if (!is.null(attr(DF, 'Rate'))) {Rate <- attr (DF, 'Rate')}
  requiredVar <- c('PSXC', 'TASX', 'ATTACK', 'SSLIP', 'GGVEW', 'GGVNS', 'EWX',
    'GGVSPD', 'VEW', 'VNS', 'THDG', 'ROLL', 'PITCH')
  if (addAKY) {requiredVar <- c(requiredVar, 'ADIFR', 'QCF', 'PSF')}
  if (addGP) {requiredVar <- c(requiredVar, 'ADIF_GP', 'BDIF_GP', 'QC_GP',
    'PS_GP', 'CROLL_GP', 'CPITCH_GP', 'CTHDG_GP',
    'CVSPD_GP', 'CVEW_GP', 'CVNS_GP')}
  if (addTC) {requiredVar <- c(requiredVar, 'PSTF', 'QCTF', 'PSXC')}
  if (addROC) {requiredVar <- c(requiredVar, 'ACINS', 'PSXC', 'ATX', 
    'GGLAT', 'GGALT')}
  requiredVar <- unique(requiredVar)
  NV <- names(DF)
  for (V in requiredVar) {
    if (!(V %in% NV)) {
      print (sprintf ('Variable %s not found; AddWind() returning unchanged data.frame', V))
      return (DF)
    }
  }
  ## make copy for function use, to add fit variables without returning them
  D <- DF
  
  ## define fit variables:
  FV <- vector('character')
  if (addAKY) {
    V <- 'QR'
    FV <- c(FV, V)
    D[, V] <- D$ADIFR / D$QCF
    D$QR[is.infinite(D$QR)] <- NA
    V <- 'M'
    FV <- c(FV, V)
    D[, V] <- MachNumber (D$PSF, D$QCF)
    V <- 'QCF'
    FV <- c(FV, V)
    D[, V] <- D$QCF
  }
  if (addGP) {
    diffP <- c(rep(0, nrow(D)), D$BDIF_GP, D$ADIF_GP)  ## check signs!!
    dim(diffP) <- c(nrow(D), 3)
    DX <- data.frame(ROLL=D$CROLL_GP, PITCH=D$CPITCH_GP, THDG=D$CTHDG_GP)
    diffPl <- Ranadu::XformLA (DX, diffP)  ## DF contains gustpod attitude angles
    diffPa <- Ranadu::XformLA (D, diffPl, .inverse=TRUE)  # pressure vector in a-frame
    D$DIFFPX <- diffPa[,1]
    D$DIFFPY <- diffPa[,2]
    D$DIFFPZ <- diffPa[,3]
    V <- 'DV1'
    FV <- c(FV, V)
    D[, V] <- with(D, DIFFPZ / QC_GP)
    D$MACHG <- MachNumber (D$PS_GP, D$QC_GP)
    V <- 'DV2'
    FV <- c(FV, V)
    D[, V] <- with (D, MACHG * DIFFPZ / QC_GP)
    V <- 'DV3'
    FV <- c(FV, V)
    D[, V] <- with(D, DIFFPY / QC_GP)
    V <- 'DV4'
    FV <- c(FV, V)
    D[, V] <- with (D, (DIFFPZ / QC_GP)^2)
    V <- 'DVS1'
    FV <- c(FV, V)
    D[, V] <- D[ ,'DV3']
    V <- 'DVS2'
    FV <- c(FV, V)
    D[, V] <- with (D, MACHG * DIFFPY / QC_GP)
    V <- 'DVS3'
    FV <- c(FV, V)
    D[, V] <- D[ ,'DV1']
    ## these are the TASG variables:
    V <- 'DVT2'
    FV <- c(FV, V)
    D[, V] <- with (D, QC_GP / PS_GP)
    V <- 'DVT3'
    FV <- c(FV, V)
    D[, V] <- with (D, MACHG * QC_GP / PS_GP)
    V <- 'DVT4'
    FV <- c(FV, V)
    D[, V] <- with (D, (QC_GP / PS_GP)^2)
    V <- 'DVT5'
    FV <- c(FV, V)
    D[, V] <- with (D, ADIF_GP / QC_GP)
    V <- 'DVT6'
    FV <- c(FV, V)
    D[, V] <- with (D, MACHG)
    V <- 'DVT7'
    FV <- c(FV, V)
    D[, V] <- with (D, (ADIF_GP / QC_GP)^2)
    V <- 'DVT8'
    FV <- c(FV, V)
    D[, V] <- with (D, MACHG * ADIF_GP / QC_GP)
  }
  for (V in FV) {
    VS <- sprintf ('%sS', V)
    VF <- sprintf ('%sF', V)
    D[, VS] <- SplitDV (D[, V], 600)
    D[, VF] <- D[, V] - D[, VS]
  }
  
  ## Add requested variables:
  if (addROC) {
    ## ROC.R -- chunk to add ROC variable
    D$Grav <- Gravity(D$GGLAT, D$GGALT)
    DPDT <- c(0, diff(D$PSXC)) * Rate
    g <- D$Grav
    g[is.na(g)] <- 9.80
    WPPRIME <- -StandardConstant('Rd') * (273.15 + D$ATX) /
      (D$PSXC * g) * DPDT
    ACINS <- zoo::na.approx (as.vector(D$ACINS), maxgap=1000, na.rm=FALSE)
    ACINS[is.na(ACINS)] <- 0
    WPSTAR <- cumsum(ACINS)
    DIF <- WPPRIME - WPSTAR
    DIF <- zoo::na.approx (as.vector(DIF), maxgap=1000, na.rm=FALSE)
    DIF[is.na(DIF)] <- 0
    tau <- 300 * Rate
    DIF <- signal::filtfilt (signal::butter (3, 2/tau), DIF)
    DF$ROC <- WPSTAR + DIF
    DF$ZROC <- D$GGALT[1] + cumsum (DF$ROC)
    rm (DPDT, g, WPPRIME, WPSTAR, DIF)
  }
  if (addAKY) {
    cff <- 21.481
    cfs <- c(4.5110727, 19.8409482, -0.0018806)
    DF$AKY <- cff * D$QRF + cfs[1] + cfs[2] * D$QRS + cfs[3] * D$QCFS
    DW <- D
    DW$ATTACK <- DF$AKY
    if (addROC) {DW$GGVSPD <- DF$ROC}
    DW <- WindProcessor (DW, CompF=FALSE)
    DF$WIY <- DW$WIN
  }
  if (addTC) {
    D$PTOT2 <- D$PSTF + D$QCTF
    if (Rate > 1) {
      D$PSFF <- zoo::na.approx (as.vector(D$PSXC), na.rm=FALSE)
      rmean <- zoo::rollapply(D$PSFF, width=51, FUN=mean, fill=NA)
      rmean <- zoo::na.approx (as.vector(rmean), na.rm=FALSE)
      means <- mean(D$PSFF, na.rm=TRUE)
      D$PSFF[is.na(D$PSFF)] <- rmean[is.na(D$PSFF)]
      D$PSFF[is.na(D$PSFF)] <- means
      D$PSFF <- signal::filtfilt (signal::butter (3, 2/(2*Rate), type='low'), D$PSFF)
      DF$QCTC <- D$PTOT2 - D$PSFF
      DF$TASTC <- TrueAirspeed (MachNumber (D$PSFF, DF$QCTC, D$EWX), D$ATX, D$EWX/D$PSXC)
    } else {
      DF$QCTC <- D$PTOT2 - D$PSXC
      DF$TASTC <- TrueAirspeed (MachNumber (D$PSXC, DF$QCTC, D$EWX), D$ATX, D$EWX/D$PSXC)
    }
    DW <- DF
    DW$TASX <- DW$TASTC
    if (addAKY) {DW$ATTACK <- DF$AKY}
    DW <- WindProcessor (DW)
    DF$WITC <- DW$WIN
    DF$WDTC <- DW$WDN
    DF$WSTC <- DW$WSN
    hdg <- DF$THDG * pi/180
    wd <- DF$WDTC * pi/180 + pi
    DF$UXTC <- DF$WSTC * (sin(hdg) * sin(wd) + cos(hdg) * cos(wd))
    hdg <- hdg - pi/2
    DF$VYTC <- DF$WSTC * (sin(hdg) * sin(wd) + cos(hdg) * cos(wd))
  }
  if (addGP) {
    ## required fit coefficients: cfas, cfaf, cfss, cfsf, cftS, cftF
    cfas <- c(2.614310,  7.777724, -7.317871,  2.517215, -3.826207)
    cfaf <- c(5.7669899, -1.9574047, 0.8617985)
    cfss <- c(-1.392315, 11.178966, -10.687443, -2.157064)
    cfsf <- c(15.705197, -6.351200, -3.027087)
    cftS <- c(-0.008546707, -6.539942685, 13.574962795, -8.252736905, -0.415905472, 1.001949524, 0.162728384, 1.038298812)
    cftF <- c(-1.43065056, 4.22860540, -1.86642444, -0.18020347, 0.37589586, 0.05232383, 0.48709595)
    ## angle of attack:
    D$AK_GPS <- with(D, cfas[1] + cfas[2] * DV1S + cfas[3] * DV2S + cfas[4] * DV3S + cfas[5] * DV4S)
    D$AK_GPF <- with(D, cfaf[1] * DV1F + cfaf[2] * DV2F + cfaf[3] * DV3F)
    DF$AK_GP <- D$AK_GPS + D$AK_GPF
    ## sideslip
    D$SS_GPS <- with(D, cfss[1] + cfss[2] * DVS1S + cfss[3] * DVS2S + cfss[4] * DVS3S)
    D$SS_GPF <- with(D, cfsf[1] * DVS1F + cfsf[2] * DVS2F + cfsf[3] * DVS3F)
    DF$SS_GP <- D$SS_GPS + D$SS_GPF
    ## airspeed:
    D$QRPS <- with(D, cftS[1] + cftS[2]*DVT2S + cftS[3]*DVT3S + cftS[4]*DVT4S +
        cftS[5]*DVT5S + cftS[6]*DVT6S + cftS[7]*DVT7S + cftS[8]*DVT8S)
    D$QRPF <- with(D, cftF[1]*DVT2F+cftF[2]*DVT3F+cftF[3]*DVT4F +
        cftF[4]*DVT5F+cftF[5]*DVT6F+cftF[6]*DVT7F+cftF[7]*DVT8F)
    D$QRP <- D$QRPS + D$QRPF
    ## replace MACHG with new value:
    D$MACHG <- MachNumber(1, D$QRP, D$EWX/D$PSXC)
    DF$TASG <- TrueAirspeed(D$MACHG, D$ATX, D$EWX/D$PSXC)
    ## new wind:
    DW <- D
    DW$TASX <- DF$TASG
    DW$ATTACK <- DF$AK_GP
    DW$SSLIP <- DF$SS_GP
    # DW$PITCH <- D$CPITCH_GP  ## don't want these; already transformed to a-frame
    DW$VEW <- D$CVEW_GP
    DW$VNS <- D$CVNS_GP
    DW$GGVSPD <- D$CVSPD_GP
    DW <- WindProcessor(DW, CompF=FALSE)
    DF$WDG <- DW$WDN
    DF$WSG <- DW$WSN
    DF$WIG <- DW$WIN
    hdg <- D$THDG * pi/180
    wd <- DF$WDG * pi/180 + pi
    DF$UXG <- DF$WSG * (sin(hdg) * sin(wd) + cos(hdg) * cos(wd))
    hdg <- hdg - pi/2
    DF$VYG <- DF$WSG * (sin(hdg) * sin(wd) + cos(hdg) * cos(wd))
  }
  return (DF)
}
