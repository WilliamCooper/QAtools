## defines plot variables to use in RPlot functions
## this script serves two functions:
## 1. The variables listed here are included in VarList, used
##    to construct the data.frame used for these plots
## 2. For individual plots, the variables are used to
##    construct each plot as specified (with some exceptions,
##    like the track plot in RPlot1, where the variables are
##    required to be as specified here in plotTrack() ).
pitch_offset = 0.37
roll_offset = -0.26
thdg_offset = -0.35
## Here is code that will find the available temperature variables
## It won't run here because the file name may not be available,
## so run this from the console and add variables to VRPlot$PV3
# NCF <- nc_open (fname)
# N <- names(NCF$var)
# nc_close(NCF)
# TTvar <- c (N[c(which(grepl("^ATH", N)), which(grepl("^ATF", N)))], "AT_A")


if (Project == "SOCRATES") {
  pitch_offset = 0.18
  roll_offset = -0.12
  thdg_offset = -0.54
  VRPlot <- list(PV1 = c("LATC", "LONC", "WDC", "WSC"))
  VRPlot$PV2 <- c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC")
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  VRPlot$PV4 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX", "CAVP_DPL", "CAVP_DPR", "PSXC", "LSRINT_VXL",
    "EW_DPL", "EW_DPR", "EW_VXL", "MR", "QCXC", "AKRD", "MACHX")
  VRPlot$PV6 <- c("PSF", "PS_A", "PSXC")
  VRPlot$PV7 <- c("QCF", "QCR", "QC_A", "QCFC", "QCRC", "TAS_A", "TASF", "TASR", "MACH_A", "MACHF", "MACHR")
  VRPlot$PV8 <- c("PSF", "PS_A", "QCF", "QC_A")
  VRPlot$PV9 <- c("IWD", "WDC", "IWS", "WSC", "WIC")
  VRPlot$PV10 <- c("GGVEW", "VEW", "VEWC", "GGVNS", "VNS", "VNSC", "GGQUAL")
  VRPlot$PV11 <- c("AKRD", "PITCH", "GGVSPD", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "ALT_A", "GGALT")
  VRPlot$PV14 <- "RSTB"
  VRPlot$PV15 <- c("CONC1DC_", "CONCD_", "CONCN", "CONCU100_", "CONCU500_", "CONCU_", "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")
  VRPlot$PV16 <- c("DBAR1DC_", "DBARD_", "DBARU_", "PLWC", "PLWC1DC_", "PLWCC", "PLWCD_", "RICE", "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC", "ATX", "PSXC", "EWX")
  VRPlot$PV20 <- c("CCDP_", "TASX")
  VRPlot$PV21 <- c("CUHSAS_", "TASX")
  VRPlot$PV22 <- c("C1DC_", "TASX")
  VRPlot$PV23 <- c(NA)
  VRPlot$PV24 <- c("ATX", "TASX")
  VRPlot$PV25 <- c("ATX", "TASX")
  VRPlot$PV26 <- c("TASX", "ATX")
  VRPlot$PV27 <- c("TASX", "ATX")
  VRPlot$PV28 <- c("TASX", "ATX")
  VRPlot$PV29 <- c("TASX", "ATX")
  VRPlot$PV30 <- c("TASX", "ATX")
}
if (Project == "WECAN-TEST") {
  pitch_offset = 0.0
  roll_offset = -0.0
  thdg_offset = -0.0
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- VRPlot$PV3
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_DPB", "DP_DPT", "ATX")
  ## don't use if CAVP not available:
  VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPB", "CAVP_DPT", "PSXC")
  ## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCXC")
  ## list of vapor pressures to plot (there is no EW_VXL in WECAN-TEST)
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPB", "EW_DPT", "MR")  # H2OMR_GMD not in HIPPO-3 files?
  VRPlot$PV5 <- c(VRPlot$PV5, "AKRD", "MACHX")
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSFC", "PSFDC")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCFR", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFRC", "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASFR", "TASR", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHFR", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSFD", "QCF", "PSFRD", "QCFR", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2",  "THDG", "THDG_IRS2")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  ## at present there is no RPlot14; UHSAS is handled later
  VRPlot$PV14 <- c("RSTB", 'RSTB1')
  ## plot concentrations:
  VRPlot$PV15 <- NA
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- NA
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- NA
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV21 <- NA
  ## plot sample of 2DC size distributions
  VRPlot$PV22 <- NA
  VRPlot$PV23 <- c("CORAW_AL", "CO2_PIC2311", "CO2C_PIC2311")
}

if (Project == "ECLIPSE") {
  pitch_offset = 0.18
  roll_offset = -0.12
  thdg_offset = -0.54
  VRPlot <- list(PV1 = c("LATC", "LONC", "WDC", "WSC"))
  VRPlot$PV2 <- c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC")
  VRPlot$PV3 <- c("ATH1", "ATH2", "AT_A")
  VRPlot$PV4 <- c("ATH1", "ATH2", "AT_A")
  VRPlot$PV5 <- c("DP_DPL", "DP_DPR", "ATX", "CAVP_DPL", "CAVP_DPR", "PSXC", "EW_DPL", "EW_DPR", "MR",
                  "QCXC", "AKRD", "MACHX")
  VRPlot$PV6 <- c("PSF", "PS_A", "PSXC")
  VRPlot$PV7 <- c("QCF", "QCR", "QC_A", "QCFC", "QCRC", "TAS_A", "TASR", "TASF", "MACH_A", "MACHR", "MACHF")
  VRPlot$PV8 <- c("PSF", "PS_A", "QCF", "QC_A")
  VRPlot$PV9 <- c("IWD", "WDC", "IWS", "WSC", "WIC")
  VRPlot$PV10 <- c("GGVEW", "VEW", "VEWC", "GGVNS", "VNS", "VNSC", "GGQUAL")
  VRPlot$PV11 <- c("AKRD", "PITCH", "GGVSPD", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "ALT_A", "GGALT")
  VRPlot$PV14 <- NA    ## not present
  ## note: must specify multiple probe names explicitly
  VRPlot$PV15 <- NA
  VRPlot$PV16 <- NA
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "PSXC", "ATX", "PSXC", "EWX")
  VRPlot$PV20 <- NA
  VRPlot$PV21 <- c(NA)
  VRPlot$PV22 <- NA
  VRPlot$PV23 <- c(NA)
  VRPlot$PV24 <- c("ATX", "TASX")
  VRPlot$PV25 <- c("ATX", "TASX")
  VRPlot$PV26 <- c("TASX", "ATX")
  VRPlot$PV27 <- c("TASX", "ATX")
  VRPlot$PV28 <- c("TASX", "ATX")
  VRPlot$PV29 <- c("TASX", "ATX")
  VRPlot$PV30 <- c("TASX", "ATX")
}

if (Project == "ARISTO2017") {
  pitch_offset = 0.18
  roll_offset = -0.12
  thdg_offset = -0.54
  VRPlot <- list(PV1 = c("LATC", "LONC", "WDC", "WSC"))
  VRPlot$PV2 <- c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC")
  VRPlot$PV3 <- c("ATH1", "ATH2", "AT_A")
  VRPlot$PV4 <- c("ATH1", "ATH2", "AT_A")
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX", "CAVP_DPL", "CAVP_DPR", "PSXC", "LSRINT_VXL",
                  "EW_DPL", "EW_DPR", "EW_VXL", "MR", "QCXC", "AKRD", "MACHX")
  VRPlot$PV6 <- c("PSF", "PS_A", "PSXC")
  VRPlot$PV7 <- c("QCF", "QCR", "QC_A", "QCFC", "QCRC", "TAS_A", "TASF", "TASR", "MACH_A", "MACHF", "MACHR")
  VRPlot$PV8 <- c("PSF", "PS_A", "QCF", "QC_A")
  VRPlot$PV9 <- c("IWD", "WDC", "IWS", "WSC", "WIC")
  VRPlot$PV10 <- c("GGVEW", "VEW", "VEWC", "GGVNS", "VNS", "VNSC", "GGQUAL")
  VRPlot$PV11 <- c("AKRD", "PITCH", "GGVSPD", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "ALT_A", "GGALT")
  VRPlot$PV14 <- NA    ## not present
                                               ## note: must specify multiple probe names explicitly
  VRPlot$PV15 <- c("CONC1DC_RWO", "CONCD_RWII", "CONCD_RWIO")  ## there are two CDP probes on ARISTO2017
  VRPlot$PV16 <- c("DBAR1DC_RWO", "DBARD_RWII", "DBARD_RWIO", "PLWC", "PLWC1DC_", "PLWCC", "PLWCD_RWII", "PLWCD_RWIO", "RICE", "TCNTD_RWII", "REJDOF_RWII", "AVGTRNS_RWII", "CDPLSRP_RWII")
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC", "ATX", "PSXC", "EWX")
  VRPlot$PV20 <- c("CCDP_RWII", "CCDP_RWIO", "TASX")
  VRPlot$PV21 <- c(NA)
  VRPlot$PV22 <- c("C1DC_RWO", "TASX")
  VRPlot$PV23 <- c(NA)
  VRPlot$PV24 <- c("ATX", "TASX")
  VRPlot$PV25 <- c("ATX", "TASX")
  VRPlot$PV26 <- c("TASX", "ATX")
  VRPlot$PV27 <- c("TASX", "ATX")
  VRPlot$PV28 <- c("TASX", "ATX")
  VRPlot$PV29 <- c("TASX", "ATX")
  VRPlot$PV30 <- c("TASX", "ATX")
}

if (Project == "ORCAS") {
  pitch_offset = 0.18
  roll_offset = -0.12
  thdg_offset = -0.54
  VRPlot <- list(PV1 = c("LATC", "LONC", "WDC", "WSC"))
  VRPlot$PV2 <- c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC")
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  VRPlot$PV4 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX", "CAVP_DPL", "CAVP_DPR", "EW_DPL", "EW_DPR", "EW_VXL",
                  "QCXC", "AKRD", "MACHX")
  VRPlot$PV6 <- c("PSF", "PS_A", "PSXC")
  VRPlot$PV7 <- c("QCF", "QCR", "QC_A", "QCFC", "QCRC", "TAS_A", "TASF", "TASR", "MACH_A", "MACHF", "MACHR")
  VRPlot$PV8 <- c("PSF", "PS_A", "QCF", "QC_A")
  VRPlot$PV9 <- c("IWD", "WDC", "IWS", "WSC", "WIC")
  VRPlot$PV10 <- c("GGVEW", "VEW", "VEWC", "GGVNS", "VNS", "VNSC", "GGQUAL")
  VRPlot$PV11 <- c("AKRD", "PITCH", "GGVSPD", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "ALT_A", "GGALT")
  VRPlot$PV14 <- "RSTB"
  VRPlot$PV15 <- c("CONC1DC_", "CONCD_", "CONCN", "CONCU100_", "CONCU500_", "CONCU_", "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")
  VRPlot$PV16 <- c("DBAR1DC_", "DBARD_", "DBARU_", "PLWC", "PLWC1DC_", "PLWCC", "PLWCD_", "RICE", "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC", "ATX", "PSXC", "EWX")
  VRPlot$PV20 <- c("CCDP_", "TASX")
  VRPlot$PV21 <- c("CUHSAS_", 'TASX')
  VRPlot$PV22 <- c("C1DC_", "CONC1DC_")
  VRPlot$PV23 <- c(NA)
  VRPlot$PV24 <- c("ATX", "TASX")
  VRPlot$PV25 <- c("ATX", "TASX")
  VRPlot$PV26 <- c("TASX", "ATX", "CONCD_", "GGALT")
  VRPlot$PV27 <- c("TASX", "ATX")
  VRPlot$PV28 <- c("TASX", "ATX")
  VRPlot$PV29 <- c("TASX", "ATX")
  VRPlot$PV30 <- c("TASX", "ATX")
}


if (Project == "CSET") {
  pitch_offset = 0.18
  roll_offset = -0.12
  thdg_offset = -0.54
  VRPlot <- list(PV1 = c("LATC", "LONC", "WDC", "WSC"))
  VRPlot$PV2 <- c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC")
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  VRPlot$PV4 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX", "CAVP_DPL", "CAVP_DPR", "PSXC", "LSRINT_VXL",
                  "EW_DPL", "EW_DPR", "EW_VXL", "MR", "QCXC", "AKRD", "MACHX")
  VRPlot$PV6 <- c("PSF", "PS_A", "PSXC")
  VRPlot$PV7 <- c("QCF", "QCR", "QC_A", "QCFC", "QCRC", "TAS_A", "TASF", "TASR", "MACH_A", "MACHF", "MACHR")
  VRPlot$PV8 <- c("PSF", "PS_A", "QCF", "QC_A")
  VRPlot$PV9 <- c("IWD", "WDC", "IWS", "WSC", "WIC")
  VRPlot$PV10 <- c("GGVEW", "VEW", "VEWC", "GGVNS", "VNS", "VNSC", "GGQUAL")
  VRPlot$PV11 <- c("AKRD", "PITCH", "GGVSPD", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "ALT_A", "GGALT")
  VRPlot$PV14 <- "RSTB"
  VRPlot$PV15 <- c("CONC1DC_", "CONCD_", "CONCN", "CONCU100_", "CONCU500_", "CONCU_", "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")
  VRPlot$PV16 <- c("DBAR1DC_", "DBARD_", "DBARU_", "PLWC", "PLWC1DC_", "PLWCC", "PLWCD_", "RICE", "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC", "ATX", "PSXC", "EWX")
  VRPlot$PV20 <- c("CCDP_", "TASX")
  VRPlot$PV21 <- c("CUHSAS_", "TASX")
  VRPlot$PV22 <- c("C1DC_", "TASX")
  VRPlot$PV23 <- c(NA)
  VRPlot$PV24 <- c("ATX", "TASX")
  VRPlot$PV25 <- c("ATX", "TASX")
  VRPlot$PV26 <- c("TASX", "ATX")
  VRPlot$PV27 <- c("TASX", "ATX")
  VRPlot$PV28 <- c("TASX", "ATX")
  VRPlot$PV29 <- c("TASX", "ATX")
  VRPlot$PV30 <- c("TASX", "ATX")
}

if (Project == "NOREASTER") {
  pitch_offset = 0.18
  roll_offset = -0.12
  thdg_offset = -0.54
  VRPlot <- list(PV1 = c("LATC", "LONC", "WDC", "WSC"))
  VRPlot$PV2 <- c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC")
  VRPlot$PV3 <- c("ATHR1", "ATHR2", "ATFH1", "ATFH2", "AT_A")
  VRPlot$PV4 <- VRPlot$PV3
  VRPlot$PV5 <- c("DP_DPL", "DP_DPR", "ATX", "CAVP_DPL", "CAVP_DPR", "PSXC", "EW_DPL", "EW_DPR", "MR",
                  "QCXC", "AKRD", "MACHX")
  VRPlot$PV6 <- c("PSF", "PS_A", "PSXC")
  VRPlot$PV7 <- c("QCF", "QCR", "QC_A", "QCFC", "QCRC", "TAS_A", "TASF", "TASR", "MACH_A", "MACHF", "MACHR")
  VRPlot$PV8 <- c("PSF", "PS_A", "QCF", "QC_A")
  VRPlot$PV9 <- c("IWD", "WDC", "IWS", "WSC", "WIC")
  VRPlot$PV10 <- c("GGVEW", "VEW", "VEWC", "GGVNS", "VNS", "VNSC", "GGQUAL")
  VRPlot$PV11 <- c("AKRD", "PITCH", "GGVSPD", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "ALT_A", "GGALT")
  VRPlot$PV14 <- NA
  VRPlot$PV15 <- NA
  VRPlot$PV16 <- NA
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC", "ATX", "PSXC", "EWX")
  VRPlot$PV20 <- NA
  VRPlot$PV21 <- NA
  VRPlot$PV22 <- NA
  VRPlot$PV23 <- c(NA)
  VRPlot$PV24 <- c("ATX", "TASX")
  VRPlot$PV25 <- c("ATX", "TASX")
  VRPlot$PV26 <- c("TASX", "ATX")
  VRPlot$PV27 <- c("TASX", "ATX")
  VRPlot$PV28 <- c("TASX", "ATX")
  VRPlot$PV29 <- c("TASX", "ATX")
  VRPlot$PV30 <- c("TASX", "ATX")
}

if (Project == "HCRTEST") {
  pitch_offset = 0.18
  roll_offset = -0.12
  thdg_offset = -0.54
  VRPlot <- list(PV1 = c("LATC", "LONC", "WDC", "WSC"))
  VRPlot$PV2 <- c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC")
  VRPlot$PV3 <- c("ATHR1", "ATHR2", "ATFH1", "ATFH2", "AT_A")
  VRPlot$PV4 <- VRPlot$PV3
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX", "CAVP_DPL", "CAVP_DPR", "PSXC", "LSRINT_VXL",
    "EW_DPL", "EW_DPR", "EW_VXL", "MR", "QCXC", "AKRD", "MACHX")
  VRPlot$PV6 <- c("PSF", "PS_A", "PSXC")
  VRPlot$PV7 <- c("QCF", "QCR", "QC_A", "QCFC", "QCRC", "TAS_A", "TASF", "TASR", "MACH_A", "MACHF", "MACHR")
  VRPlot$PV8 <- c("PSF", "PS_A", "QCF", "QC_A")
  VRPlot$PV9 <- c("IWD", "WDC", "IWS", "WSC", "WIC")
  VRPlot$PV10 <- c("GGVEW", "VEW", "VEWC", "GGVNS", "VNS", "VNSC", "GGQUAL")
  VRPlot$PV11 <- c("AKRD", "PITCH", "GGVSPD", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "ALT_A", "GGALT")
  VRPlot$PV14 <- NA
  VRPlot$PV15 <- c("CONC1DC_", "CONCD_" )
  VRPlot$PV16 <- c("DBAR1DC_", "DBARD_", "PLWC1DC_", "PLWCD_", "RICE", "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "PSXC", "ATX", "PSXC", "EWX")
  VRPlot$PV20 <- c("CCDP_", "TASX")
  VRPlot$PV21 <- NA
  VRPlot$PV22 <- c("C1DC_", "TASX")
  VRPlot$PV23 <- c(NA)
  VRPlot$PV24 <- c("ATX", "TASX")
  VRPlot$PV25 <- c("ATX", "TASX")
  VRPlot$PV26 <- c("TASX", "ATX")
  VRPlot$PV27 <- c("TASX", "ATX")
  VRPlot$PV28 <- c("TASX", "ATX")
  VRPlot$PV29 <- c("TASX", "ATX")
  VRPlot$PV30 <- c("TASX", "ATX")
}

if (Project == "DEEPWAVE") {
  pitch_offset = 0.18
  roll_offset = -0.12
  thdg_offset = -0.54
  VRPlot <- list(PV1 = c("LATC", "LONC", "WDC", "WSC"))
  VRPlot$PV2 <- c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC")
  VRPlot$PV3 <- c("ATHR1", "ATHR2", "ATRL", "AT_A")
  VRPlot$PV4 <- c("ATHR1", "ATHR2", "ATRL", "AT_A")
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX", "CAVP_DPL", "CAVP_DPR", "PSXC", "LSRINT_VXL",
                  "EW_DPL", "EW_DPR", "EW_VXL", "MR", "QCXC", "AKRD", "MACHX")
  VRPlot$PV6 <- c("PSF", "PS_A", "PSXC")
  VRPlot$PV7 <- c("QCF", "QCR", "QC_A", "QCFC", "QCRC", "TAS_A", "TASF", "TASR", "MACH_A", "MACHF", "MACHR")
  VRPlot$PV8 <- c("PSF", "PS_A", "QCF", "QC_A")
  VRPlot$PV9 <- c("IWD", "WDC", "IWS", "WSC", "WIC")
  VRPlot$PV10 <- c("GGVEW", "VEW", "VEWC", "GGVNS", "VNS", "VNSC", "GGQUAL")
  VRPlot$PV11 <- c("AKRD", "PITCH", "GGVSPD", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "ALT_A", "GGALT")
  VRPlot$PV14 <- NA
  VRPlot$PV15 <- c("CNCONC", "CONCU100_", "CONCU500_", "CONCU_", "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")
  VRPlot$PV16 <- c("DBARU_", "RICE")
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "PSXC", "ATX", "PSXC", "EWX")
  VRPlot$PV20 <- NA
  VRPlot$PV21 <- c("CUHSAS_", "TASX")
  VRPlot$PV22 <- NA
  VRPlot$PV23 <- c(NA)
  VRPlot$PV24 <- c("ATX", "TASX")
  VRPlot$PV25 <- c("ATX", "TASX")
  VRPlot$PV26 <- c("TASX", "ATX")
  VRPlot$PV27 <- c("TASX", "ATX")
  VRPlot$PV28 <- c("TASX", "ATX")
  VRPlot$PV29 <- c("TASX", "ATX")
  VRPlot$PV30 <- c("TASX", "ATX")
}

if (Project == "CONTRAST") {
  pitch_offset = 0.18
  roll_offset = -0.12
  thdg_offset = -0.54
  VRPlot <- list(PV1 = c("LATC", "LONC", "WDC", "WSC"))
  VRPlot$PV2 <- c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC")
  VRPlot$PV3 <- c("ATFH1", "ATFH2", "ATHR1", "ATHR2", "AT_A")
  VRPlot$PV4 <- VRPlot$PV3
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX", "CAVP_DPL", "CAVP_DPR", "PSXC", "LSRINT_VXL",
                  "EW_DPL", "EW_DPR", "EW_VXL", "MR", "QCXC", "AKRD", "MACHX")
  VRPlot$PV6 <- c("PSF", "PS_A", "PSXC")
  VRPlot$PV7 <- c("QCF", "QCR", "QC_A", "QCFC", "QCRC", "TAS_A", "TASF", "TASR", "MACH_A", "MACHF", "MACHR")
  VRPlot$PV8 <- c("PSF", "PS_A", "QCF", "QC_A")
  VRPlot$PV9 <- c("IWD", "WDC", "IWS", "WSC", "WIC")
  VRPlot$PV10 <- c("GGVEW", "VEW", "VEWC", "GGVNS", "VNS", "VNSC", "GGQUAL")
  VRPlot$PV11 <- c("AKRD", "PITCH", "GGVSPD", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "GVSPD_A", "GALT_A", "GGALT")
  VRPlot$PV14 <- c('VISBC', 'VISTC', 'VISTHT')
  VRPlot$PV15 <- c("CONC1DC_", "CONCD_", "CONCU100_", "CONCU500_", "CONCU_", "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")
  VRPlot$PV16 <- c("DBAR1DC_", "DBARD_", "DBARU_", "PLWC", "PLWC1DC_", "PLWCC", "PLWCD_", "RICE", "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC", "ATX", "PSXC", "EWX")
  VRPlot$PV20 <- c("CCDP_", "TASX")
  VRPlot$PV21 <- c("CUHSAS_", "TASX")
  VRPlot$PV22 <- c("C1DC_", "TASX")
  VRPlot$PV23 <- c('CORAW_AL', 'FO3_ACD', 'CO2C_PIC2311', 'H2O_PIC2311')
  VRPlot$PV24 <- c("ATX", "TASX")
  VRPlot$PV25 <- c("ATX", "TASX")
  VRPlot$PV26 <- c("TASX", "ATX")
  VRPlot$PV27 <- c("TASX", "ATX")
  VRPlot$PV28 <- c("TASX", "ATX")
  VRPlot$PV29 <- c("TASX", "ATX")
  VRPlot$PV30 <- c("TASX", "ATX")
}

if (Project == "SPRITE-II") {
  pitch_offset = 0.18
  roll_offset = -0.12
  thdg_offset = -0.54
  VRPlot <- list(PV1 = c("LATC", "LONC", "WDC", "WSC"))
  VRPlot$PV2 <- c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC")
  VRPlot$PV3 <- c("ATHR1", "ATHR2", "ATRL", "AT_A")
  VRPlot$PV4 <- VRPlot$PV3
  VRPlot$PV5 <- c("DP_DPL", "DP_DPR", "ATX", "CAVP_DPL", "CAVP_DPR", "PSXC", "EW_DPL", "EW_DPR", "MR",
                  "QCXC", "AKRD", "MACHX")
  VRPlot$PV6 <- c("PSF", "PS_A", "PSXC")
  VRPlot$PV7 <- c("QCF", "QCR", "QC_A", "QCFC", "QCRC", "TAS_A", "TASF", "TASR", "MACH_A", "MACH_A2")
  VRPlot$PV8 <- c("PSF", "PS_A", "QCF", "QC_A")
  VRPlot$PV9 <- c("WDC", "WSC", "WIC")
  VRPlot$PV10 <- NA
  VRPlot$PV11 <- c("AKRD", "PITCH", "GGVSPDB", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  VRPlot$PV12 <- NA
  VRPlot$PV13 <- NA
  VRPlot$PV14 <- NA
  VRPlot$PV15 <- NA
  VRPlot$PV16 <- NA
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "PSXC", "ATX", "PSXC", "EWX")
  VRPlot$PV20 <- NA
  VRPlot$PV21 <- NA
  VRPlot$PV22 <- NA
  VRPlot$PV23 <- c(NA)
  VRPlot$PV24 <- c("ATX", "TASX")
  VRPlot$PV25 <- c("ATX", "TASX")
  VRPlot$PV26 <- c("TASX", "ATX")
  VRPlot$PV27 <- c("TASX", "ATX")
  VRPlot$PV28 <- c("TASX", "ATX")
  VRPlot$PV29 <- c("TASX", "ATX")
  VRPlot$PV30 <- c("TASX", "ATX")
}

if (Project == "MPEX") {
  pitch_offset = 0.18
  roll_offset = -0.12
  thdg_offset = -0.54
  VRPlot <- list(PV1 = c("LATC", "LONC", "WDC", "WSC"))
  VRPlot$PV2 <- c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC")
  VRPlot$PV3 <- c("ATH2", "ATF1", "AT_A")
  VRPlot$PV4 <- VRPlot$PV3
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX", "CAVP_DPL", "CAVP_DPR", "PSXC",
                  "EW_DPL", "EW_DPR", "EW_VXL", "MR", "QCXC", "AKRD", "MACHX")
  VRPlot$PV6 <- c("PSF", "PS_A", "PSXC")
  VRPlot$PV7 <- c("QCF", "QCR", "QC_A", "QCFC", "QCRC", "TAS_A", "TASF", "TASR", "MACH_A", "MACHX")
  VRPlot$PV8 <- c("PSF", "PS_A", "QCF", "QC_A")
  VRPlot$PV9 <- c("WDC", "WSC", "WIC")
  VRPlot$PV10 <- c("GGVEW", "VEW", "VEWC", "GGVNS", "VNS", "VNSC", "GGQUAL")
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  VRPlot$PV12 <- NA
  VRPlot$PV13 <- NA
  VRPlot$PV14 <- NA
  VRPlot$PV15 <- NA
  VRPlot$PV16 <- NA
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "PSXC", "ATX", "PSXC", "EWX")
  VRPlot$PV20 <- NA
  VRPlot$PV21 <- NA
  VRPlot$PV22 <- NA
  VRPlot$PV23 <- c(NA)
  VRPlot$PV24 <- c("ATX", "TASX")
  VRPlot$PV25 <- c("ATX", "TASX")
  VRPlot$PV26 <- c("TASX", "ATX")
  VRPlot$PV27 <- c("TASX", "ATX")
  VRPlot$PV28 <- c("TASX", "ATX")
  VRPlot$PV29 <- c("TASX", "ATX")
  VRPlot$PV30 <- c("TASX", "ATX")
}


if (Project == "DC3") {
  pitch_offset = 0.70
  roll_offset = -0.192
  thdg_offset = -0.87
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- VRPlot$PV3
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX")
  ## don't use if CAVP not available:
  VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR", "PSXC", "LSRINT_VXL")
  ## use only if CAVP not available: will plot surrogate CAVP
  #  VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCFC", "LSRINT_VXL")
  ## list of vapor pressures to plot (there is no EW_VXL in HIPPO-2)
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "EW_VXL", "MR", "QCXC", "AKRD", "MACHX")  # H2OMR_GMD not in HIPPO-3 files?
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSF")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2",  "THDG", "THDG_IRS2")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  ## now radiation
  VRPlot$PV14 <- c(NA)
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCN", "CONCU_", "CONCU100_", "CONCU500_")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC1DC_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_", "DBAR1DC_", "PLWCD_", "PLWC1DC_", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "TASX")
  ## radiometers not present in HIPPO-2
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV21 <- c("CUHSAS_", "TASX")
  ## plot sample of 2DC size distributions
  VRPlot$PV22 <- c("C1DC_", "TASX")
  VRPlot$PV23 <- c("CORAW_AL", "CO2_PIC")
}

if (Project == "TREX") {
  pitch_offset = 0.178
  roll_offset = -0.192
  thdg_offset = -0.536
  pitch_offset = 0.41
  roll_offset = -0.27
  thdg_offset = -0.12  ## -0.32
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- VRPlot$PV3
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_DPL", "DP_DPR", "ATX")
  ## don't use if CAVP not available:
  # not available, PREDICT: VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR", "PSXC", "LSRINT_VXL")
  ## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCFC")
  ## list of vapor pressures to plot
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "MR")  # H2OMR_GMD not in HIPPO-3 files?
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSF")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TASDRY", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2",  "THDG", "THDG_IRS2")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  ## radiometers not present in START08
  VRPlot$PV14 <- c(NA)
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCN_WCN")  # top panel
  # VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC1DC_")   # 2nd panel
  # VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_LMO", "USCAT_LMO")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  # VRPlot$PV16 <- c("DBARD_", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  # VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  VRPlot$PV16 <- c("PLWCC", "PLWC")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  # VRPlot$PV20 <- c("CCDP_", "TASX")
  VRPlot$PV20 <- c(NA)
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV21 <- c(NA)
  ## plot sample of 2DC size distributions
  VRPlot$PV22 <- c(NA)
  # VRPlot$PV23 <- c("CORAW_AL")
  VRPlot$PV23 <- c(NA)
}

if (Project == "PACDEX") {
  pitch_offset = 0.178
  roll_offset = -0.192
  thdg_offset = -0.536
  pitch_offset = 0.41
  roll_offset = -0.27
  thdg_offset = -0.35  ## -0.32
  VRPlot <- list(PV1 = c("LATC", "LONC", "WDC", "WSC"))
  VRPlot$PV2 <- c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC")
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  VRPlot$PV4 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX", "PSXC", "QCFC", "EW_DPL", "EW_DPR", "MR")
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSF")
  VRPlot$PV7 <- c("QCF", "QCR", "QCFC", "QCRC", "QC_A", "TASF", "TASR", "TASDRY", "TAS_A", "MACHF", "MACHR", "MACH_A")
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A", 'AKRD')
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")
  VRPlot$PV10 <- c("GGVEW", "VEW", "VEWC", "GGVNS", "VNS", "VNSC", "GGQUAL")
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "ALT_A", "GGALT")
  VRPlot$PV14 <- c(NA)
  VRPlot$PV15 <- c("CONC1DC_", "CONCD_", "CONCN_WCN", "CONCU100_", "CONCU500_", "CONCU_", "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARU_", "DBARD_", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC", "ATX", "PSXC", "EWX")
  VRPlot$PV20 <- c("CCDP_", "TASX")
  VRPlot$PV21 <- c("CUHSAS_RWI")
  VRPlot$PV22 <- c("C1DC_", "TASX")
  VRPlot$PV23 <- NA
  VRPlot$PV24 <- c("TASX", "ATX")
  VRPlot$PV25 <- c("TASX", "ATX")
  VRPlot$PV26 <- c("TASX", "ATX")
  VRPlot$PV27 <- c("TASX", "ATX")
  VRPlot$PV28 <- c("TASX", "ATX")
  VRPlot$PV29 <- c("TASX", "ATX")
  VRPlot$PV30 <- c("TASX", "ATX")
}


if (Project == "START08") {
  pitch_offset = 0.178
  roll_offset = -0.192
  thdg_offset = -0.536
  pitch_offset = 0.41
  roll_offset = -0.27
  thdg_offset = -0.12  ## -0.32
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- VRPlot$PV3
  ## the next line should end with ATX and list dewpoints
  # VRPlot$PV5 <- c("DPV_VXL", "DP_DPL", "DP_DPR", "ATX")
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX")
  ## don't use if CAVP not available:
  # not available, PREDICT: VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR", "PSXC", "LSRINT_VXL")
  ## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCFC", "LSRINT_VXL")
  ## list of vapor pressures to plot
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "MR")  # H2OMR_GMD not in HIPPO-3 files?
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSF")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TASDRY", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2",  "THDG", "THDG_IRS2")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  ## radiometers not present in START08
  VRPlot$PV14 <- c(NA)
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCN_WCN")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC1DC_")   # 2nd panel
  # VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_LMO", "USCAT_LMO")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "TASX")
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  #VRPlot$PV21 <- c("CUHSAS_LMO", "CS200_", "TASX")
  VRPlot$PV21 <- c(NA)
  ## plot sample of 2DC size distributions
  VRPlot$PV22 <- c("C1DC_", "TASX")
  VRPlot$PV23 <- c("CORAW_AL")
}

if (Project == "TORERO") {
  pitch_offset = 0.178
  roll_offset = -0.192
  thdg_offset = -0.536
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- VRPlot$PV3
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX")
  ## don't use if CAVP not available:
  VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR", "PSXC", "LSRINT_VXL")
  ## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCFC", "LSRINT_VXL")
  ## list of vapor pressures to plot (there is no EW_VXL in HIPPO-2)
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "EW_VXL", "MR")  # H2OMR_GMD not in HIPPO-3 files?
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSF")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2",  "THDG", "THDG_IRS2")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  ## at present there is no RPlot14; UHSAS is handled later
  VRPlot$PV14 <- c("RSTB")
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCN", "CONCU_", "CONCU100_", "CONCU500_")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC1DC_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "TASX")
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV21 <- c("CUHSAS_", "TASX")
  ## plot sample of 2DC size distributions
  VRPlot$PV22 <- c("C1DC_", "TASX")
  VRPlot$PV23 <- c("CORAW_AL", "CO2_PIC")
}
if (Project == "HIPPO-1") {
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- c("ATH1", "ATH2", "ATF1", "AT_A")
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DPV_VXL", "DP_DPL", "DP_DPR", "ATX")
  VRPlot$PV5 <- c(VRPlot$PV5, "CAVPE_DPL", "CAVPE_DPR")
  ## don't use if CAVP not available:
  # VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR". "PSXC", "LSRINT_VXL")
  ## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCFC", "LSRINT_VXL")
  ## list of vapor pressures to plot (there is no EW_VXL in HIPPO-2)
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "MR", "H2OMR_GMD")
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSF")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TASDRY", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "ROLL", "THDG")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  ## radiometers not present
  VRPlot$PV14 <- c(NA)
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCU_", "CONCU100_", "CONCU500_")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_", "PLWCD_", "PLWCC", "PLWC", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "TASX")
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV21 <- c("CUHSAS_", "TASX")
  VRPlot$PV22 <- c(NA)
  VRPlot$PV23 <- c(NA)
  ## plot sample of 2DC size distributions
  ##VRPlot$PV23 <- c("C1DC_", "TASX")
  # VRPlot$PV30 <- c("CORAW_AL", "FO3_ACD", "COFLOW_AL", "INLETP_AL")
}

if (Project == "HIPPO-2") {
      ## track plot: don't change any exc. GGALT
      ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
      ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
      ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATH3", "ATH4", "AT_A")
      ## RPlot4: compare temperatures in pairs; specify up to five.
      ## first is reference for comparisons
  VRPlot$PV4 <- c("ATH1", "ATH2", "ATH3", "ATH4", "AT_A")
      ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DPV_VXL", "DP_DPL", "DP_DPR", "ATX")
  VRPlot$PV5 <- c(VRPlot$PV5, "CAVPE_DPL", "CAVPE_DPR")
## don't use if CAVP not available:
  # VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR". "PSXC", "LSRINT_VXL")
## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCFC", "LSRINT_VXL")
      ## list of vapor pressures to plot (there is no EW_VXL in HIPPO-2)
      ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "MR", "H2OMR_GMD")
      ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSF")
      ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
      ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
      ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TASDRY", "TAS_A")    #plot 7b-top
      ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
      ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
      ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")
      ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
      ## compare calculated AOA/SS vs measured to check sensitivity coefficients
      ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
      ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
      ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  VRPlot$PV14 <- c(NA)
      ## plot concentrations:
  VRPlot$PV15 <- c("CONCU_", "CONCU100_", "CONCU500_")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC1DC_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")  # 3rd panel
      ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
      ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
      ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
      ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
      ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
      ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "TASX")
      ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV21 <- c("CUHSAS_", "TASX")
      ## plot sample of 2DC size distributions
  VRPlot$PV22 <- c("C1DC_", "TASX")
  VRPlot$PV23 <- c(NA)
  # VRPlot$PV30 <- c("CORAW_AL", "FO3_ACD", "COFLOW_AL", "INLETP_AL")
}
if (Project == "HIPPO-3") {
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATH3", "ATH4", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- c("ATH1", "ATH2", "ATH3", "ATH4", "AT_A")
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX")
  ## don't use if CAVP not available:
  # VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR". "PSXC", "LSRINT_VXL")
  VRPlot$PV5 <- c(VRPlot$PV5, "CAVPE_DPL", "CAVPE_DPR")
  ## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCFC", "LSRINT_VXL")
  ## list of vapor pressures to plot (there is no EW_VXL in HIPPO-2)
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "MR")  # H2OMR_GMD not in HIPPO-3 files?
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSF")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TAS_A", "TASDRY")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  VRPlot$PV14 <- c(NA)
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCU_", "CONCU100_", "CONCU500_")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC1DC_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "TASX")
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV21 <- c("CUHSAS_", "TASX")
  ## plot sample of 2DC size distributions
  VRPlot$PV22 <- c("C1DC_", "TASX")
  # VRPlot$PV30 <- c("CORAW_AL", "FO3_ACD", "COFLOW_AL", "INLETP_AL")
  VRPlot$PV23 <- c(NA)
}
if (Project == "HIPPO-4" || Project == "HIPPO-5") {
  pitch_offset = 0.178
  roll_offset = -0.192
  thdg_offset = -0.536
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATX", "ATH4", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- VRPlot$PV3
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX")
  ## don't use if CAVP not available:
  VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR", "PSXC", "LSRINT_VXL")
  ## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCFC", "LSRINT_VXL")
  ## list of vapor pressures to plot (there is no EW_VXL in HIPPO-2)
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "EW_VXL", "MR")  # H2OMR_GMD not in HIPPO-3 files?
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSF")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TASDRY", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2",  "THDG", "THDG_IRS2")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  VRPlot$PV14 <- c(NA)
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCU_", "CONCU100_", "CONCU500_")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC1DC_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "TASX")
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV21 <- c("CUHSAS_", "TASX")
  ## plot sample of 2DC size distributions
  VRPlot$PV22 <- c("C1DC_", "TASX")
  # VRPlot$PV30 <- c("CORAW_AL", "FO3_ACD", "COFLOW_AL", "INLETP_AL")
  VRPlot$PV23 <- c(NA)
}
if (Project == "PREDICT" || Project == 'HEFT10' || Project == 'IDEAS-4') {
  pitch_offset = 0.178
  roll_offset = -0.192
  thdg_offset = -0.536
  pitch_offset = 0.41
  roll_offset = -0.27
  thdg_offset = -0.32
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATH1C", "ATH2C", "ATH3", "ATH4", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- VRPlot$PV3
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX")
  ## don't use if CAVP not available:
  # not available, PREDICT: VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR", "PSXC", "LSRINT_VXL")
  ## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCFC", "LSRINT_VXL")
  ## list of vapor pressures to plot
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "EW_VXL", "MR")  # H2OMR_GMD not in HIPPO-3 files?
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSF")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TASDRY", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WDS", "WSS", "WIC", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2",  "THDG", "THDG_IRS2")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  VRPlot$PV14 <- c(NA)
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCU_LMO", "CONCU100_LMO", "CONCU500_LMO", "CONCP_", "CONCN_WCN")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC1DC_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_LMO", "USCAT_LMO")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_LMO", "DBARP_OPC", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "TASX")
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV21 <- c("CUHSAS_LMO", "CS200_OPC", "TASX")
  ## plot sample of 2DC size distributions
  VRPlot$PV22 <- c("C1DC_", "TASX")
  # VRPlot$PV30 <- c("CORAW_AL", "FO3_ACD", "COFLOW_AL", "INLETP_AL")
  VRPlot$PV23 <- c(NA)
}

if (Project == "DC3-TEST") {
  pitch_offset = 0.178
  roll_offset = -0.192
  thdg_offset = -0.536
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATH1", "ATH2", "ATH3", "ATH4", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- VRPlot$PV3
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX")
  ## don't use if CAVP not available:
  VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR", "PSXC", "QCFC", "LSRINT_VXL")
  ## use only if CAVP not available: will plot surrogate CAVP
  # VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCFC", "LSRINT_VXL")
  ## list of vapor pressures to plot
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "MR")  # H2OMR_GMD not in HIPPO-3 files?
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSF")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TASDRY", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  ## compare IRU attitude angles, IRS1 and IRS3
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS3", "ROLL", "ROLL_IRS3",  "THDG", "THDG_IRS3")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS3", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  VRPlot$PV14 <- c(NA)
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCU_", "CONCU100_", "CONCU500_")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC1DC_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "TASX")
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV21 <- c("CUHSAS_", "TASX")
  ## plot sample of 2DC size distributions
  VRPlot$PV22 <- c("C1DC_", "TASX")
  # VRPlot$PV30 <- c("CORAW_AL", "FO3_ACD", "COFLOW_AL", "INLETP_AL")
  VRPlot$PV23 <- c(NA)
}

if (Project == "WINTER") {
  pitch_offset = 0.178
  roll_offset = -0.192
  thdg_offset = -0.536
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATHL1", "ATHL2", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- VRPlot$PV3
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_VXL", "DP_DPB", "DP_DPT", "ATX")
  ## don't use if CAVP not available:
  VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPB", "CAVP_DPT", "PSXC", "LSRINT_VXL")
  ## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCXC", "LSRINT_VXL")
  ## list of vapor pressures to plot (there is no EW_VXL in HIPPO-2)
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPB", "EW_DPT", "EW_VXL", "MR")  # H2OMR_GMD not in HIPPO-3 files?
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSFD")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCFR", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFRC", "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSFD", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2",  "THDG", "THDG_IRS2")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  ## at present there is no RPlot14; UHSAS is handled later
  VRPlot$PV14 <- c("RSTB", 'RSTT')
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCN", "CONCU_", "CONCU100_", "CONCU500_")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONCF_", "CONC1DC_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "CS100_", "TASX")
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV21 <- c("CUHSAS_", "CS200_", "TASX")
  ## plot sample of 2DC size distributions
  VRPlot$PV22 <- c("C1DC_", "TASX")
  VRPlot$PV23 <- c("CORAW_AL", "CO2_PIC2311")
}

if (Project == "NOMADSS") {
  pitch_offset = 0.0
  roll_offset = 0.0
  thdg_offset = 0.0
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures
  VRPlot$PV3 <- c("ATHL1", "ATHL2", "ATRL", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- VRPlot$PV3
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_DPB", "DP_DPT", "DP_UVH", "ATX")
  ## don't use if CAVP not available:
  VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPB", "CAVP_DPT", "PSXC")
  ## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSXC", "QCXC")
  ## list of vapor pressures to plot (there is no EW_VXL in NOMADSS)
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPB", "EW_DPT", "EW_UVH", "MR")  
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSXC", "PS_A", "PSFC", "PSFDC")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCFR", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFRC", "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASFR", "TASR", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHFR", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSFD", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_A", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS")
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2",  "THDG", "THDG_IRS2")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_A", "GGALT", "ALT_A")
  ## at present there is no RPlot14; UHSAS is handled later
  VRPlot$PV14 <- c("RSTB", 'RSTB1')
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCN", "CONCU_", "CONCU100_", "CONCU500_")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC3_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_", "DBAR3_", "DBARP_", "PLWCD_", "PLWCC1", "PLWC1", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSXC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSXC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSXC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "CS100_", "TASX")
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV21 <- c("CUHSAS_", "CS200_", "CS300_", "TASX")
  ## plot sample of 2DC size distributions
  VRPlot$PV22 <- NA
  VRPlot$PV23 <- c("CORAW_AL", "CO2_PIC2311", "CO2_PIC1301")
}
