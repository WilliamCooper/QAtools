### plot 23: chemistry (CO, O3)

RPlot23 <- function (data, Seq=NA) { 
  ## needs COFLOW_AL, CORAW_AL, INLETP_AL, FO3_ACD, CO2_PIC
  if (!('CORAW_AL' %in% names (data))) {return ()}
  op <- par (mfrow=c(2,1), mar=c(5,5,2,2)+0.1,oma=c(1.1,0,0,0))
  ## beware of all-missing case:
  if (!any(!is.na(data$CORAW_AL))) {return ()}

  # plot CORAW
  if ("CORAW_AL" %in% names(data)) {data$CORAW_AL <- 0.001*data$CORAW_AL}
  plotWAC (data[, c("Time", VRPlot$PV23)],
         ylab="ppmv",
         lty=c(1,1), lwd=c(2), legend.position='bottomright',
         col='red')
  title("CORAW", cex.main=0.8)
  if ("FO3_ACD" %in% names (data)) {
    plotWAC (data[, c("Time", "FO3_ACD")], ylab="FO3 [ppbv]")
  }
  AddFooter ()
  if (!is.na(Seq) && (Seq == 1)) {return()}
  # plot COFLOW and INLETP
  if (("COFLOW_AL" %in% names (data)) && any(!is.na(data$COFLOW_AL))) {
    plotWAC(data[, c("Time", "COFLOW_AL")])
  }
  if ("INLETP_AL" %in% names (data)) {
    plotWAC(data[, c("Time","INLETP_AL")])
  }
  #legend('bottomright', legend=c("COFLOW", "INLETP"), pch=20, col=c('red', 'blue'))
  AddFooter ()
}

