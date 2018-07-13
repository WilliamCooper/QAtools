### plot 23: chemistry (CO, O3)

RPlot23 <- function (data, Seq=NA) { 
  ## needs COFLOW_AL, CORAW_AL, INLETP_AL, FO3_ACD, CO2_PIC
  # if (!('CORAW_AL' %in% names (data))) {
  #   plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
  #   text (0.5, 0.8, 'no CO measurements')
  #   return ()
  # }
  # op <- par (mfrow=c(2,1), mar=c(5,5,2,2)+0.1,oma=c(1.1,0,0,0))
  # ## beware of all-missing case:
  # if (!any(!is.na(data$CORAW_AL))) {return ()}
  # 
  # # plot CORAW
  # if ("CORAW_AL" %in% names(data)) {data$CORAW_AL <- 0.001*data$CORAW_AL}
  # plotWAC (data[, c("Time", "CORAW_AL")],
  #        ylab="ppmv",
  #        lty=c(1,1), lwd=c(2), legend.position='bottomright',
  #        col='red')
  # title("CORAW", cex.main=0.8)
  # 
  
  layout(matrix(1:4, ncol = 1), widths = 1)#, heights = c(5,5,5,5))
  op <- par (mar=c(5,5,5,1),oma=c(2,2,2,1))
  par(cex.lab=2, cex.main=2)
  
  # plot CO2
  if ("CO2_PIC2311" %in% names(data)) {
    if ("CO2C_PIC2311" %in% names(data)) {
      plotWAC (data[, c("Time", "CO2_PIC2311", "CO2C_PIC2311")], 
               ylab="ppmv",
               lty=c(1,1), lwd=c(2), legend.position='bottomright')
         } else {
      plotWAC (data[, c("Time", "CO2_PIC2311")], ylab="ppmv",
               lty=c(1,1), lwd=c(2), legend.position='bottomright')
    }
  }
  
  # plot CH4
  
  if ("CH4_PIC2311" %in% names(data)) {
    if ("CH4C_PIC2311" %in% names(data)) {
      plotWAC (data[, c("Time", "CH4_PIC2311", "CH4C_PIC2311")], 
               ylab="ppmv",
               lty=c(1,1), lwd=c(2), legend.position='bottomright')
    } else {
      plotWAC (data[, c("Time", "CH4_PIC2311")], ylab="ppmv",
               lty=c(1,1), lwd=c(2), legend.position='bottomright')
    }
  }
  
  # plot OZONE with Methane, if available
  if ("FO3C_ACD" %in% names(data)) {
    if ("CORAW" %in% names(data)) {
      plotWAC (data[, c("Time", "FO3C_ACD", "CORAW")], 
               ylab="ppmv",
               lty=c(1,1), lwd=c(2), legend.position='bottomright')
    } else {
      plotWAC (data[, c("Time", "FO3C_ACD")], ylab="ppmv",
               lty=c(1,1), lwd=c(2), legend.position='bottomright')
    }
  }
  # AddFooter ()
  # if (!is.na(Seq) && (Seq == 1)) {return()}
  # # 
  # plot COFLOW and INLETP
  # if (("COFLOW_AL" %in% names (data)) && any(!is.na(data$COFLOW_AL))) {
  #   plotWAC(data[, c("Time", "COFLOW_AL")])
  # }
  if ("INLETP_AL" %in% names (data)) {
    plotWAC(data[, c("Time","INLETP_AL")])
  }
  #legend('bottomright', legend=c("COFLOW", "INLETP"), pch=20, col=c('red', 'blue'))
  AddFooter ()
}

