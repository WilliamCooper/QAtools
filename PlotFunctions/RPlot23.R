### plot 23: chemistry (CO, O3)

RPlot23 <- function (data, Seq=NA, panl=1) { 
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  panel11 <- function (data) {
    par(cex.lab=1, cex.main=1)
    # plot CO2
    if ("CO2C_PIC2401" %in% names(data)) {
      if ("CO2_PIC2401" %in% names(data)) {
        ifelse (exists ('panel1ylim'),
          plotWAC (data[, c("Time", "CO2_PIC2401", "CO2C_PIC2401")], 
            ylab="ppmv", lty=c(1,1), lwd=c(2), ylim=panel1ylim),
          plotWAC (data[, c("Time", "CO2_PIC2401", "CO2C_PIC2401")], 
            ylab="ppmv", lty=c(1,1), lwd=c(2))
        )
      } else {
        ifelse (exists ('panel1ylim'),
          plotWAC (data[, c("Time", "CO2C_PIC2401")], ylab="ppmv",
            lty=c(1,1), lwd=c(2), ylim=panel1ylim),
          plotWAC (data[, c("Time", "CO2C_PIC2401")], ylab="ppmv",
            lty=c(1,1), lwd=c(2))
        )
      }
      title('Carbon Dioxide')
    }
  }
  
  panel12 <- function (data) {
    # plot CH4
    
    if ("CH4C_PIC2401" %in% names(data)) {
      if ("CH4_PIC2401" %in% names(data)) {
        ifelse (exists ('panel2ylim'),
          plotWAC (data[, c("Time", "CH4_PIC2401", "CH4C_PIC2401")], 
            ylab="ppmv", lty=c(1,1), lwd=c(2), ylim=panel2ylim),
          plotWAC (data[, c("Time", "CH4_PIC2401", "CH4C_PIC2401")], 
            ylab="ppmv", lty=c(1,1), lwd=c(2))
        )
      } else {
        ifelse (exists ('panel2ylim'),
          plotWAC (data[, c("Time", "CH4C_PIC2401")], ylab="ppmv",
            lty=c(1,1), lwd=c(2), ylim=panel2ylim),
          plotWAC (data[, c("Time", "CH4C_PIC2401")], ylab="ppmv",
            lty=c(1,1), lwd=c(2))
        )
      }
      title('Methane')
    }
  }
  
  panel13 <- function (data) {
    # plot OZONE with Methane, if available
    idx<-match(c("CO_PIC2401", "FO3C_ACD", "CO_ARI"), names(data))
    idx<-idx[is.finite(idx)==TRUE]
    if (length(idx)>0) {
      SF<-vector ()
      dummyDF<-as.data.frame(data[,names(data[idx])])
      for (j in 1:ncol(dummyDF)){ 
        id<-which(!is.finite(dummyDF[,j]))
        if (length(id)>0){
          dummyDF[id,j]<-NA
          print(names(dummyDF)[j])
          print(range(dummyDF[,j],na.rm=TRUE))
        }
      }
      if ('CO_PIC2401' %in% names(dummyDF)){
        dummyDF$CO_PIC2401<-dummyDF$CO_PIC2401*1000.
        
        if ('FO3C_ACD' %in% names(dummyDF)){
          scalefactor<-mean(dummyDF$CO_PIC2401,na.rm=T)/mean(dummyDF$FO3C_ACD,na.rm=T)
          dummyDF$FO3C_ACD<-dummyDF$FO3C_ACD*scalefactor
          SF<-scalefactor
        } 
        
      }
      DF<-data.frame(Time=data$Time)
      DF<-cbind(DF,dummyDF)
      ylm<-c(max(c(0,min(dummyDF, na.rm=TRUE))),
        min(c(max(dummyDF, na.rm=TRUE),400)))
      if (ylm[2]<ylm[1] | 'FALSE' %in% is.finite(ylm)){ ylm<-c(0,400)}
      print(ylm)
      ifelse (exists ('panel3ylim'),
        plotWAC (DF, 
          ylab="ppb", 
          ylim=panel3ylim,
          lty=c(1,1), lwd=c(2)),
        plotWAC (DF, 
          ylab="ppb", 
          ylim=ylm,
          lty=c(1,1), lwd=c(2))
      )
      title(paste('Carbon Monoxide and Fast Ozone (if available, scaled',toString(round(SF,2)),')'))
    }
  }
  
  panel14 <- function (data) {
    # if (!is.na(Seq) && (Seq == 1)) {return()}
    # # 
    # plot COFLOW and INLETP
    # if (("COFLOW_AL" %in% names (data)) && any(!is.na(data$COFLOW_AL))) {
    #   plotWAC(data[, c("Time", "COFLOW_AL")])
    # }
    if ("INLETP_AL" %in% names (data)) {
      ifelse (exists ('panel4ylim'),
        plotWAC(data[, c("Time","INLETP_AL")], 
          ylim=panel4ylim),
        plotWAC(data[, c("Time","INLETP_AL")], 
          ylim=c(range(data[,"INLETP_AL"], na.rm=TRUE) ))
      )
      title('Inlet Pressure')
    }
    #legend('bottomright', legend=c("COFLOW", "INLETP"), pch=20, col=c('red', 'blue'))
  }
  
  
  ###############################################################
  if (shinyDisplay) {
    op <- par (mfrow=c(1,1), mar=c(5,5,1,1)+0.1,oma=c(1.1,0,0,0))
    switch (panl,
      {
        op <- par (mar=c(1,5,1,1)+0.1)
        panel11(data)
      },
      {
        op <- par (mar=c(1,5,1,1)+0.1)
        panel12(data)
      },
      {
        op <- par (mar=c(1,5,1,1)+0.1)
        panel13(data)
      },
      {
        panel14(data)
        AddFooter()
      }
    )
    
    ###############################################################
  } else {
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
    
    # layout(matrix(1:4, ncol = 1), widths = 1)
    # op <- par (mar=c(5,5,5,1),oma=c(0,3,0,3))
    # par(cex.lab=2, cex.main=2)
    layout (matrix(1:4, ncol=1), widths=1, heights=c(5,5,5,6))
    op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
    
    
    if (is.na(Seq) || Seq == 1) {
      panel11(data)
      panel12(data)
      panel13(data)
      op <- par (mar=c(5,4,1,1)+0.1)
      panel14(data)
      AddFooter ()
      if (!is.na(Seq) && (Seq == 1)) {return()}
    }
    
    # END OF FIRST PLOT PAGE
    # 
    if (is.na(Seq) || Seq == 2) {
      ## nothing here for now
    }
  }
}
