### plot 20: UHSAS/PCASP CDP/CS100/CS200 2DC-10/2DC-25 size distributions
RPlot20 <- function (data, Seq=NA) {
  layout(matrix(c(1,1,0,2,2,0,3,3,0), ncol = 3, byrow=TRUE))
  op <- par (mar=c(5,5,5,1),oma=c(0,3,0,3))
  par(cex.lab=2, cex.main=2)
  
   if (is.na (VRPlot$PV20) || (length(VRPlot$PV20) < 1)) {
    plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
    text (0.5, 0.8, 'Size Distributions Not Ready')
    return ()}
  ## needs CCDP_xxx and SP100_xxx; references fname from calling environment
  # ptm <- proc.time()
  # tic('RPlot20')
#  print (sprintf('entry to RPlot20, Seq=%d: names in data are', Seq))
  #print (sort(names(data)))
  #print (str(data))

# UHSAS/ PCASP
  if (is.na (Seq) || (Seq == 1)) {  
  kount = 0
  plotTest <- 50
  nms <- names(data)
  nm1 <- nm2 <- character(0)
  if (length (grep ("CUHSAS_", VRPlot[[20]])) > 0) {
    nm1 <- nms[grep ('CUHSAS_', nms)]
    CellLimitsU <- attr(data[,nm1[1]], 'CellSizes')
   print(nm1)
  }
  if (length (grep ("CS100_|CS200_", VRPlot[[20]])) > 0) {
    nm2 <- nms[grep ('CS100_|CS200_', nms)]
    CellLimitsP <- attr(data[,nm2[1]], 'CellSizes')
    print(nm2)
  }
  

  idx1 <- getIndex (data$Time, StartTime)
   if (idx1 < 1) {idx1 <- 1}
  ## reference to calling environment for StartTime
  jstart <- ifelse (StartTime > 0, idx1, 1)
  
  # print(StartTime)
  
  if (length(nm1) > 0) {
    # data[, nm1[1]] <- data[, nm1[1]] / diff(log10(CellLimitsU))
    #data[, nm1[1]][data[, nm1[1]] <= 0] <-  1e-4 # Can these not be set to NA???
    Y1 <- apply(data[seq(idx1,idx1+59,by=1),nm1[[1]]],2,mean,na.rm=TRUE )
    Y1 <- Y1 / diff(log10(CellLimitsU))
    Y1[Y1<=0]<-1e-4 # for log plotting
    
    # Plot UHSAS here
    plot ((CellLimitsU*1000), c(1.e-4, Y1), type='S',ylim=c(1,1.e5), yaxt='n',
          ylab=expression('dN/dlog'[10]*'D'),cex.axis=2,
          xlab=expression('Diameter [nm]'), log="xy", col='blue', lwd=2, xlim=c(50,3000))
    tckmarks<-c(1,10,100,1000,10000,100000)
    axis(2,at = tckmarks,#, 1000000), 
         labels=c(expression(10^0),expression(10^1), expression(10^2),
                  expression(10^3), expression(10^4),expression(10^5)), 
                  #expression(10^6)), 
                  cex.axis=2)
     for (tt in 1:length(tckmarks)){
      abline(h=tckmarks[tt], lty=2)
    }
  }
 
  
  if (length(nm2) > 0) {
    #CPCASP <- sum(data[j, nm2[1]], na.rm=TRUE)
    # data[, nm2[1]][data[, nm2[1]] <= 0] <- 1e-4 # Can these not be set to NA???
    Y1 <- apply(data[seq(idx1,idx1+59,by=1),nm2[[1]]],2,mean,na.rm=TRUE )
    PCASP_N<<-Y1
    PCASP_D<<-diff(log10(CellLimitsP))
    Y1 <- Y1 / diff(log10(CellLimitsP))
    PCASP_R<<-Y1
    Y1[Y1<=0]<-1e-4 # for log plotting
    print('PCASP data')
    print(Y1)
    print(CellLimitsP)
  
    
    # Plot PCASP here
    
   if (length(nm1)>0){
     lines ((CellLimitsP*1000), c(1.e-4, Y1), type='S', col='orangered', lwd=2)
    # Testing below how different plotting affects histogram
     # lines ((CellLimitsP*1000), c(Y1,1.e-4), type='S', col='cyan', lwd=2)
    #lines ((CellLimitsP*1000), c(1.e-4, Y1), type='s', col='gray40', lwd=2)
     
    # tckmarks<-c(1,10,100,1000,10000,100000)
    # axis(2,at = tckmarks,#, 1000000), 
    #      labels=c(expression(10^0),expression(10^1), expression(10^2),
    #               expression(10^3), expression(10^4),expression(10^5)), 
    #      #expression(10^6)), 
    #      cex.axis=2)
    title(paste(nm1,',',nm2,'Number Size Distributions'))
    text(2000, 10^4+3000,'PCASP*', col='red', cex=2)
    mtext('Use time range slider to adjust one-minute forward averaging window', side=3)
    
   } else{
     title(paste(nm1,'Number Size Distributions'))
     mtext('Use time range slider to adjust one-minute forward averaging window', side=3)
     plot ((CellLimitsP*1000), c(1.e-4, Y1), type='S',ylim=c(1,1.e5), yaxt='n',
           ylab=expression('dN/dlog'[10]*'D'),cex.axis=2,
           xlab=expression('Diameter [nm]'), log="xy", col='blue', lwd=2)
     tckmarks<-c(1,10,100,1000,10000,100000)
     axis(2,at = tckmarks,#, 1000000), 
          labels=c(expression(10^0),expression(10^1), expression(10^2),
                   expression(10^3), expression(10^4),expression(10^5)), 
          #expression(10^6)), 
          cex.axis=2)
     title(paste(nm1,',',nm2,'Number Size Distributions'))
     mtext('Use time range slider to adjust one-minute forward averaging window', side=3)
    
   }
    #mtext('Use time range slider to adjust one-minute forward averaging window', side=3)
    for (tt in 1:length(tckmarks)){
      abline(h=tckmarks[tt], lty=2)
    }
  }
    
 
# End UHSAS/PCASP size distribution
# Begin CDP/2DC :: 'CCDP_RPC','C1DC_LPO','C1DC_LPC
    nm1 <- nm2 <- character(0)
    if (length (grep ("CCDP_", VRPlot[[20]])) > 0) {
      nm1 <- nms[grep ('CCDP_', nms)]
      CellLimitsC <- attr(data[,nm1[1]], 'CellSizes')
      print(nm1)
    }
    if (length (grep ("C1DC_", VRPlot[[20]])) > 0) {
      nm2 <- nms[grep ('C1DC_', nms)]
      print(nm2)
    }
    
    if (length(nm1) > 0) {
      data[, nm1[1]] <- data[, nm1[1]] / diff(log10(CellLimitsC))
      data[, nm1[1]][data[, nm1[1]] <= 0] <-  1e-4 # Can these not be set to NA???
      
      # Plot CDP here
      Y1 <- apply(data[seq(idx1,idx1+59,by=1),nm1[[1]]],2,mean,na.rm=TRUE )
      plot ((CellLimitsC*1000), c(1.e-4, Y1), type='S',ylim=c(1,1.e5), yaxt='n',
            ylab=expression('dN/dlog'[10]*'D'),cex.axis=2,
            xlab=expression('Diameter [nm]'), log="xy", col='blue', lwd=2)#, xlim=c(50,3000))
      tckmarks<-c(1,10,100,1000,10000,100000)
      axis(2,at = tckmarks,#, 1000000), 
           labels=c(expression(10^0),expression(10^1), expression(10^2),
                    expression(10^3), expression(10^4),expression(10^5)), 
           #expression(10^6)), 
           cex.axis=2)
      for (tt in 1:length(tckmarks)){
        abline(h=tckmarks[tt], lty=2)
      }
      title(paste(nm1,'Number Size Distributions'))
        mtext('Use time range slider to adjust one-minute forward averaging window', side=3)
    }
    
    return()
    
    if (length(nm2) > 0 & length(nm1)>0) {
      for (j in 1:length(nm2)){
        CellLimitsD <- attr(data[,nm2[j]], 'CellSizes')
        print(CellLimitsD)
        print(diff(log10(CellLimitsD)))
        data[, nm2[j]] <- data[, nm2[j]] / diff(log10(CellLimitsD))
        data[, nm2[j]][data[, nm2[j]] <= 0] <- 1e-4 # Can these not be set to NA???
      # Plot PCASP here
      Y1 <- apply(data[seq(idx1,idx1+59,by=1),nm2[[j]]],2,mean,na.rm=TRUE )
    
        lines ((CellLimitsD*1000), c(1.e-4, Y1), type='S', col='orangered', lwd=2)
        # tckmarks<-c(1,10,100,1000,10000,100000)
        # axis(2,at = tckmarks,#, 1000000), 
        #      labels=c(expression(10^0),expression(10^1), expression(10^2),
        #               expression(10^3), expression(10^4),expression(10^5)), 
        #      #expression(10^6)), 
        #      cex.axis=2)
      }
        title(paste(nm1,',',nm2,'Number Size Distributions'))
        text(2000, 10^4+3000,'1DC', col='red', cex=2)
        mtext('Use time range slider to adjust one-minute forward averaging window', side=3)
        
      } 
    if (length(nm2) > 0 & length(nm1)<1) {
        title(paste(nm1,'Number Size Distributions'))
        mtext('Use time range slider to adjust one-minute forward averaging window', side=3)
        for (j in 1:length(nm2)){
          CellLimitD <- attr(data[,nm2[j]], 'CellSizes')
          data[, nm2[j]] <- data[, nm2[j]] / diff(log10(CellLimitsD))
          data[, nm2[j]][data[, nm2[j]] <= 0] <- 1e-4 # Can these not be set to NA???
          # Plot PCASP here
          Y1 <- apply(data[seq(idx1,idx1+59,by=1),nm2[[j]]],2,mean,na.rm=TRUE )
          
          if (j == 1){
            plot ((CellLimitsD*1000), c(1.e-4, Y1), type='S',ylim=c(1,1.e5), yaxt='n',
                  ylab=expression('dN/dlog'[10]*'D'),cex.axis=2,
                  xlab=expression('Diameter [nm]'), log="xy", col='blue', lwd=2)
            tckmarks<-c(1,10,100,1000,10000,100000)
            axis(2,at = tckmarks,#, 1000000), 
                 labels=c(expression(10^0),expression(10^1), expression(10^2),
                          expression(10^3), expression(10^4),expression(10^5)), 
                 #expression(10^6)), 
                 cex.axis=2)
            title(paste(nm1,',',nm2,'Number Size Distributions'))
            mtext('Use time range slider to adjust one-minute forward averaging window', side=3) 
          } else { 
          lines ((CellLimitsD*1000), c(1.e-4, Y1), type='S', col='orangered', lwd=2)
          # tckmarks<-c(1,10,100,1000,10000,100000)
          # axis(2,at = tckmarks,#, 1000000), 
          #      labels=c(expression(10^0),expression(10^1), expression(10^2),
          #               expression(10^3), expression(10^4),expression(10^5)), 
          #      #expression(10^6)), 
          #      cex.axis=2)
          }
        }
        
      
      #mtext('Use time range slider to adjust one-minute forward averaging window', side=3)
      for (tt in 1:length(tckmarks)){
        abline(h=tckmarks[tt], lty=2)
      }
    }
  
  
  
  return() 
  if (idx1 < 1) {idx1 <- 1}
  ## reference to calling environment for StartTime
  jstart <- ifelse (StartTime > 0, idx1, 1)
  # print (sprintf ("start time in RPlot21 is %d and jstart is %d\n",
  #                 StartTime, jstart))
  # I don't think we want these next two lines anymore
  CV <- nms[grep ('CONCU_', nms)][1]
  iw <- which(data[, CV] > plotTest)  ## get list of indices where CONCU > PlotTest
  if (length(nm1) > 0) {
    nm <- nm1[1]
    # for (j in jstart:length(Time)) {
    for (j in iw) {
      if (is.na(data$Time[j])) {next}
      if (!is.na(data$TASX[j]) && (data$TASX[j] < 60)) {next}
      CUHSAStot <- sum (data[j, nm], na.rm=TRUE)
      ## convert distributions to number per cm^3 per um
      data[j, nm] <- data[j, nm] / diff(CellLimitsU)
      data[j, nm][data[j, nm] <= 0] <- 1e-4
      if (length (nm1) > 1) {
        CUHSAS2tot <- sum (data[j, nm1[2]], na.rm=TRUE)
        data[j, nm1[2]] <- data[j, nm1[2]] / diff(CellLimitsU)
        data[j, nm1[2]][data[j, nm1[2]] <= 0] <- 1e-4
      }
      if (length(nm2) > 0) {
        CPCASP <- sum(data[j, nm2[1]], na.rm=TRUE)
        data[j, nm2[1]] <- data[j, nm2[1]] / diff(CellLimitsP)
        data[j, nm2[1]][data[j, nm2[1]] <= 0] <- 1e-4
      }
      if ((any(data[j, nm] > 1, na.rm=TRUE))) {
        kount <- kount + 1
        if (is.na (Seq) || (kount > (Seq-1)*6)) {
          ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
                  op <- par (mar=c(5.2,2,1,1)+0.1))
 # PLOT at next line
                   plot (CellLimitsU, c(1.e-4, data[j, nm]), type='S', ylim=c(1,1.e6),
                xlab="Diameter [um]", log="y", col='blue', lwd=2)
        } 
        if (length(nm2) > 0) {
          lines (CellLimitsP, c(1.e-4, data[j, nm2[1]]), type='S', col='magenta', lty=3)
        }
        if (length(nm1) > 1) {
          lines (CellLimitsU, c(1.e-4,data[j, nm1[2]]), type='S', col='forestgreen', lty=2)
          legend('bottomright', legend=nm1, text.col=c('blue', 'forestgreen'), 
                 col=c('blue', 'forestgreen'), lty=c(1,2), lwd=c(2,1))
        }
        title(sprintf("size distribution, Time=%s", strftime (data$Time[j], format="%H:%M:%S", tz='UTC')), 
              cex.main=.75)
        if (length (nm2) > 0) {
          legend ("topright", legend=c("UHSAS", "PCASP"), col=c('blue', 'magenta'), 
                  lwd=c(2,1), cex=0.75) 
        } else {
          legend ('topright', legend='UHSAS', col='blue', text.col='blue')
        }
        if (kount%%6==0)   AddFooter ()
      }
      if (!is.na(Seq) && (kount >= (Seq*6))) {break}
      if (kount >= 24) {break}
    }
  }
  
  if (!is.na(Seq) && (Seq == 1)) {return()}
}
# CDP   
  kount = 0
  nms <- names(data)
  nm1 <- nm2 <- character(0)
  if (length (grep ("CCDP_", VRPlot[[20]])) > 0) {
    nm1 <- nms[grep ('CCDP_', nms)]
    CellLimitsD <- attr(data[,nm1[1]], 'CellSizes')
    #print (sprintf ('CCDP sizes in %s', nm1[1]))
    #print (CellLimitsD)
  }
  if (length (grep ("CS100_|CS200_", VRPlot[[20]])) > 0) {
    nm2 <- nms[grep ('CS100_|CS200', nms)]
    CellLimitsF <- attr(data[,nm2[1]], 'CellSizes')
  }
  if (length (nm1) < 1 && length (nm2) < 1) {
    plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
    text (0.5, 0.8, 'no cloud-droplet measurements')
    return ()
  }
  
  idx1 <- getIndex (data$Time, StartTime)
  if (idx1 < 1) {idx1 <- 1}
  ## reference to calling environment for StartTime
  jstart <- ifelse (StartTime > 0, idx1, 1)
  #print (sprintf ("start time in RPlot20 is %d and jstart is %d",
    #StartTime, jstart))
  CV <- nms[grep ('CONCD_', nms)][1]
  iw <- which(data[, CV] > 1)  ## get list of indices where CONCD > 1
  if (length(nm1) > 0) {
    nm <- nm1[1]
      # for (j in jstart:length(data$Time)) {
    for (j in iw) {    ## (about x10 faster than the preceding statement)
      if (is.na(data$Time[j])) {next}
      if (is.na(data$TASX[j]) || data$TASX[j] < 60) {next}
      # if (any(is.na(data[j, nm]))) {next}
      # if (all(data[j, nm] < 1)) {next}
      ## convert distributions to number per cm^3 per um
      CDPtot <- sum (data[j, nm], na.rm=TRUE)
      data[j, nm] <- data[j, nm] / diff(CellLimitsD)
      data[j, nm][data[j, nm] <= 0] <- 1e-4
      if (length (nm1) > 1) {
        CDPi2tot <- sum (data[j, nm1[2]], na.rm=TRUE)
        data[j, nm1[2]] <- data[j, nm1[2]] / diff(CellLimitsD)
        data[j, nm1[2]][data[j, nm1[2]] <= 0] <- 1e-4
      }
      if (length(nm2) > 0) {
        CFSSP <- sum(data[j, nm2[1]], na.rm=TRUE)
        data[j, nm2[1]] <- data[j, nm2[1]] / diff(CellLimitsF)
        data[j, nm2[1]][data[j, nm2[1]] <= 0] <- 1e-4
      }
      if ((any(data[j, nm] > 1, na.rm=TRUE))) {
        kount <- kount + 1
        # print (sprintf ('kount is %d, j is %d', kount, j))
        if (is.na (Seq) || (kount > (Seq-1)*6)) {
          ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
            op <- par (mar=c(5.2,2,1,1)+0.1))
          # print (sprintf ('j=%d, nm=%s', j, nm))
          # print (c('CellLimitsD', CellLimitsD))
          # print (data[j, nm])
          ## Type 'S' draws line in vertical, then horizontal, so spans the bin correctly
          ## if x_i is the upper size limit of bin i and x_(i+1) that of bin i+1,
          ## because the line should be drawn at the height of bin i+1, moving
          ## at the level of the concentration in bin i+1. This works if there are
          ## 
          plot (CellLimitsD, c(1.e-4, data[j, nm]), type='S', ylim=c(1.e-1,1.e3), 
            xlab="Diameter [um]", log="y", col='blue', lwd=2)
          if (length (nm2) > 0) {
            lines (CellLimitsF, c(1.e-4, data[j, nm2[1]]), type='S', col='magenta', lty=3)
          }
          if (length(nm1) > 1) {
            lines (CellLimitsD, c(1.e-4,data[j, nm1[2]]), type='S', col='forestgreen', lty=2)
            legend('bottomright', legend=nm1, text.col=c('blue', 'forestgreen'), 
              col=c('blue', 'forestgreen'), lty=c(1,2), lwd=c(2,1))
          }
          title(sprintf("Time=%s CONCD=%.1f", strftime (data$Time[j], format="%H:%M:%S", tz='UTC'), CDPtot), 
            cex.main=1)
          legend ("topright", legend=c("CDP"), col='black', 
            lwd=c(2,1), cex=0.75) 
          if (kount%%6==0)   AddFooter ()
        }
      }
      if (!is.na(Seq) && (kount >= (Seq*6))) {break}
      if (kount >= 24) {break}
    }
  }
  if (!is.na(Seq) && kount == (Seq-1)*6) {
    print ('no qualifying seconds found for CDP plot')
    plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
    text (0.5, 0.8, labels='no CDP/SP100 measurements')
  }
  # print(proc.time() - ptm)
  # toc()
}

