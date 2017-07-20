
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

server <- function(input, output, session) {
  
  ## RSessions stuff:
  output$S1E1Plot <- renderPlot ({
    V <- input$S1Var
    nm <- c('Temperature [deg. C]', 'Wind Speed [m/s]', 'Pressure [hPa]')
    names (nm) <- c('ATX', 'WSC', 'PSXC')    ## these are the actual variables in the data file
    # Data <- getNetCDF('/Data/DEEPWAVE/DEEPWAVErf20.nc', c('ATX', 'WSC', 'PSXC'))
    load ('~/RStudio/RSessions/RSessions/Session1/Data.Rdata')
    plot (Data$Time, Data[, V], type='l', col='blue', lwd=2, xlab='Time [UTC]', ylab=nm[V])
    title ("DEEPWAVE flight 20")
    # with (Data, plotWAC (data.frame (Time, Data[, V]), ylab=nm[V]))
  })
  
  output$S1E2Plot <- renderPlot ({
    # Directory <- DataDirectory ()    # for portability; sets the local data directory
    # Flight <- "rf20"                 # select a flight
    # Project = "DEEPWAVE"             # select a project
    # fname = sprintf("%s%s/%s%s.nc", Directory,Project,Project,Flight)
    # # XXX set variables needed, here a standard list including DPX and EWX
    # # preliminary look shows that final descent was from 84400 to 91100
    # Data <- getNetCDF (fname, c("Time", "DPXC", "ATX", "PALT"), 84400, 91100)
    saveDataFile <- '~/RStudio/RSessions/RSessions/Session1/Data2.RData'
    # save (Data, file = saveDataFile) 
    # for future runs, it will be much faster to use:
    load(saveDataFile)
    plot (Data$DPXC, Data$PALT, type='l', lwd=1.5, # type='l': line plot
          xlab='Temperature or Dew Point [deg C]', ylab='pressure altitude [m]')   
    lines (Data$ATX, Data$PALT, col='forestgreen', lwd=2) # add temperature
    s <- Data$DPXC > Data$ATX
    lines (Data$DPXC[s], Data$PALT[s], col='red', lwd=3)
    # will show how to add legends, titles, axis labels, etc, later
  })
  
  output$S1Stats <- renderDataTable ({
    Dstats <- data.frame()
    VarList <- c('WIC', 'ATX', 'DPXC', 'PSXC', 'GGALT', 'PALT')
    Ds <- getNetCDF ('/Data/DEEPWAVE/DEEPWAVErf20.nc', VarList)
    ## FL400 means pressure altitude of 40000 ft
    Ds <- Ds[Ds$PALT/0.3048 > 40000, ]  ## select only points above 40000 ft
    save (Ds, file='~/RStudio/RSessions/RSessions/Session1/Data3.Rdata')
    load ('~/RStudio/RSessions/RSessions/Session1/Data3.Rdata')
    Dstats['Time', 1] <- 'Time'
    Dstats['Time', 2] <- NA
    Dstats['Time', 3] <- NA
    Dstats['Time', 4] <- formatTime (Ds$Time[1])
    Dstats['Time', 5] <- formatTime (Ds$Time[nrow(Ds)])
    for (nm in names(Ds)) {
      if (nm == 'Time') {next}
      Dstats[nm, 1] <- nm
      Dstats[nm, 2] <- mean (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 3]   <- sd   (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 4]  <- min  (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 5]  <- max  (Ds[, nm], na.rm=TRUE)
    }
    names(Dstats) <- c('variable', 'mean', 'sd', 'min', 'max')
    row.names (Dstats) <- names(Ds)
    for (k in 2:5) {
      Dstats[2:nrow(Dstats), k] <- sprintf('%.3f', as.numeric(Dstats[2:nrow(Dstats), k]))
    }
    Dstats
  })
  
  output$txtCalc1 <- renderUI({
    y <- NA
    {options(digits=5)
      e <- paste ('y <- round(', input$cformula, ', 6)', sep='')
      try(eval (parse (text=e)), silent=TRUE)
      if (!is.na(y[1])) {
        pre(HTML(y))
      }}
  })
  
  output$txtS2a <- renderUI({
    RT <- input$selS2a
    if (length(RT) == 0 || RT == 1) {
      tx <- paste('Answer will appear here when you select a button below',
                  ' ', ' ', ' ', ' ', ' ', ' ', sep='<br/>')
    }
    a <- 1:12
    if (RT == 2) {tx <- paste('1 2 3 4 5 6 7 8 9 10 11 12', 
                              ' ', 
                              '## the colon operator generates a sequence ', 
                              ' ', ' ', ' ', ' ', sep='<br/>')}
    dim(a) <- c(3,4)
    if (RT == 3) {
      tx <- paste(
        '     [,1] [,2] [,3] [,4] ',
        '[1,]  1    4    7   10 ',
        '[2,]  2    5    8   11 ',
        '[3,]  3    6    9   12 ', 
        ' ', 
        '## c() generates a vector with elements equal to its arguments',
        '## note the column-major order', sep='<br/>')
    }
    if (RT == 4) {
      tx <- paste(
        '     [,1] [,2] [,3]', 
        '[1,]  1    2    3 ',
        '[2,]  4    5    6 ',
        '[3,]  7    8    9 ',
        '[4,] 10   11   12', 
        ' ',
        '## t() is the transpose operator', sep='<br/>')
    }
    pre(HTML(tx))
  })
  
  output$txtS2c1 <- renderUI({
    RT <- input$selS2c1
    if (length(RT) == 0 || RT == 1) {
      tx <- paste('Answer will appear here when you select a button above.',
                  ' ', ' ', ' ', ' ', ' ', ' ', sep='<br/>')
    }
    ## retrieve data.frame Data:
    load('~/RStudio/RSessions/RSessions/Session2/DataS2b.Rdata')
    load('~/RStudio/RSessions/RSessions/Session2/txw.Rdata')
    blkline <- '                           '
    
    # Data$ATX[5]
    if (RT == 2) {tx <- paste(txw[1], txw[2],  
                              ' ', '## 5th row in column ATX',
                              '## "$V" without quotes identifies variable V',
                              ' ', ' ', ' ', ' ',
                              sep='<br/>')}
    # Data[5,2]
    if (RT == 3) {tx <- paste(txw[3], txw[4],
                              ' ', '## ATX is the 2nd column.',
                              '## You can also use multiple indices.',
                              '## (Try Data[5,2:5] or Data[5, c(2,4,6)]',
                              ' ', ' ', ' ',
                              sep='<br/>')}
    # Data[5, ]
    if (RT == 4) {tx <- paste(txw[5], ' ',
                              '## Selects all variables in the 5th row.',
                              '## Defines a single-row data.frame.',
                              '## Do not omit the comma; ',
                              '## Data[5] gives a surprising answer.',
                              '## (try it)', ' ', ' ', sep='<br/>')}
    # Data[5, 'ATX']
    if (RT == 5) {tx <- paste(txw[12], txw[13], ' ',
                              '## The character name of a column also works',
                              '## and you can use a vector of names',
                              '## to select multiple columns',
                              ' ', ' ', ' ', sep='<br/>'
    )}
    # Data$ATX
    if (RT == 6) {tx <- paste(txw[14], txw[15], txw[16], ' ',
                              '## This selects the entire column',
                              '## and returns a vector, not a data.frame.',
                              '## Equivalent to Data[, 2].',
                              ' ', ' ', sep='<br/>')}
    if (RT == 7) {tx <- paste('attach(Data); ATX[5]', txw[2],
                              '## This call makes all the variables in',
                              '## the data.frame available as independent',
                              '## variables. Although it is sometimes very',
                              '## useful, it is dangerous because',
                              '## confusion may arise among same-name',
                              '## variables in different environments.', sep='<br/>'
    )}
    if (RT == 8) {tx <- paste('with(Data, ATX[5]', txw[2],
                              '## This is usually better than "attach"',
                              '## because it isolates the scope to the',
                              '## enclosing ( ). However, assignments in',
                              '## the ( ) must use <<- if you want to',
                              '## use them outside the ( ); try:',
                              '##   with(Data, X <- ATX[5])',
                              '##   print (X)', sep='<br/>' 
    )}
    pre(HTML(tx))
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  
  
  output$ui2 <- renderUI ({
    #     print (sprintf ('entered ui2 with plot %d', input$plot))
    #     print (chp[[input$plot]]);print (slp[[input$plot]])
    if (Trace) {print (sprintf ('entered ui2 with plot %d', input$plot))}
    if (input$addVar != 'add var') {
      chp[[input$plot]] <<- c(chp[[input$plot]],input$addVar)
    }
    PVar <<- slp[[input$plot]]
    updateSelectInput (session, 'addVar', selected='add var')
    selectInput ('PlotVar', label='variables', selectize=FALSE, multiple=TRUE,
                 choices=chp[[input$plot]], selected=PVar, size=10)
  })
  
  ################ OBSERVERS ########################
  
  observe ({                              ## PlotVar
    if (Trace) {
      print ('entered observe for PlotVar')
      print (sprintf ('PVar is %s', PVar))
      print (sprintf ('PlotVar is %s', input$PlotVar))
    }
    if (is.na (any (match(PVar, input$PlotVar)))
        || is.na (any (match(input$PlotVar, PVar)))) {
      reac$newdisplay <- FALSE
    } else {
      reac$newdisplay <- TRUE
    }
    input$plot
    isolate (np <- input$plot)
    if (length (input$PlotVar) < 1) {return()}
    PVar <- input$PlotVar
    if ((length(data ()) < 2) || any (!(PVar %in% names (data ())))) {
      print ('need new data')
      reac$newdata <- TRUE
    }
    jp <- psq[1, np]
    ## need to change VRPlot to have the specified variables
    if (Trace) {
      print (sprintf ('redefined global VRPlot[[%d]]', jp))
      print (PVar)
    }
    # reac$newdisplay <- TRUE
    VRPlot[[jp]] <<- PVar
  }, priority=-5)
  
  observe({                             ## Rplot
    vp <- switch (input$Rplot,
                  'track' = 1,
                  'temperature' = 3,
                  'humidity' = 5,
                  'pressure' = 9,
                  'wind' = 13,
                  'radiation' = 20,
                  'particles' = 21,
                  'skew-T' = 26,
                  'potential T' = 27,
                  'CDP' = 29,
                  'UHSAS/PCASP' = 33,
                  '2DC' = 37,
                  'air chemistry' = 41,
                  'extras' = 43
    )
    updateNumericInput (session, 'plot', value=vp)
  })
  
  observeEvent (input$Tmaneuvers, {
    msg <- 'This takes several minutes and cannot be interrupted.'
    showNotification(msg, action = NULL, duration = 15, closeButton = TRUE,
                     id = 'noticeMan', type = "warning")
    ProjectSeekManeuvers (inp=input)
  })
  
  observeEvent (input$reconfigure, saveConfig ())
  observeEvent (input$savePDF,
                savePDF (Data=data(), inp=input))
  observeEvent (input$savePNG,
                savePNG (Data=data(), inp=input))
  observeEvent (input$saveRdata,
                saveRdata (Data=data(), inp=input))
  observeEvent (input$ncplot, OpenInProgram (data(), warnOverwrite=FALSE))
  observeEvent (input$Xanadu, OpenInProgram (data(), 'Xanadu', warnOverwrite=FALSE))
  observeEvent (input$maneuvers, SeekManeuvers (data ()))
  observeEvent (input$manual, seeManual ())
  
  observe ({
    print (c('observe: ProjectPP is', input$ProjectPP))
    if (!exists('Maneuvers')) {
      load ('Maneuvers.Rdata')
      Maneuvers <<- Maneuvers
    }
    countPM <<- 0
    countYM <<- 1
    countRH <<- 1
    countCR <<- 1
    itemYM <<- 0
    itemRH <<- 0
    PM <<- Maneuvers [Maneuvers$Project == input$ProjectPP & Maneuvers$Type == 'pitch', ]
    YM <<- Maneuvers [Maneuvers$Project == input$ProjectPP & Maneuvers$Type == 'yaw', ]
    RH <<- Maneuvers [Maneuvers$Project == input$ProjectPP & Maneuvers$Type == 'reverse heading', ]
    CR <<- Maneuvers [Maneuvers$Project == input$ProjectPP & Maneuvers$Type == 'circle', ]
    SR <<- Maneuvers [Maneuvers$Project == input$ProjectPP & Maneuvers$Type == 'speed run', ]
    chSR <- vector('character');chPM <<- vector('character');chYM <<- vector('character')
    chRH <- vector('character');chCR <- vector('character')
    if (nrow(SR) > 0) {
      for (i in 1:nrow(SR)) {
        print (s <- sprintf ('%s %d-%d', SR$Flight[i], SR$Start[i], SR$End[i]))
        chSR[i] <- sprintf ('%d', i)
        names(chSR)[i] <- s
      }
      updateRadioButtons(session, inputId='selSR', choices=chSR)
    } else {
      updateRadioButtons(session, inputId='selSR', choices='none')
    }
    if (nrow(PM) > 0) {
      for (i in 1:nrow(PM)) {
        print (s <- sprintf ('%s %d-%d', PM$Flight[i], PM$Start[i], PM$End[i]))
        chPM[i] <- sprintf ('%d', i)
        names(chPM)[i] <- s
      }
      updateRadioButtons(session, inputId='selPM', choices=chPM)
    } else {
      updateRadioButtons(session, inputId='selPM', choices='none')
    }
    print (sprintf ('yaw maneuvers for project %s', input$ProjectPP))
    if (nrow(YM) > 0) {
      for (i in 1:nrow(YM)) {
        print (s <- sprintf ('%s %d-%d', YM$Flight[i], YM$Start[i], YM$End[i]))
        chYM[i] <- sprintf ('%d', i)
        names(chYM)[i] <- s
      }
      updateRadioButtons(session, inputId='selYM', choices=chYM, selected='1')
      # updateSliderInput (session, inputId='sliderYM', min=YM$Start[1], max=YM$End[1])
    } else {
      updateRadioButtons(session, inputId='selYM', choices='none')
    }
    print (sprintf ('circle maneuvers for project %s', input$ProjectPP))
    if (nrow(CR) > 0) {
      for (i in 1:nrow(CR)) {
        print (s <- sprintf ('%s %d-%d', CR$Flight[i], CR$Start[i], CR$End[i]))
        chCR[i] <- sprintf ('%d', i)
        names(chCR)[i] <- s
      }
      updateRadioButtons(session, inputId='selCR', choices=chCR, selected='1')
      # updateSliderInput (session, inputId='sliderCR', min=CR$Start[1], max=CR$End[1])
    } else {
      updateRadioButtons(session, inputId='selCR', choices='none')
    }
    print (sprintf ('reverse heading maneuvers for project %s', input$ProjectPP))
    if (nrow(RH) > 0) {
      for (i in 1:nrow(RH)) {
        print (s <- sprintf ('%s %d-%d', RH$Flight[i], RH$Start[i], RH$Other2[i]))
        chRH[i] <- sprintf ('%d', i)
        names(chRH)[i] <- s
      }
      updateRadioButtons(session, inputId='selRH', choices=chRH, selected='1')
      # updateSliderInput (session, inputId='sliderRH', min=RH$Start[1], max=RH$End[1])
    } else {
      updateRadioButtons(session, inputId='selRH', choices='none')
    }
  }, priority=10)
  
  observe ({
    item <- input$selYM
    if (item != 'none' && !is.na(item)) {
      item <- as.integer(item)
      print (sprintf ('item %s nrow(YM)=%d', input$selYM, nrow(YM)))
      if (is.na(item) || nrow(YM) < 1) {
        itemYM <<- item <- 0
        DYM <<- data.frame()
      }
      ProjDir <- input$ProjectPP
      if (!is.na(item) && item != 'none' && item != 0 && length(item) > 0 && item <= nrow(YM)) {
        itemYM <<- item
        print (c('item, YM', item, YM[item,]))
        if (grepl('HIPPO', ProjDir)) {ProjDir <- 'HIPPO'}
        VL <- c('TASX', 'GGALT', 'SSRD', 'BDIFR', 'QCF', 'WDC', 'WSC', 'THDG', 'VYC',
                'GGVNS', 'GGVEW')
        START <- AddT (as.integer (YM$Start[item]), -120)
        END <- AddT (as.integer (YM$End[item]), 120)
        DYM <<- dataDYM(ProjDir, input$ProjectPP, YM$Flight[item], VL, START, END)
        minT <- DYM$Time[1]; maxT <- DYM$Time[nrow(DYM)]
        # mint <- as.POSIXlt (minT, tz='UTC'); maxT <- as.POSIXlt (maxT, tz='UTC')
        updateSliderInput (session, 'sliderYM', min=minT, max=maxT, value=c(minT, maxT))
        print ( sprintf ('updating YM time slider, limits are %s %s', minT, maxT))
        # print (str(DYM))
      }
    }
  }, priority=0)
  
  observe ({
    item <- input$selRH
    updateSelectInput(session, 'setRHT', selected='leg 1')
    if (item != 'none' && !is.na(item)) {
      item <- as.integer(item)
      print (sprintf ('item %s nrow(RH)=%d', input$selRH, nrow(RH)))
      if (is.na(item) || nrow(RH) < 1) {
        itemRH <<- item <- 0
        DRH <<- data.frame()
      }
      ProjDir <- input$ProjectPP
      if (!is.na(item) && item != 'none' && item != 0 && length(item) > 0 && item <= nrow(RH)) {
        itemRH <<- item
        print (c('item, RH', item, RH[item,]))
        if (grepl('HIPPO', ProjDir)) {ProjDir <- 'HIPPO'}
        VL <- c('LATC', 'LONC', 'TASX', 'GGALT', 'SSLIP', 'BDIFR', 'QCF', 'WDC', 'WSC', 'THDG', 'VYC',
                'GGVNS', 'GGVEW')
        START <- AddT (as.integer (RH$Start[item]), -120)
        END <- AddT (as.integer (RH$Other2[item]), 120)
        DRH <<- dataDRH(ProjDir, input$ProjectPP, RH$Flight[item], VL, START, END)
        minT <- DRH$Time[1]; maxT <- DRH$Time[nrow(DRH)]
        setT1 <- DRH$Time[getIndex(DRH, RH$Start[item])]
        setT2 <- DRH$Time[getIndex(DRH, RH$End[item])]
        # mint <- as.POSIXlt (minT, tz='UTC'); maxT <- as.POSIXlt (maxT, tz='UTC')
        updateSliderInput (session, 'sliderRH', min=minT, max=maxT, value=c(setT1, setT2))
        print ( sprintf ('updating RH time slider, limits are %s %s setting is %s %s', minT, maxT, setT1, setT2))
        # print (str(DRH))
        countRH <<- 1
      }
    }
  }, priority=5)
  
  
  observe ({
    item <- input$selCR
    updateSelectInput(session, 'setCRT', selected='leg 1')
    if (item != 'none' && !is.na(item)) {
      item <- as.integer(item)
      print (sprintf ('item %s nrow(CR)=%d', input$selCR, nrow(CR)))
      if (is.na(item) || nrow(CR) < 1) {
        itemCR <<- item <- 0
        DCR <<- data.frame()
      }
      ProjDir <- input$ProjectPP
      if (!is.na(item) && item != 'none' && item != 0 && length(item) > 0 && item <= nrow(CR)) {
        itemCR <<- item
        print (c('item, CR', item, CR[item,]))
        if (grepl('HIPPO', ProjDir)) {ProjDir <- 'HIPPO'}
        VL <- c('LATC', 'LONC', 'TASX', 'GGALT', 'PITCH', 'ATTACK', 'ROLL', 'SSLIP', 'SSRD', 'BDIFR', 
                'QCF', 'WDC', 'WSC', 'THDG', 'VYC', 'GGVSPD', 'VEW', 'VNS', 'GGVNS', 'GGVEW')
        START <- AddT (as.integer (CR$Start[item]), -120)
        END <- AddT (as.integer (CR$End[item]), 120)
        DCR <<- dataDCR(ProjDir, input$ProjectPP, CR$Flight[item], VL, START, END)
        minT <- DCR$Time[1]; maxT <- DCR$Time[nrow(DCR)]
        setT1 <- DCR$Time[getIndex(DCR, CR$Start[item])]
        setT2 <- DCR$Time[getIndex(DCR, CR$End[item])]
        # mint <- as.POSIXlt (minT, tz='UTC'); maxT <- as.POSIXlt (maxT, tz='UTC')
        updateSliderInput (session, 'sliderCR', min=minT, max=maxT, value=c(setT1, setT2))
        print ( sprintf ('updating CR time slider, limits are %s %s setting is %s %s', minT, maxT, setT1, setT2))
        # print (str(DCR))
        countCR <<- 1
      }
    }
  }, priority=5)

  observe ({                              ## tleg for RH maneuver
    input$setRHT
    if (Trace) {print (sprintf ('entered tleg observer with value %s', input$setRHT))}
    item <- as.integer(isolate(input$selRH))
    if (!is.na(item) && item != 'none' && item != 0 && length(item) > 0 && item <= nrow(RH)) {
      minT <- DRH$Time[1]; maxT <- DRH$Time[nrow(DRH)]
      if (input$setRHT == 'leg 1') {
        setT1 <- DRH$Time[getIndex(DRH, RH$Start[item])]
        setT2 <- DRH$Time[getIndex(DRH, RH$End[item])]
        # mint <- as.POSIXlt (minT, tz='UTC'); maxT <- as.POSIXlt (maxT, tz='UTC')
        updateSliderInput (session, 'sliderRH', min=minT, max=maxT, value=c(setT1, setT2))
      } else {
        setT1 <- DRH$Time[getIndex(DRH, RH$Other1[item])]
        setT2 <- DRH$Time[getIndex(DRH, RH$Other2[item])]
        # mint <- as.POSIXlt (minT, tz='UTC'); maxT <- as.POSIXlt (maxT, tz='UTC')
        updateSliderInput (session, 'sliderRH', min=minT, max=maxT, value=c(setT1, setT2))
      }
    }
  }, priority=4)
    
  observe ({                              ## typeFlight
    if (Trace) {print (sprintf ('entered typeFlight observer with value %s', input$typeFlight))}
    typeFlight <<- input$typeFlight
    reac$newdata <- TRUE
  })
  
  observe ({
    if (input$ProjectPP != ProjectPP) {
      ProjectPP <<- input$ProjectPP
      countPM <- 0
      FlightPP <- input$FlightPP
      FLT <- ifelse (input$AllPP, 1, FlightPP)
      ProjDir <- ProjectPP
      if (grepl('HIPPO', ProjectPP)) {ProjDir <- 'HIPPO'}
      fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), ProjDir, ProjectPP, FLT)
      FI <- DataFileInfo (fnamePP)
      VT <- c(ATVARS[ATVARS %in% FI$Variables])
      updateSelectInput (session, inputId='ATsel', selected='ATX')
      updateSelectInput (session, inputId='ATsc', choices=VT)
    }
  }, priority=5)
  
  observe ({                              ## VRP
    if (Trace) {print (sprintf ('entered VRP, Project=%s %s',
                                input$Project, Project))}
    if (input$Project != Project) {
      Project <<- Project <- input$Project
      if (Trace) {print (sprintf ('set new project: %s', Project))}
      typeFlight <<- flightType ()
      
      if (grepl ('HIPPO', Project)) {
        if (grepl ('raf_data', DataDir)) {
          fn <- sprintf ('%sHIPPO/old_nimbus/%srf01.nc', DataDirectory (), Project)  
        } else {
          fn <- sprintf ('%sHIPPO/%srf01.nc', DataDirectory (), Project)
        }
      } else {
        fn <- sprintf ('%s%s/%srf01.nc', DataDirectory (), Project, Project)
      }
      if (!file.exists (fn)) {
        fn <- sub ('\\.nc', '.Rdata', fn)
      }
      if (!file.exists (fn)) {
        if (grepl ('HIPPO', Project)) {
          if (grepl ('raf_data', DataDir)) {
            fn <- sprintf ('%sHIPPO/%stf01.nc', DataDirectory (), Project)
          } else {
            fn <- sprintf ('%sHIPPO/%stf01.nc', DataDirectory (), Project)
          }
        } else {
          fn <- sprintf ('%s%s/%stf01.nc', DataDirectory (), Project, Project)
        }
      }
      if (!file.exists (fn)) {
        fn <- sub ('\\.nc', '.Rdata', fn)
      }
      if (!file.exists (fn)) {
        warning ('need tf01 or rf01 to initialize')
      } else {
        FI <<- DataFileInfo (fn)
        if (Trace) {print (sprintf ('using file %s for FI', fn))}
      }
      VRPlot <<- loadVRPlot (Project, FALSE, 1, psq)
    }
  }, priority=10)
  
  observe ({                             ## time
    if (Trace) {print ('setting time')}
    if (Trace) {print (sprintf ('Project and Flight: %s %s%02d',
                                isolate(input$Project),
                                isolate (input$typeFlight),
                                isolate (input$Flight)))}
    #     reac$newdisplay
    #     reac$newdisplay <- TRUE
    Data <- data ()
    if (length (Data) < 2) {
      reac$newdata <- TRUE
      if (Trace) {print ('error, need data first')}
      return ()
    }
    step <- 60
    minT <- Data$Time[1]
    minT <- minT - as.integer (minT) %% step
    maxT <- Data$Time[nrow(Data)]
    maxT <- maxT - as.integer (maxT) %% step + step
    if (Trace) {print (sprintf ('slider values %s %s', formatTime (minT),
                                formatTime (maxT)))}
    updateSliderInput(session, inputId='times', label=NULL,
                      value=c(minT, maxT),
                      min=minT, max=maxT)
    times <<- c(minT, maxT)
  }, priority=0)
  
  observeEvent (input$plot_brush, {
    xmin <- as.integer(input$plot_brush$xmin)
    xmax <- as.integer(input$plot_brush$xmax)
    T1 <- as.POSIXlt(xmin, origin='1970-01-01', tz='UTC')
    T2 <- as.POSIXlt(xmax, origin='1970-01-01', tz='UTC')
    TB1 <- T1$hour*10000 + T1$min*100 + T1$sec
    TB2 <- T2$hour*10000 + T2$min*100 + T2$sec
    #   print (sprintf ('brush times are %d %d', TB1, TB2))
    updateSliderInput (session, 'times', value=c(T1, T2))
    times <<- c(T1, T2)
  } )
  
  observeEvent (input$plot2_brush, {
    xmin <- as.integer(input$plot2_brush$xmin)
    xmax <- as.integer(input$plot2_brush$xmax)
    T1 <- as.POSIXlt(xmin, origin='1970-01-01', tz='UTC')
    T2 <- as.POSIXlt(xmax, origin='1970-01-01', tz='UTC')
    TB1 <- T1$hour*10000 + T1$min*100 + T1$sec
    TB2 <- T2$hour*10000 + T2$min*100 + T2$sec
    #   print (sprintf ('brush times are %d %d', TB1, TB2))
    updateSliderInput (session, 'sliderPM', value=c(T1, T2))
    # times <<- c(T1, T2)
  } )
  
  observeEvent (input$plot3_brush, {
    xmin <- as.integer(input$plot3_brush$xmin)
    xmax <- as.integer(input$plot3_brush$xmax)
    T1 <- as.POSIXlt(xmin, origin='1970-01-01', tz='UTC')
    T2 <- as.POSIXlt(xmax, origin='1970-01-01', tz='UTC')
    TB1 <- T1$hour*10000 + T1$min*100 + T1$sec
    TB2 <- T2$hour*10000 + T2$min*100 + T2$sec
    #   print (sprintf ('brush times are %d %d', TB1, TB2))
    updateSliderInput (session, 'sliderYM', value=c(T1, T2))
    # times <<- c(T1, T2)
  } )

  observeEvent (input$plot4_brush, {
    xmin <- as.integer(input$plot4_brush$xmin)
    xmax <- as.integer(input$plot4_brush$xmax)
    ymin <- as.integer(input$plot4_brush$ymin)
    ymax <- as.integer(input$plot4_brush$ymax)
    print (sprintf ('rect limits from brush = %.2f, %.2f. %.2f, %.2f', xmin, xmax, ymin, ymax))
    r <- DRH$xa >= xmin & DRH$xa <= xmax & DRH$ya >= ymin & DRH$ya <= ymax
    item <- isolate(input$selRH)
    if (item != 'none' && !is.na(item)) {
      item <- as.integer (item)
      RH$Start[item] <<- as.integer (gsub(':', '', formatTime (DRH$Time[r1 <- which(r)[1]])))
      RH$End[item] <<- as.integer (gsub(':', '', formatTime (DRH$Time[r2 <- which(!r & DRH$Time > DRH$Time[r1])[1]])))
      RH$Other1[item] <<- as.integer (gsub(':', '', formatTime(DRH$Time[r3 <- which(r & DRH$Time > DRH$Time[r2])[1]])))
      RH$Other2[item] <<- as.integer (gsub(':', '', formatTime(DRH$Time[r4 <- which(!r & DRH$Time > DRH$Time[r3])[1]])))
      print (sprintf ('RH[%d]=%d %d %d %d', item, RH$Start[item], RH$End[item], RH$Other1[item], RH$Other2[item]))
      if (input$setRHT == 'leg 1') {
        updateSliderInput (session, 'sliderRH', value=c(DRH$Time[r1], DRH$Time[r2]))
      } else {
        updateSliderInput (session, 'sliderRH', value=c(DRH$Time[r3], DRH$Time[r4]))
      }
    }
    # T1 <- as.POSIXlt(xmin, origin='1970-01-01', tz='UTC')
    # T2 <- as.POSIXlt(xmax, origin='1970-01-01', tz='UTC')
    # TB1 <- T1$hour*10000 + T1$min*100 + T1$sec
    # TB2 <- T2$hour*10000 + T2$min*100 + T2$sec
    # #   print (sprintf ('brush times are %d %d', TB1, TB2))
    # updateSliderInput (session, 'sliderYM', value=c(T1, T2))
    # times <<- c(T1, T2)
  } )
    
  observeEvent (input$plot5_brush, {
    xmin <- as.integer(input$plot5_brush$xmin)
    xmax <- as.integer(input$plot5_brush$xmax)
    ymin <- as.integer(input$plot5_brush$ymin)
    ymax <- as.integer(input$plot5_brush$ymax)
    print (sprintf ('rect limits from brush = %.2f, %.2f. %.2f, %.2f', xmin, xmax, ymin, ymax))
    r <- DCR$xa >= xmin & DCR$xa <= xmax & DCR$ya >= ymin & DCR$ya <= ymax
    item <- isolate(input$selCR)
    if (item != 'none' && !is.na(item)) {
      item <- as.integer (item)
      CR$Start[item] <<- as.integer (gsub(':', '', formatTime (DCR$Time[r1 <- which(r)[1]])))
      CR$End[item] <<- as.integer (gsub(':', '', formatTime (DCR$Time[r2 <- which(!r & DCR$Time > DCR$Time[r1])[1]])))
      CR$Other1[item] <<- as.integer (gsub(':', '', formatTime(DCR$Time[r3 <- which(r & DCR$Time > DCR$Time[r2])[1]])))
      CR$Other2[item] <<- as.integer (gsub(':', '', formatTime(DCR$Time[r4 <- which(!r & DCR$Time > DCR$Time[r3])[1]])))
      print (sprintf ('CR[%d]=%d %d %d %d', item, CR$Start[item], CR$End[item], CR$Other1[item], CR$Other2[item]))
      if (input$setCRT == 'leg 1') {
        updateSliderInput (session, 'sliderCR', value=c(DCR$Time[r1], DCR$Time[r2]))
      } else {
        updateSliderInput (session, 'sliderCR', value=c(DCR$Time[r3], DCR$Time[r4]))
      }
    }
    # T1 <- as.POSIXlt(xmin, origin='1970-01-01', tz='UTC')
    # T2 <- as.POSIXlt(xmax, origin='1970-01-01', tz='UTC')
    # TB1 <- T1$hour*10000 + T1$min*100 + T1$sec
    # TB2 <- T2$hour*10000 + T2$min*100 + T2$sec
    # #   print (sprintf ('brush times are %d %d', TB1, TB2))
    # updateSliderInput (session, 'sliderYM', value=c(T1, T2))
    # times <<- c(T1, T2)
  } )
  
  observeEvent (input$resetT, {
    step <- 60
    Data <- data ()
    minT <- Data$Time[1]
    minT <- minT - as.integer (minT) %% step + step
    maxT <- Data$Time[nrow(Data)]
    maxT <- maxT - as.integer (maxT) %% step
    times <<- c(minT, maxT)
    updateSliderInput (session, 'times', value=times)
  } )
  
  observeEvent (input$XS2a, {
    showModal(modalDialog(
      includeHTML('~/RStudio/RSessions/RSessions/Session2/Session2c3.html'),
      title = "Solution: One Example",
      size='l',
      easyClose = TRUE
    ))
  })
  
  observeEvent (input$infoPP, {
    showModal(modalDialog(
      includeHTML('PSXC/PSFIT.html'),
      title = "Expected Results",
      size='l',
      easyClose = TRUE
    ))
  })
  
  observeEvent (input$infoIC, {
    showModal(modalDialog(
      includeHTML('inCloud/inCloud.html'),
      title = "Instructions and Expected Results",
      size='l',
      easyClose = TRUE
    ))
  })
  
  observeEvent (input$infoSR, {
    showModal(modalDialog(
      includeHTML('maneuvers/SRMan.html'),
      title = 'Expected Results',
      size = 'l',
      easyClose = TRUE
    ))
  })  
  
  observeEvent (input$infoPM, {
    showModal(modalDialog(
      includeHTML('maneuvers/PitchManeuver.html'),
      title = 'Expected Results',
      size = 'l',
      easyClose = TRUE
    ))
  })
  
  observeEvent (input$infoYM, {
    showModal(modalDialog(
      includeHTML('maneuvers/YawManeuver.html'),
      title = 'Expected Results',
      size = 'l',
      easyClose = TRUE
    ))
  })
  
  observeEvent (input$infoCR, {
    showModal(modalDialog(
      includeHTML('maneuvers/CircleManeuver.html'),
      title = 'Expected Results',
      size = 'l',
      easyClose = TRUE
    ))
  })
  
  observeEvent (input$delCR, {
    print ('entered delete CR')
    iCR <- as.integer(input$selCR)
    ## want match to Maneuvers for Project, Flight,
    print (CR[iCR,])
    idel <- which(Maneuvers$Project == CR$Project[iCR] & Maneuvers$End == CR$End[iCR] & Maneuvers$Type == CR$Type[iCR] & Maneuvers$Flight == CR$Flight[iCR])
    print (sprintf ('deleting maneuver number %d', idel))
    Maneuvers <<- Maneuvers <- Maneuvers[-idel, ]
    save(Maneuvers, file='Maneuvers.Rdata')
    # print(Maneuvers[Maneuvers$Project == ProjectPP & Maneuvers$Type == 'pitch' & Maneuvers$End == chPM[iPM,3],])
  })
  
  observeEvent (input$saveCR, {
    print ('entered saveCR')
    iCR <- as.integer(input$selCR)
    if (Trace) {print (sprintf ('saving new maneuver times for maneuver %d', iCR))}
    ## irev was set in plotCR
    # irev <- which(Maneuvers$Project == CR$Project[iCR] & Maneuvers$End == CR$End[iCR] & Maneuvers$Type == CR$Type[iCR] & Maneuvers$Flight == CR$Flight[iCR])
    Maneuvers$Start[irev] <<- CR$Start[iCR]
    Maneuvers$End[irev] <<- CR$End[iCR]
    Maneuvers$Other1[irev] <<- CR$Other1[iCR]
    Maneuvers$Other2[irev] <<- CR$Other2[iCR]
    print (sprintf('revised maneuvers irev=%d', irev))
    print (Maneuvers[irev, ])
    save(Maneuvers, file='Maneuvers.Rdata')
  })
  
  observeEvent (input$infoRH, {
    showModal(modalDialog(
      includeHTML('maneuvers/RHManeuver.html'),
      title = 'Expected Results',
      size = 'l',
      easyClose = TRUE
    ))
  })
  
  observeEvent (input$delRH, {
    print ('entered delete RH')
    iRH <- as.integer(input$selRH)
    ## want match to Maneuvers for Project, Flight,
    print (RH[iRH,])
    idel <- which(Maneuvers$Project == RH$Project[iRH] & Maneuvers$End == RH$End[iRH] & Maneuvers$Type == RH$Type[iRH] & Maneuvers$Flight == RH$Flight[iRH])
    print (sprintf ('deleting maneuver number %d', idel))
    Maneuvers <<- Maneuvers <- Maneuvers[-idel, ]
    save(Maneuvers, file='Maneuvers.Rdata')
    # print(Maneuvers[Maneuvers$Project == ProjectPP & Maneuvers$Type == 'pitch' & Maneuvers$End == chPM[iPM,3],])
  })
  
  observeEvent (input$saveRH, {
    print ('entered saveRH')
    iRH <- as.integer(input$selRH)
    if (Trace) {print (sprintf ('saving new maneuver times for maneuver %d', iRH))}
    ## irev was set in plotRH
    # irev <- which(Maneuvers$Project == RH$Project[iRH] & Maneuvers$End == RH$End[iRH] & Maneuvers$Type == RH$Type[iRH] & Maneuvers$Flight == RH$Flight[iRH])
    Maneuvers$Start[irev] <<- RH$Start[iRH]
    Maneuvers$End[irev] <<- RH$End[iRH]
    Maneuvers$Other1[irev] <<- RH$Other1[iRH]
    Maneuvers$Other2[irev] <<- RH$Other2[iRH]
    print (sprintf('revised maneuvers irev=%d', irev))
    print (Maneuvers[irev, ])
    save(Maneuvers, file='Maneuvers.Rdata')
  })
  
  observeEvent (input$infoQC, {
    showModal(modalDialog(
      includeHTML('PSXC/QCFIT.html'),
      title = "Expected Results",
      size='m',
      easyClose = TRUE
    ))
  })
  
  observeEvent (input$infoAT, {
    showModal(modalDialog(
      includeHTML('ATemp/ATFIT.html'),
      title = "Expected Results",
      size='m',
      easyClose = TRUE
    ))
  })
  
  observeEvent (input$infoAHE, {
    showModal(modalDialog(
      includeHTML('ATemp/AHE.html'),
      title = "Expected Results",
      size='l',
      easyClose = TRUE
    ))
  })
  
  observeEvent (input$infoPHE, {
    showModal(modalDialog(
      includeHTML('PSXC/PSHeq.html'),
      title = "Expected Results",
      size='l',
      easyClose = TRUE
    ))
  })
  
  observeEvent (input$qcheck, {
    chooseQVar (fname)
    ## check if any requested variables not present in Data:
    if (any (!(quickPlotVar %in% VarList))) {
      VarList <<- unique (c(VarList, quickPlotVar))
      # print (c(VarList, quickPlotVar))
      isolate (reac$newdata <- reac$newdata + 1)
    }
    isolate (reac$quick <- reac$quick + 1)
  })
  
  
  observe ({                          ## global time
    if (Trace) {print ('entering global-time observer')}
    times <<- input$times
  })
  
  ################ REACTIVES ########################
  
  reac <- reactiveValues (newdata=FALSE, newdisplay=FALSE, quick = 0)
  
  flightType <- reactive ({              ## typeFlight
    ## reset typeFlight to rf
    updateRadioButtons (session, 'typeFlight', label=NULL, selected='rf')
    'rf'
  })
  
  data <- reactive({                     ## data
    if (Trace) {print ('entered data')}
    # Project <<- Project <- input$Project
    reac$newdata
    reac$newdata <- FALSE
    VarList <- vector()
    # VRPlot <- VRP ()
    for (i in 1:length(VRPlot)) {
      for (j in 1:length (VRPlot[[i]])) {
        VarList <- c(VarList, VRPlot[[i]][j])
      }
    }
    if (exists ('quickPlotVar')) {
      VarList <- c(VarList, quickPlotVar)
    }
    VarList <- unique (VarList)
    VarList <- VarList[!is.na(VarList)]
    VarList <<- VarList  ## just saving for outside-app use
    ## these would be needed for translation to new cal coefficients
    ## VarList <- c(VarList, "RTH1", "RTH2", "RTF1")
    if (grepl ('HIPPO', input$Project)) {
      if (grepl ('raf_data', DataDir)) {
        fname <<- sprintf ('%sHIPPO/old_nimbus/%s%s%02d.nc', DataDirectory (), input$Project,
                           typeFlight, input$Flight)
      } else {
        fname <<- sprintf ('%sHIPPO/%s%s%02d.nc', DataDirectory (), input$Project,
                           typeFlight, input$Flight)
      }
    } else if (grepl ('PREDICT', input$Project)) {
      fname <<- sprintf ('%sPREDICT/%s%s%02dHW.nc', DataDirectory (), input$Project,
                         typeFlight, input$Flight)
    } else {
      fname <<- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), input$Project,
                         input$Project, typeFlight, input$Flight)
    }
    #     if (input$Production) {
    #       print (sprintf ('Production section, input$Production=%d', input$Production))
    #       dr <- sprintf ('%s../raf/Prod_Data/%s', DataDirectory (), Project)
    #       scmd <- sprintf ('ls -lt `/bin/find %s -ipath "\\./movies" -prune -o -ipath "\\./*image*" -prune -o -name %s%s%02d.nc`',
    #                        dr, Project, input$typeFlight, input$Flight)
    #       fl <- system (scmd, intern=TRUE)[1]
    #       if ((length (fl) > 0) && (!grepl ('total', fl))) {
    #         fname <- sub ('.* /', '/', fl[1])
    #       }
    #       scmd <- sub ('\\.nc', '.Rdata', scmd)
    #       fl <- system (scmd, intern=TRUE)[1]
    #       if ((length (fl) > 0) && (!grepl ('total', fl))) {
    #         fname <- sub ('.* /', '/', fl[1])
    #       }
    #     }
    if (Trace) {print (sprintf ('in data, fname=%s', fname))}
    reac$newdisplay <- TRUE
    if (file.exists(fname)) {
      D <- getNetCDF (fname, VarList)
      if (length (D) > 1) {
        fname.last <<- fname
        return (D)
      } else {
        print (sprintf ('fname=%s', fname))
        print (VarList)
        ## stopping to prevent looping
        stop ('variable not found; stopping to avoid looping')
      }
    } else {
      warning (sprintf ('the file %s does not exist', fname))
      fnRdata <- sub ('\\.nc', '.Rdata', fname)
      if (file.exists (fnRdata)) {
        warning ('using Rdata file instead')
        fl <- load (file=fnRdata)
        FI <<- DataFileInfo (fnRdata)
        loadVRPlot (Project, Production=FALSE, input$Flight, psq)
        fname.last <<- fname
        # print (sprintf ('data returned with dimensions %d', dim(Data)))
        return (Data)
      }
      ## try tf01
      fn <- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), input$Project,
                     input$Project, 'tf', input$Flight)
      if (file.exists (fn)) {
        warning (sprintf ('switched to tf%02d because rf%02d does not exist',
                          input$Flight, input$Flight))
        updateRadioButtons (session, 'typeFlight', label=NULL, selected='tf')
        typeFlight <<- 'tf'
        return (getNetCDF (fn, VarList))
      } else {
        if (Trace) {print ('error in data, returning -1')}
        return (-1)
      }
    }
  })
  
  
  ################ OUTPUTS ########################
  
  output$M1 <- renderText ({
    switch (psq[1, input$plot],
            'Track Plot and z-t',
            'Track Plot and z-t',
            'Temperature history',
            'Temp. scatterplots',
            'Humidity plots',
            'pressure',
            'dynamic p/TAS/M',
            'total pressure',
            'wind',
            'Schuler/comp f.',
            'AKRD/SSRD',
            'IRU comparison',
            'more IRU',
            'radiation',
            'concentrations',
            'dbar/lwc/housek.',
            'skew-T diagram',
            'plot not available',
            'potential T',
            'CDP',
            'UHSAS/PCASP',
            '2DC (1D sizes)',
            'air chemistry',
            'extra2',
            'extra'
    )
  })
  
  output$display <- renderPlot ({  ## display
    # input$typeFlight
    if (is.null(input$times[1])) {
      if (Trace) {print ('in display but input time is NULL')}
      return ()
    }
    if (Trace) {
      print ('display entry, reac$newdisplay is:')
      print (reac$newdisplay)
    }
    if (reac$newdisplay) {
      input$PlotVar
      Project <- input$Project
      # VRPlot <- VRP ()
      if (Trace) {print ('entered display')}
      # VRPlot <<- VRPlot
      Data <- data()
      DataRef <<- Data
      if (length (Data) < 2) {
        warning (sprintf ('variable error in (%s)', fname))
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('requested data file (%s) not found', fname))
        return ()
      }
      
      if (Trace) {
        print (sprintf ('input$times %s %s', formatTime (input$times[1]),
                        formatTime (input$times[2])))
        print (sprintf ('global times are %s %s',
                        formatTime (times[1]), formatTime (times[2])))
      }
      namesV <- names(Data)
      namesV <- namesV[namesV != "Time"]
      for (n in namesV) {
        Data[!is.na(Data[ ,n]) & (abs(Data[,n]+32767) < 1), n] <- NA
      }
      # Data <- Data[(Data$Time > input$times[1]) & (Data$Time < input$times[2]), ]
      Data <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
      if (nrow (Data) <= 0) {
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
        reac$newdisplay <- TRUE
        reac#newdata <- TRUE
        return()
      }
      ## see global.R functions:
      DataV <- limitData (Data, input)
      ndv <- names (DataV)
      ## guard against inf. VCSEL limits
      if (('DP_VXL' %in% ndv) && all(is.na(DataV$DP_VXL))) {
        DataV$DP_VXL <- rep(0, nrow(DataV))
      }
      if (('DP_DPR' %in% ndv) && all(is.na(DataV$DP_DPR))) {
        DataV$DP_DPR <- rep(0, nrow(DataV))
      }
      if (('DP_DPL' %in% ndv) && all(is.na(DataV$DP_DPL))) {
        DataV$DP_DPL <- rep(0, nrow(DataV))
      }
      DataV$DPXC[!is.na(DataV$DPXC) & (DataV$DPXC < -1000)] <- NA
      if (psq[1, input$plot] %in% c(20:22)) {
        t1 <- times[1]    #input$times[1]
        # print (class(t1))
        t <- as.POSIXlt (t1, tz='UTC')
        # print (class(t))
        StartTime <<- as.integer (10000*t$hour+100*t$min+t$sec)
        # print (StartTime)
      }
      #       if (fname != fname.last) {
      #         warning (sprintf ('requested data file (%s) not found', fname))
      #         plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      #         text (0.5, 0.8, sprintf ('requested data file (%s) not found', fname))
      #       } else {
      # if (Trace) {print (str(Data))}
      SE <- getStartEnd (Data$Time)
      i <- getIndex (Data$Time, SE[1])
      FigFooter=sprintf("%s %s%02d %s %s-%s UTC,", Project, input$typeFlight,
                        input$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                        strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),
                        strftime(Data$Time[getIndex(Data$Time,SE[2])],
                                 format="%H:%M:%S", tz='UTC'))
      FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
      AddFooter <<- function() {
        CallingFunction <- sub ("\\(.*\\)", "", deparse (sys.call (-1)))
        mtext(paste(FigFooter,'generated by ', CallingFunction,
                    FigDatestr),1,outer=T,cex=0.75)
      }
      if (input$limits) {
        eval(parse(text=sprintf("RPlot%d(DataV, Seq=%d)",
                                psq[1, input$plot], psq[2, input$plot])))
      } else {
        eval(parse(text=sprintf("RPlot%d(Data, Seq=%d)",
                                psq[1, input$plot], psq[2, input$plot])))
      }
      # }
      #       si <- input$plot
      #       updateSelectInput (session, 'Rplot', selected=st[si])
      if (Trace) {print ('finished display')}
    }
  }, width=920, height=680)
  
  output$stats <- renderDataTable ({
    if (Trace) {print ('entered stats')}
    input$times
    Ds <- limitData (data(), input)
    # Ds <- Ds[, c('Time', slp[[input$plot]])]
    Ds <- Ds[, c('Time', VRPlot[[psq[1, input$plot]]])]
    Ds <- Ds[(Ds$Time >= times[1]) & (Ds$Time <= times[2]), ]
    Dstats <- data.frame ()
    Dstats['Time', 1] <- 'Time'
    Dstats['Time', 2] <- NA
    Dstats['Time', 3] <- NA
    Dstats['Time', 4] <- formatTime (Ds$Time[1])
    Dstats['Time', 5] <- formatTime (Ds$Time[nrow(Ds)])
    for (nm in names(Ds)) {
      if (nm == 'Time') {next}
      Dstats[nm, 1] <- nm
      Dstats[nm, 2] <- mean (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 3]   <- sd   (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 4]  <- min  (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 5]  <- max  (Ds[, nm], na.rm=TRUE)
    }
    names(Dstats) <- c('variable', 'mean', 'sd', 'min', 'max')
    row.names (Dstats) <- names(Ds)
    # Dstats[2:nrow(Dstats), 2:5] <- format(Dstats[2:nrow(Dstats),2:5], digits=5, scientific=FALSE)
    for (k in 2:5) {
      Dstats[2:nrow(Dstats), k] <- sprintf('%.3f', as.numeric(Dstats[2:nrow(Dstats), k]))
    }
    if (Trace) {print (str(Dstats))}
    Dstats
  }, options=list(paging=FALSE, searching=FALSE))
  
  output$hist <- renderPlot ({
    input$PlotVar
    input$times
    layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(8,8,8))
    op <- par (mar=c(5.2,5,1,1)+0.1,oma=c(1.1,0,0,0))
    if (Trace) {print ('entered hist')}
    Ds <- limitData (data(), input)
    # Ds <- Ds[, c('Time', slp[[input$plot]])]
    Ds <- Ds[, c('Time', VRPlot[[psq[1, input$plot]]])]
    Ds <- Ds[(Ds$Time > times[1]) & (Ds$Time < times[2]), ]
    kount <- 0
    for (nm in names (Ds)) {
      if (nm == 'Time') {next}
      kount <- kount + 1
      if (kount > 6) {break}
      hist (Ds[ ,nm], freq=FALSE, breaks=50, xlab=nm, 
            ylab='probability density', main=NA)
    }
  }, width=920, height=680)
  
  output$barWvsZ <- renderPlot ({
    if (Trace) {print ('entered barXvsZ')}
    input$PlotVar
    input$times
    layout (matrix(1:6, ncol=3), widths=c(5,5,5), heights=c(8,8))
    op <- par (mar=c(5.2,5,1,1)+0.1,oma=c(1.1,0,0,0))
    Ds <- limitData (data(), input)
    
    Ds <- Ds[, c('Time', VRPlot[[psq[1, input$plot]]], 'GGALT')]
    Ds <- Ds[(Ds$Time > times[1]) & (Ds$Time < times[2]), ]
    Ds <- Ds[!is.na (Ds$GGALT), ]
    kount <- 0
    for (nm in names (Ds)) {
      if (nm == 'Time') {next}
      if (nm == 'GGALT') {next}
      kount <- kount + 1
      if (kount > 6) {break}
      DB <- data.frame ('Z1'=Ds[, nm])
      DB[Ds$GGALT > 1000, 'Z1'] <- NA
      for (j in 2:15) {
        zmax <- j*1000
        zmin <- zmax-1000
        V <- sprintf ('Z%d', j)
        DB[,V] <- Ds[, nm]
        DB[(Ds$GGALT < zmin) | (Ds$GGALT > zmax), V] <- NA
      }
      boxplot (DB, horizontal=TRUE, outline=TRUE, 
               xlab=nm, ylab='altitude [km]', names=NULL)
    }
  }, width=920, height=680)
  
  output$listing <- renderDataTable ({
    if (Trace) {print ('entered listing')}
    Ds <- limitData (data(), input)
    Ds <- Ds[, c('Time', slp[[input$plot]])]
    Ds <- Ds[(Ds$Time > times[1]) & (Ds$Time < times[2]), ]
    Ds
  }, options=list(paging=TRUE, searching=TRUE))
  
  reac <- reactiveValues (newfit=0, chkm55=0, updatefit=0)
  
  output$fitSummary <- renderPrint({
    reac$newfit
    SF <- readLines ('SummaryFile.txt')
    SF <- gsub ('"', '', SF)
    SF <- gsub ('\\[.\\] ', '', SF)
    print(SF)
  })
  
  output$m55a <- renderPrint ({
    reac$chkm55
    pcterror=abs(input$m55-11.96)/11.96 * 100
    if (pcterror < 0.3) {
      print (sprintf ('That\'s within 0.3%% - excellent!'))
      print ('That\'s about the best expected with this calibration.')
      print ('My answer is 11.96')
    } else if (pcterror < 1) {print (sprintf ('That\'s within 1%% - not bad, but you can do better!'))}
    else if (pcterror < 2) {print (sprintf ('That\'s within 2%% - try again'))}
    else if (pcterror < 5) {print (sprintf ('That\'s within 5%% - way off!'))}
  })
  
  output$chksum <- renderPrint ({
    reac$updatefit
    y <- NA
    M <- CalData$M
    e <- paste ('y <- ', input$fformula, sep='')
    try(eval (parse (text=e)), silent=TRUE)
    if (!is.na(y[1])) {
      sf <- sd (y - CalData$x)
      print (sprintf ('standard deviation is %.3f', sf))
    }
  })
  
  observeEvent (input$checkIt, {
    e <- paste ('y <- ', input$fformula, sep='')
    print (sprintf ('in checkIt, e is %s', e))
    y <- NA
    M <- CalData$M
    try(eval (parse (text=e)), silent=TRUE)
    if (!is.na(y[1])) {
      sf <<- sd (y - CalData$x)
      print (sprintf ('sd=%.3f', sf))
    }
    isolate (reac$updatefit <- reac$updatefit + 1)
  })
  
  observeEvent (input$manual, seeManual ())
  
  output$hrplot <- renderPlot ({
    fm8 <- with(CalData, lm (x ~ MP + I(MP^2) + I(MP^3) + I(MP^4)))
    cf8 <- coef(fm8)
    x2 <- 1:30
    a1 <- 5; a2 <- 3; a3 <- 0.1
    y5a <- a1 + a2 * x2 + a3 * x2^2
    x8 <- cf8[1] + cf8[2]*y5a + cf8[3]*y5a^2 + cf8[4]*y5a^3 + cf8[5]*y5a^4
    plot(x2, y5a, type='l', col='blue', xlab='x', ylab='M')
    points (CalData$x, CalData$MP, pch=19, col='blue')
    lines (x8, y5a, col='red', lwd=2, lty=2)
    legend('bottomright', legend=c('cal points (100-pt ave.)', 'true calibration', '4th-order fit to ave. data'),
           col=c('blue', 'blue', 'red'), lwd=c(NA,1,2), lty=c(NA,1,2),
           pch=c(19,NA,NA), text.col=c('blue', 'blue', 'red'))
  })
  
  output$showfit <- renderPlot({
    plot(CalData$x, CalData$M, type='p', pch=19, col='blue')
    y <- NA
    with (CalData, {
      e <- paste ('y <- ', input$fformula, sep='');
      suppressMessages (suppressWarnings (
        try (eval (parse (text=e)), silent=TRUE))
      );
      if (!is.na(y[1])) {lines (y, M, col='red', lwd=2)}
      # lines (y, z, col='red', lwd=2)
    })
    abline (h=55, col='forestgreen', lty=2)
  })
  output$calibrationPlot <- renderPlot({
    order <- input$fitOrder
    reverse <- input$reverse
    if (reverse) {
      e <- expression ('CalData$M ~ CalData$x')
      if (order > 1) {e <- paste (e, expression (' + I(CalData$x^2)'))}
      if (order > 2) {e <- paste (e, expression (' + I(CalData$x^3)'))}
      if (order > 3) {e <- paste (e, expression (' + I(CalData$x^4)'))}
      if (order > 4) {e <- paste (e, expression (' + I(CalData$x^5)'))}
      if (order > 5) {e <- paste (e, expression (' + I(CalData$x^6)'))}
    } else {
      e <- expression ('CalData$x ~ CalData$M')
      if (order > 1) {e <- paste (e, expression (' + I(CalData$M^2)'))}
      if (order > 2) {e <- paste (e, expression (' + I(CalData$M^3)'))}
      if (order > 3) {e <- paste (e, expression (' + I(CalData$M^4)'))}
      if (order > 4) {e <- paste (e, expression (' + I(CalData$M^5)'))}
      if (order > 5) {e <- paste (e, expression (' + I(CalData$M^6)'))}
    }
    fm <- lm (eval(e))
    SummarizeFit (fm)
    sink (file='SummaryFile.txt')
    SummarizeFit (fm)
    sink (NULL)
    isolate(reac$newfit <- reac$newfit + 1)
    cf <- coef (fm)
    plot(CalData$x, CalData$M, type='p', pch=19, col='blue')
    if (reverse) {
      y <- 0
      z <- (0:500) * (max(CalData$x) - min(CalData$x)) / 500 + min(CalData$x)
      for (i in 1:length(cf)) {y <- y + cf[i]*z^(i-1)}
      lines (z, y, col='red', lwd=2)
    } else {
      y <- 0
      z <- (0:500) * (max(CalData$M) - min(CalData$M)) / 500 + min(CalData$M)
      for (i in 1:length(cf)) {y <- y + cf[i]*z^(i-1)}
      lines (y, z, col='red', lwd=2)
    }
  })
  
  output$fitplot <- renderPlot ({  ## fitplot
    Project <- plotSpec$Project
    Flight <- plotSpec$Flight
    tf <- plotSpec$TypeFlight
    input$response
    input$fformula
    reac$updatefit
    input$times  ## make sensitive to time changes
    op <- par (mar=c(5,6,1,1)+0.1,oma=c(1.1,0,0,0))
    if (Trace) {
      print ('fitplot: entered')
      # Sys.sleep(5)
    }
    Data <- data()
    if (nrow (Data) <= 1) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
      reac$newdata <- reac$newdata + 1
      if (Trace) {print ('fitplot: exiting for new data')}
      return()
    }
    namesV <- names(Data)
    namesV <- namesV[namesV != "Time"]
    DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, input$limitsFit)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    i <- getIndex (DataR$Time, SE[1])
    isolate (
      if (plotSpec$TypeFlight == 'F') {
        FigFooter <<- sprintf("%s rf%02dF %s %s-%s UTC,", Project,
                              plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                              strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                              strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                       format="%H:%M:%S", tz='UTC'))
      } else {
        FigFooter <<- sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                              plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                              strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                              strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                       format="%H:%M:%S", tz='UTC'))
      }
    )
    FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
    AddFooter <<- function() {
      isolate (
        mtext(paste(FigFooter,'generated by Ranadu plot ', input$plot,
                    FigDatestr),1,outer=T,cex=0.75)
      )
    }
    plotWAC (data.frame(x=fitm$y, y=fitm$fitted.values),
             col='blue', type='p',
             xlab=input$response,
             ylab='fit value',
             pch=20, cex=0.8, legend.position=NA)
    V <- DataV[, input$response]
    pts <- c(min(V, na.rm=TRUE), max(V, na.rm=TRUE))
    lines (pts, pts, col='darkorange', lty=2, lwd=2)
  })
  
  observeEvent (input$createV, {
    TX <- input$formla
    m <- gregexpr('[[:alnum:]]+', TX)
    V <- regmatches(TX, m)[[1]]
    V <- V[grepl('[[:upper:]]', V)]
    print (sprintf ('required variables are %s', V))
    ## if a requested variable is not present, get new data:
    nms <- names (data ())
    needAddedVariables <- FALSE
    for (VV in V) {
      if (!(VV %in% nms)) {
        addedVariables <<- c(addedVariables, VV)
        print (sprintf (' need to add variable %s to data', VV))
        print (sprintf (' list of added Variables is:'))
        print (addedVariables)
        # reac$newdata <- reac$newdata + 1
        needAddedVariables <- TRUE
      }
    }
    if (needAddedVariables) {
      reac$newdata <- reac$newdata + 1
    }
    nv <- input$newvar
    assign (nv, with (data (), eval (parse (text=input$formla))))
    isolate (print (summary (eval(parse(text=input$newvar)))))
    if (!exists ('specialData')) {
      specialData <<- data.frame ('Time'=data()$Time)
    }
    specialData[, nv] <<- eval(parse(text=nv))
    FI$Variables <<- c(FI$Variables, nv)
    print (sprintf (' adding %s to FI$Variables', nv))
    isolate (plt <- input$plot)
    isolate (pnl <- input$panel)
    isolate (hpnl <- input$hpanel)
    isolate (spnl <- input$spanel)
    isolate (bpnl <- input$bpanel)
    isolate (lv <- input$lineV)
    isolate (hlv <- input$hlineV)
    isolate (slv <- input$slineV)
    isolate (blv <- input$blineV)
    isolate (rlv <- input$rvNumber)
    choices <- c('select', 'omit',sort(FI$Variables))
    print (sprintf (' setting variable choices to this list:'))
    print (sort(FI$Variables))
    updateSelectInput (session, 'addVarP', choices=choices,
                       selected=plotSpec$Plot[[plt]]$panel[[pnl]]$var[lv])
    updateSelectInput (session, 'haddVarP', choices=choices,
                       selected=plotSpec$Hist[[plt]]$panel[[hpnl]]$var[hlv])
    updateSelectInput (session, 'saddVarP1', choices=choices,
                       selected=plotSpec$Scat[[plt]]$panel[[spnl]]$varx)
    updateSelectInput (session, 'saddVarP2', choices=choices,
                       selected=plotSpec$Scat[[plt]]$panel[[spnl]]$vary[slv])
    updateSelectInput (session, 'baddVarP1', choices=choices,
                       selected=plotSpec$Bin[[plt]]$panel[[bpnl]]$varx)
    updateSelectInput (session, 'baddVarP2', choices=choices,
                       selected=plotSpec$Bin[[plt]]$panel[[bpnl]]$vary[blv])
    updateSelectInput (session, 'specvar', choices=choices, selected=plotSpec$Variance[[1]]$Definition$var)
    updateSelectInput (session, 'speccovar', choices=choices, selected=plotSpec$Variance[[1]]$Definition$cvar)
    updateSelectInput (session, 'rvar', choices=choices,
                       selected=plotSpec$Restrictions$RVAR[rlv])
    VF <- isolate (input$response)
    updateSelectInput (session, 'response', choices=sort(VarList), selected=VF)
    ## force re-read to get this variable added to data:
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {
      print ('createV: reset newdata')
      print ('createV: str(specialData is:')
      print (str(specialData))
    }
  })
  
  observeEvent (input$Run, {
    runScript(session)
    progress$set(message = 'processing is complete', detail=sprintf ('Flight %d', Flight), value=99)
    updateNumericInput(session, 'viewPlot', value=3)
  })
  
  exprProjectKF <- quote ({
    if (input$ProjectKF != ProjectKF) {
      ProjectKF <<- input$ProjectKF
    }
  })
  obsProjectKF <- observe (exprProjectKF, quoted=TRUE)
  
  exprFlightKF <- quote ({
    if (input$FlightKF != FlightKF) {
      FlightKF <<- input$FlightKF
      progress$set(message = 'ready to run', detail = sprintf('flight %d', FlightKF))
    }
  })
  obsFlightKF <- observe (exprFlightKF, quoted=TRUE)
  
  exprNSTEP <- quote ({
    if (input$NSTEP != NSTEP) {
      NSTEP <<- input$NSTEP
    }
  })
  obsNSTEP <- observe (exprNSTEP, quoted=TRUE)
  
  exprNEXT <- quote ({
    if (input$NEXT != NEXT) {
      NEXT <<- input$NEXT
      if (NEXT) {Flight <- 'NEXT'}
    }
  })
  obsNEXT <- observe (exprNEXT, quoted=TRUE)
  
  exprALL <- quote ({
    if (input$ALL != ALL) {
      ALL <<- input$ALL
      if (ALL) {
        js_string <- 'alert("For ALL, see instructions. Previously generated files are skipped; delete to reprocess. This run may take a very long time.")'
        session$sendCustomMessage(type='jsCode', list(value = js_string))
      }
    }
  })
  obsALL <- observe (exprALL, quoted=TRUE)
  
  exprNewAK <- quote ({
    if (input$newAK != newAK) {
      newAK <<- input$newAK
    }
  })
  obsNewAK <- observe (exprNewAK, quoted=TRUE)
  
  exprNewSS <- quote ({
    if (input$newSS != newSS) {
      newSS <<- input$newSS
    }
  })
  obsNewSS <- observe (exprNewSS, quoted=TRUE)
  
  exprSimple <- quote ({
    if (input$simple != simple) {
      simple <<- input$simple
    }
  })
  obsSimple <- observe (exprSimple, quoted=TRUE)
  
  exprGenPlot <- quote ({
    if (input$genPlot != genPlot) {
      genPlot <<- input$genPlot
    }
  })
  obsGenPlot <- observe (exprGenPlot, quoted=TRUE)## get the wind variables:
  
  exprViewPlot <- quote ({
    if (input$viewPlot != viewPlot) {
      viewPlot <<- input$viewPlot
    }
  })
  obsViewPlot <- observe (exprViewPlot, quoted=TRUE)
  output$runPar <- renderText({
    if (!progressExists) {
      progress <- Progress$new(session, min=0, max=100)
    }
    # on.exit(progress$close())
    
    progress$set(message = 'ready to run')
    progress$set (value=1)
    progress <<- progress
    progressExists <<- TRUE
    messg <<- sprintf ("%srf%02d.nc dt=%d AK=%s SS=%s Simple=%s",
                       input$Project, input$Flight, input$NSTEP, input$newAK, input$newSS, input$simple)
    messg <- NULL
  })
  
  output$resultPlot <- renderImage({
    
    plotNo <- input$viewPlot
    pname <- c('../KalmanFilter/KFplots/Position.png',
               '../KalmanFilter/KFplots/Velocity.png',
               '../KalmanFilter/KFplots/AAlframe.png',
               '../KalmanFilter/KFplots/AAaframe.png',
               '../KalmanFilter/KFplots/HDG.png',
               '../KalmanFilter/KFplots/Wind.png',
               '../KalmanFilter/KFplots/Wind2.png',
               '../KalmanFilter/KFplots/HCPlot.png')
    # Return a list containing the filename
    list(src = pname[plotNo],
         contentType = 'image/png',
         width = 900,
         height = 600,
         alt = "Waiting for Plots")
  }, deleteFile=FALSE)
  
  
  output$resolutionPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2] 
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    
    D <- input$separation
    CL <- input$conf
    x1<-dnorm(xp+D/2); x2 <- dnorm(xp-D/2)
    y <- dnorm(xp+D/2, sd=sqrt(2))
    plotWAC(data.frame(xp, x1, x2, y), xlim=c(-5, 5), xlab=expression(paste('x [units ', sigma[x], ']')),
            ylab='Gaussian probability distributions', legend.position='topleft')
    x2t <- x2
    x2t[(xp > -D/2-sqrt(2)*CL) & (xp < sqrt(2)*CL-D/2)] <- NA
    lines(xp, x2t, col='darkgreen', lwd=2.5)
    text (4, 0.35, labels=sprintf('d=%.1f', D))
    text (4, 0.30, labels=sprintf('P=%.3f', 1-pnorm(-D+CL*sqrt(2))+pnorm(-D-CL*sqrt(2))))
    text (4, 0.25, labels=sprintf('P2=%.3f',1-pnorm(CL*sqrt(2), D, sqrt(2))+pnorm(-sqrt(2)*CL,D,sqrt(2))))
    points (c(-D/2+sqrt(2)*CL, -D/2, -D/2-sqrt(2)*CL), rep(dnorm(sqrt(2)*CL, sd=sqrt(2)), 3), pch=20, col='red')
    # points (-D/2-sqrt(2), dnorm(-sqrt(2), sd=sqrt(2)), pch=20, col='red')
    
    arrows (c(-D/2-sqrt(2), -D/2), dnorm(-sqrt(2), sd=sqrt(2)), c(-D/2, -D/2+sqrt(2)), dnorm(-sqrt(2), sd=sqrt(2)),
            length=.2, code=3, col='red', lty=4)
    lines(c(-D/2+sqrt(2)*CL, -D/2+sqrt(2)*CL), c(-1,1), col='red', lty=2)
    lines(c(-D/2-sqrt(2)*CL, -D/2-sqrt(2)*CL), c(-1,1), col='red', lty=2)
    points(-D/2+sqrt(2)*CL, dnorm(sqrt(2)*CL-D), pch=20, col='darkgreen')
    points(-D/2-sqrt(2)*CL, dnorm(-D-sqrt(2)*CL), pch=20, col='darkgreen')
    if (D > 0) {
      arrows(-D/2, 0.04, D/2, 0.04, length=min(D/2, 0.2), code=3, col='black', lty=4)
      text(0, 0.055, labels='d')
    }
    y <- DX + D
    E <- (ecdf(y)(ddd))
    j <- which (ddd > 1.41)[1]
    P <- 1 - E[j]
    text (6, 0.75, labels=sprintf('D=%.1f', D))
    text (6, 0.70, labels=sprintf('P=%.3f', P))
    text (-0.7-D/2, 0.19, labels=expression(sqrt(2)), col='red')
    abline(v=-D/2, col='blue', lty=2)
    abline (v=D/2, col='darkgreen', lty=2)
  })
  output$distBins <- renderPlot({
    plotWAC(xpp, ypp, xlab='Time', ylab='y', lwd=3)
    n <- input$bits
    m <- 2^n-1
    ypn <- round(ypp*m)/m
    lines(xpp, ypn, col='red', lwd=1.5)
    legend('topleft', legend=c('measurand', sprintf('%d-bit digitized', n)),
           lwd=c(3,1.5), col=c('blue', 'red'))
  })
  
  output$PSplot <- renderPlot ({
    ProjectPPDir <- input$ProjectPP
    if (grepl('HIPPO', ProjectPPDir)) {ProjectPPDir <- 'HIPPO'}
    fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), ProjectPPDir, input$ProjectPP, input$FlightPP)
    VL <- c('PSXC', 'PS_A', 'QCXC', 'QC_A', 'TASX', 'GGALT', 'ADIFR', 'QCF', 'ROLL')
    # print (sprintf('entered PSplot, inputs %s %s %s', input$ProjectPP, input$FlightPP, input$AllPP))
    
    RdataFile <- sprintf ('Data/dataPQ%s.Rdata', input$ProjectPP)
    if (file.exists (RdataFile)) {
      load (RdataFile)
    } else {
      DataPP <- makeDataFile (input$ProjectPP, 'ALL', VL)
      DataPP <- qualifyData (DataPP)
      save (DataPP, file=RdataFile)
    }
    if (input$AllPP) {
      Data <- DataPP
    } else {
      Data <- DataPP[DataPP$RF == input$FlightPP, ]
    }
    
    # if (input$AllPP) {
    #   ## loop through all the flights in this project:
    #   Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjectPPDir),
    #                           sprintf ("%srf...nc$", input$ProjectPP)))
    #   if (!is.na (Fl[1])) {
    #     Data <- data.frame()
    #     for (Flt in Fl) {
    #       FltPP <- sub('.*rf', '', sub ('.nc$', '', Flt))
    #       FltPP <- as.integer (FltPP)
    #       fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
    #                           ProjectPPDir, input$ProjectPP, FltPP)
    #       Data <- rbind (Data, qualifyPS(fnamePP, VL, FltPP))
    #     }
    #   }
    #   fnamePP <<- 'All'
    # } else {
    #   fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
    #                       ProjectPPDir, input$ProjectPP, input$FlightPP)
    #   Data <- qualifyPS (fnamePP, VL, input$FlightPP)
    #   fnamePP <<- fnamePP
    # }
    cf <- c(-2.2717351e+00, 1.0044060e+00, 1.7229198e-02, -3.1450368e-06) # ORCAS only
    cf <- c(-2.6239872e+00, 1.0063093e+00, 1.6020764e-02, -4.6657542e-06)  #CSET+ORCAS+DEEPWAVE
    Data$PSFIT <- with(Data, cf[1] + PS_A * (cf[2] + cf[4] * PS_A) + cf[3] * QC_A)
    DataPP <<- Data
    # with(Data, plotWAC(data.frame(Time, PSXC-PS_A), ylim=c(-2,2), ylab='PSXC-PS_A'))
    M <- with(Data,
              sprintf('mean and std dev: %.2f +/- %.2f hPa', 
                      mean(PSXC-PSFIT, na.rm=TRUE), sd(PSXC-PSFIT, na.rm=TRUE)))
    b <- ceiling(with(Data, (max(PSXC-PSFIT, na.rm=TRUE)-min(PSXC-PSFIT, na.rm=TRUE))*20))
    with(Data, hist (PSXC-PSFIT, breaks=b, xlim=c(-2,2), xlab='PSXC-PSFIT [hPa]',
                     freq=FALSE, main=M))
    # abline(h=0, lty=2)
    # abline(h=1, lty=2, col='forestgreen')
    # abline(h=-1, lty=2, col='forestgreen')
  })
  
  output$PSSplot <- renderPlot ({
    ProjectPPDir <- input$ProjectPP
    if (grepl('HIPPO', ProjectPPDir)) {ProjectPPDir <- 'HIPPO'}
    fname <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), input$ProjectPP, input$ProjectPP, input$FlightPP)
    VL <- c('PSXC', 'PS_A', 'QCXC', 'QC_A', 'TASX', 'GGALT', 'ADIFR', 'QCF', 'ROLL')
    
    RdataFile <- sprintf ('Data/dataPQ%s.Rdata', input$ProjectPP)
    if (file.exists (RdataFile)) {
      load (RdataFile)
    } else {
      DataPP <- makeDataFile (input$ProjectPP, 'ALL', VL)
      DataPP <- qualifyData (DataPP)
      save (DataPP, file=RdataFile)
    }
    if (input$AllPP) {
      Data <- DataPP
    } else {
      Data <- DataPP[DataPP$RF == input$FlightPP, ]
    }
    
    cf <- c(-2.2717351e+00, 1.0044060e+00, 1.7229198e-02, -3.1450368e-06) #CSET only
    cf <- c(-2.6239872e+00, 1.0063093e+00, 1.6020764e-02, -4.6657542e-06)  #CSET+ORCAS+DEEPWAVE
    Data$PSFIT <- with(Data, cf[1] + PS_A * (cf[2] + cf[4] * PS_A) + cf[3] * QC_A)
    bs <- with(Data, binStats(data.frame(PSXC-PSFIT, PSXC), bins=10))
    g <- ggplot(data=bs)
    g <- g + geom_errorbarh (aes (y=xc, x=ybar, xmin=ybar-sigma, 
                                  xmax=ybar+sigma), na.rm=TRUE) 
    xlow <- floor(min (bs$ybar-bs$sigma, na.rm=TRUE))
    xhigh <- ceiling(max (bs$ybar+bs$sigma, na.rm=TRUE))
    if (xhigh < 2) {xhigh <- 2}
    if (xlow > -2) {xlow <- -2}
    g <- g + xlim(xlow, xhigh) + theme_WAC()
    g <- g + xlab('PSXC-PSFIT [hPa]') + ylab('PSXC [hPa]') + ylim(1050, 100) 
    g <- g + geom_point (aes (x=bs$ybar, y=bs$xc), size=3, colour='blue', na.rm=TRUE)
    g <- g + geom_label (aes (x=1.9, y=bs$xc, label=sprintf('%d', bs$nb)))
    print (g)
    
    # with(DataPP, plot(PSXC-PSFIT, PSXC, xlab='PSXC-PSFIT [hPa]', ylab='PSXC [hPa]',
    #                 type='p', pch=20, col='blue', xlim=c(-2,2)))
  })
  
  output$QCplot <- renderPlot ({
    ProjectPPDir <- input$ProjectPP
    if (grepl('HIPPO', ProjectPPDir)) {ProjectPPDir <- 'HIPPO'}
    fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), ProjectPPDir, input$ProjectPP, input$FlightPP)
    VL <- c('PSXC', 'PS_A', 'QCXC', 'QC_A', 'TASX', 'GGALT', 'ADIFR', 'QCF', 'ROLL')
    RdataFile <- sprintf ('Data/dataPQ%s.Rdata', input$ProjectPP)
    if (file.exists (RdataFile)) {
      load (RdataFile)
    } else {
      DataPP <- makeDataFile (input$ProjectPP, 'ALL', VL)
      DataPP <- qualifyData (DataPP)
      save (DataPP, file=RdataFile)
    }
    if (input$AllPP) {
      Data <- DataPP
    } else {
      Data <- DataPP[DataPP$RF == input$FlightPP, ]
    }
    
    cfq <- c(2.7809637e+00, 9.7968460e-01, -6.7437126e-03, 4.8584555e-06)
    Data$QCFIT <- with(Data, cfq[1] + PS_A * (cfq[3] + cfq[4] * PS_A) + cfq[2] * QC_A)
    M <- with(Data,
              sprintf('mean and std dev: %.2f +/- %.2f hPa', 
                      mean(QCXC-QCFIT, na.rm=TRUE), sd(QCXC-QCFIT, na.rm=TRUE)))
    b <- ceiling(with(Data, (max(QCXC-QCFIT, na.rm=TRUE)-min(QCXC-QCFIT, na.rm=TRUE))*20))
    with(Data, hist (QCXC-QCFIT, breaks=b, xlim=c(-2,2), xlab='QCXC-QCFIT [hPa]',
                     freq=FALSE, main=M))
  })
  
  output$QCSplot <- renderPlot ({
    ProjectPPDir <- input$ProjectPP
    if (grepl('HIPPO', ProjectPPDir)) {ProjectPPDir <- 'HIPPO'}
    fname <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), input$ProjectPP, input$ProjectPP, input$FlightPP)
    VL <- c('PSXC', 'PS_A', 'QCXC', 'QC_A', 'TASX', 'GGALT', 'ADIFR', 'QCF', 'ROLL')
    RdataFile <- sprintf ('Data/dataPQ%s.Rdata', input$ProjectPP)
    if (file.exists (RdataFile)) {
      load (RdataFile)
    } else {
      DataPP <- makeDataFile (input$ProjectPP, 'ALL', VL)
      DataPP <- qualifyData (DataPP)
      save (DataPP, file=RdataFile)
    }
    if (input$AllPP) {
      Data <- DataPP
    } else {
      Data <- DataPP[DataPP$RF == input$FlightPP, ]
    }
    
    cfq <- c(2.7809637e+00, 9.7968460e-01, -6.7437126e-03, 4.8584555e-06)
    Data$QCFIT <- with(Data, cfq[1] + PS_A * (cfq[3] + cfq[4] * PS_A) + cfq[2] * QC_A)
    bs <- with(Data, binStats(data.frame(QCXC-QCFIT, QCXC), bins=10))
    g <- ggplot(data=bs)
    g <- g + geom_errorbarh (aes (y=xc, x=ybar, xmin=ybar-sigma, 
                                  xmax=ybar+sigma), na.rm=TRUE) 
    xlow <- floor(min (bs$ybar-bs$sigma, na.rm=TRUE))
    xhigh <- ceiling(max (bs$ybar+bs$sigma, na.rm=TRUE))
    if (xhigh < 2) {xhigh <- 2}
    if (xlow > -2) {xlow <- -2}
    g <- g + xlim(xlow, xhigh) + theme_WAC()
    g <- g + xlab('QCXC-QCFIT [hPa]') + ylab('QCXC [hPa]') + ylim(200, 50) 
    g <- g + geom_point (aes (x=bs$ybar, y=bs$xc), size=3, colour='blue', na.rm=TRUE)
    g <- g + geom_label (aes (x=1.9, y=bs$xc, label=sprintf('%d', bs$nb)))
    print (g)
  })
  
  output$PSHeq <- renderPlot ({
    ProjectPPDir <- input$ProjectPP
    if (grepl('HIPPO', ProjectPPDir)) {ProjectPPDir <- 'HIPPO'}
    fname <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), input$ProjectPP, input$ProjectPP, input$FlightPP)
    if (checkBad(sprintf ('%srf%02d', input$ProjectPP, input$FlightPP))) {
      print ('bad flight -- skipping')
      return()
    }
    Data <- getNetCDF(fname)  ## standard variables OK
    Data$ATX <- ShiftInTime(Data$ATX, .shift=-2300)
    Data$GGALT <- ShiftInTime(Data$GGALT, .shift=500)
    re <- is.na(Data$EWX)
    Data$EWX[re] <- 0.5*MurphyKoop(Data$ATX[re], Data$PSXC[re])
    Data$Grav <- Gravity(Data$LATC, Data$GGALT)
    Data$DZ <- c(0, diff(Data$GGALT))
    Data$DGZ <- SmoothInterp (Data$DZ, .Length=121)
    Data$DGZ2 <- SmoothInterp (Data$DZ, .Length=1801)
    i1 <- which(!is.na(Data$Grav) & !is.na(Data$PSXC))[1]
    i1 <- max(i1, which(Data$TASX > 95 & Data$DGZ > 1)[1])
    Data <- Data[i1:nrow(Data), ]
    #i1 <- max (i1, which (Data$PSXC < 900)[1])
    # i2 <- which (Data$GGVSPD < 3 & Data$PSXC < 550)[1]
    # i2 <- which (Data$PSXC < 200)[1]
    # i2 <- which (Data$GGALT == max(Data$GGALT, na.rm=TRUE))
    i2 <- which (Data$DGZ2 < 0)[1]
    # i2 <- which (Data$DGZ < 1)[1]
    Data <- Data[1:i2, ]
    ## check for sequential times. If any are missing, fill in:
    NR <- nrow (Data)
    for (i in 2:NR) {
      j <- as.numeric (Data$Time[i]-Data$Time[i-1])
      if (j != 1) {
        print (c(sprintf('gap i=%d', i),Data$Time[i]))
        DFill <- data.frame(Data[i-1, ])
        DFill[2:ncol(DFill)] <- NA
        DTail <- data.frame()
        for (jj in 1:(j-1)) {
          DFill$Time <- DFill$Time + 1
          DTail <- rbind(DTail, DFill)
        }
        DTail <- rbind(DTail, Data[i:nrow(Data),])
        Data <- rbind(Data[1:(i-1), ], DTail)
      }
    }
    Data$Ra <- SpecificHeats(Data$EWX / Data$PSXC)[,3]
    # Data <- Data[!is.na(Data$Ra), ]
    Data$GbyR <- Data$Grav / Data$Ra
    X <- Data$GbyR * Data$DZ / (273.15 + Data$ATX)
    XL <- Data$GbyR * Data$DZ / (273.15 + Data$ATX - 0.5)
    XH <- Data$GbyR * Data$DZ / (273.15 + Data$ATX + 0.5)
    X <- zoo::na.approx (as.vector(X), maxgap=1000, na.rm=FALSE)
    XL <- zoo::na.approx (as.vector(XL), maxgap=1000, na.rm=FALSE)
    XH <- zoo::na.approx (as.vector(XH), maxgap=1000, na.rm=FALSE)
    X[is.na(X)] <- 0; XL[is.na(XL)] <- 0; XH[is.na(XH)] <- 0
    DLP <- cumsum(X); DLPL <- cumsum(XL); DLPH <- cumsum(XH)
    Data$PN <- Data$PSXC[1]*exp(-DLP)
    Data$PNL <- Data$PSXC[1]*exp(-DLPL)
    Data$PNH <- Data$PSXC[1]*exp(-DLPH)
    # plot(Data$PSXC, Data$PN)
    # print (summary(lm(PN ~ PSXC, data=Data)))
    Data$DPN <- Data$PSXC - Data$PN
    Data$DPNL <- Data$PSXC - Data$PNL
    Data$DPNH <- Data$PSXC - Data$PNH
    with(Data, plotWAC(data.frame(DPN, GGALT), xlab='PSXC-PSH [hPa]', 
                       xlim=c(-4,4), ylab='GGALT [m]'))
    with (Data, lines (DPNL, GGALT, col='red', lty=2))
    with (Data, lines (DPNH, GGALT, col='red', lty=2))
  })
  
  output$ATplot <- renderPlot ({
    ProjectPPDir <- input$ProjectPP
    if (grepl('HIPPO', ProjectPPDir)) {ProjectPPDir <- 'HIPPO'}
    fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), ProjectPPDir, input$ProjectPP, input$FlightPP)
    VL <- standardVariables (c('AT_A', 'PS_A', 'QC_A', 'TAS_A', 'GGALT', 'ROLL'))
    RdataFile <- sprintf ('Data/dataATA%s.Rdata', input$ProjectPP)
    if (file.exists (RdataFile)) {
      load (RdataFile)
    } else {
      DataPP <- makeDataFile (input$ProjectPP, 'ALL', VL)
      DataPP <- qualifyData (DataPP)
      save (DataPP, file=RdataFile)
    }
    if (input$AllPP) {
      Data <- DataPP
    } else {
      Data <- DataPP[DataPP$RF == input$FlightPP, ]
    }
    
    cfA <- c(0.28178716560, 1.01636832046, -0.00012560891)  ## const, AT_A, AT_A^2
    Data$ATFIT <- with(Data, cfA[1] + AT_A * (cfA[2] + cfA[3] * AT_A))
    DataPP <<- Data
    # with(Data, plotWAC(data.frame(Time, PSXC-PS_A), ylim=c(-2,2), ylab='PSXC-PS_A'))
    M <- with(Data,
              sprintf('mean and std dev: %.2f +/- %.2f deg. C', 
                      mean(ATX-ATFIT, na.rm=TRUE), sd(ATX-ATFIT, na.rm=TRUE)))
    b <- ceiling(with(Data, (max(ATX-ATFIT, na.rm=TRUE)-min(ATX-ATFIT, na.rm=TRUE))*20))
    with(Data, hist (ATX-ATFIT, breaks=b, xlim=c(-2,2), xlab='ATX-ATFIT [deg. C]',
                     freq=FALSE, main=M))
    # abline(h=0, lty=2)
    # abline(h=1, lty=2, col='forestgreen')
    # abline(h=-1, lty=2, col='forestgreen')
  })
  
  output$ATSplot <- renderPlot ({
    ProjectPPDir <- input$ProjectPP
    if (grepl('HIPPO', ProjectPPDir)) {ProjectPPDir <- 'HIPPO'}
    fname <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), input$ProjectPP, input$ProjectPP, input$FlightPP)
    VL <- standardVariables (c('AT_A', 'PS_A', 'QC_A', 'TAS_A', 'GGALT', 'ROLL'))
    RdataFile <- sprintf ('Data/dataATA%s.Rdata', input$ProjectPP)
    if (file.exists (RdataFile)) {
      load (RdataFile)
    } else {
      DataPP <- makeDataFile (input$ProjectPP, 'ALL', VL)
      DataPP <- qualifyData (DataPP)
      save (DataPP, file=RdataFile)
    }
    if (input$AllPP) {
      Data <- DataPP
    } else {
      Data <- DataPP[DataPP$RF == input$FlightPP, ]
    }
    
    cfA <- c(0.28178716560, 1.01636832046, -0.00012560891)  ## const, AT_A, AT_A^2
    Data$ATFIT <- with(Data, cfA[1] + AT_A * (cfA[2] + cfA[3] * AT_A))
    bs <- with(Data, binStats(data.frame(ATX-ATFIT, PSXC), bins=10))
    g <- ggplot(data=bs)
    g <- g + geom_errorbarh (aes (y=xc, x=ybar, xmin=ybar-sigma, 
                                  xmax=ybar+sigma), na.rm=TRUE) 
    xlow <- floor(min (bs$ybar-bs$sigma, na.rm=TRUE))
    xhigh <- ceiling(max (bs$ybar+bs$sigma, na.rm=TRUE))
    if (xhigh < 2) {xhigh <- 2}
    if (xlow > -2) {xlow <- -2}
    g <- g + xlim(xlow, xhigh) + theme_WAC()
    g <- g + xlab('ATX-ATFIT [deg. C]') + ylab('PSXC [hPa]') + ylim(1050, 80) 
    g <- g + geom_point (aes (x=bs$ybar, y=bs$xc), size=3, colour='blue', na.rm=TRUE)
    g <- g + geom_label (aes (x=1.9, y=bs$xc, label=sprintf('%d', bs$nb)))
    print (g)
  })
  
  output$ATHeq <- renderPlot ({
    ProjectPPDir <- input$ProjectPP
    ATsel <- input$ATsel
    if (grepl('HIPPO', ProjectPPDir)) {ProjectPPDir <- 'HIPPO'}
    fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), ProjectPPDir, 
                        input$ProjectPP, input$FlightPP)
    # VL <- standardVariables (c('PS_A', 'QC_A', 'TAS_A',
    #                            'RTX', 'PALT', 'ROLL', 'QCF', 'QCR'))
    VL <- c('ATX', 'EWX', 'GGALT', 'LATC', 'MACHX', 'PALT', 'PSXC',
            'QCF', 'QCR', 'ROLL', 'TASX')
    ## add temperatures:
    TCN <- c('ATH1', 'ATH2', 'ATH3', 'ATH4', 'ATF1', 'ATF2', 'AT_A', 'AT_A2', 
             'ATFH1', 'ATFH2', 'ATHR1', 'ATHR2')
    FI <- DataFileInfo (fnamePP)
    
    VL <- c(VL, TCN[which(TCN %in% FI$Variables)])
    
    RdataFile <- sprintf ('Data/dataATHeq%s.Rdata', input$ProjectPP)
    if (file.exists (RdataFile)) {
      load (RdataFile)
    } else {
      DataPP <- makeDataFile (input$ProjectPP, 'ALL', VL)
      save (DataPP, file=RdataFile)
    }
    ch <- c('ATX', sort (TCN[which(TCN %in% names(DataPP))]))
    updateSelectInput (session, 'ATsel', label=NULL, 
                       choices=ch, selected=ATsel)
    
    # if (input$AllPP) {
    #   if (fnamePPS == sprintf('ALL%s', input$ProjectPP)) {
    #     Data <- DataPP
    #   } else {
    #     ## loop through all the flights in this project:
    #     Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjectPPDir),
    #                             sprintf ("%srf...nc$", input$ProjectPP)))
    #     if (!is.na (Fl[1])) {
    #       Data <- data.frame()
    #       for (Flt in Fl) {
    #         FltPP <- sub('.*rf', '', sub ('.nc$', '', Flt))
    #         FltPP <- as.integer (FltPP)
    #         fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
    #                             ProjectPPDir, input$ProjectPP, FltPP)
    #         if (checkBad(sprintf ('%srf%02d', input$ProjectPP, FltPP))) {
    #           print ('bad flight -- skipping')
    #           next
    #         }
    #         Data <- rbind (Data, getNetCDF(fnamePP, VL, F=FltPP))
    #       }
    #     }
    #     fnamePPS <<- sprintf('ALL%s', input$ProjectPP)
    #     DataPP <<- Data
    #   }
    # } else {
    #   fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
    #                       ProjectPPDir, input$ProjectPP, input$FlightPP)
    #   if (checkBad(fnamePP)) {
    #     print ('bad flight -- skipping')
    #     return()
    #   }
    #   if (fnamePPS != fnamePP) {
    #     Data <- getNetCDF (fnamePP, VL, F=input$FlightPP)
    #     fnamePPS <<- fnamePP
    #     DataPP <<- Data
    #   } else{
    #     Data <- DataPP
    #   }
    # }
    if (input$AllPP) {
      Data <- DataPP
    } else {
      Data <- DataPP[DataPP$RF == input$FlightPP, ]
    }
    
    Data$ATX <- Data[, ATsel]
    CP <- SpecificHeats (Data$EWX / Data$PSXC)
    Data$Grav <- Gravity(Data$LATC, Data$GGALT)
    Data$Ra <- CP[, 3]
    Data$cv <- CP[, 2]
    LOGP <- log (Data$PSXC)
    Data$DLP <- c(0, diff (LOGP))
    Data$DZ <- c(0, diff (Data$GGALT))
    with (Data, {
      Valid <- rep(TRUE, nrow(Data))
      Valid[is.na(PSXC)] <- FALSE
      Valid[is.na(GGALT)] <- FALSE
      Valid[is.na(EWX)] <- FALSE
      Valid[is.na(TASX)] <- FALSE
      Valid[is.na(ROLL)] <- FALSE
      Valid[is.na(MACHX)] <- FALSE
      Valid[is.na(DLP)] <- FALSE
      # Valid[abs(DLP) < 0.0005] <- FALSE
      Valid[is.na(Grav)] <- FALSE
      Valid[is.na(Ra)] <- FALSE
      Valid[TASX < 130] <- FALSE
      Valid[abs(ROLL) > 5] <- FALSE
      Valid[abs(DZ) < 2] <- FALSE
      Valid[abs(DZ) > 7.5] <- FALSE
      Valid[ATX < -80] <- FALSE
      Valid[abs(QCF-QCR) > 2] <- FALSE
      Valid <<- Valid
    })
    ## also remove the first minute of flight, when T sensor lagging
    FF <- unique(Data$RF)
    for (F in FF) {
      ixf <- which (Data$RF == F & Data$TASX > 130)[1]
      Valid[ixf:(ixf+60)] <- FALSE
    }
    Valid[is.na(Valid)] <- FALSE
    Data$Valid <- Valid
    # pf <- c(0.1,0.01)
    # pf <- c(0.1, 0.01, -0.0005)
    # result <- optim(pf, fr, gr=NULL, Data, control = list(reltol=1.e-7,parscale=c(1.,0.05, 0.0005)))
    # print (result)
    
    Data$PT <- with(Data, -Grav * DZ / (Ra * DLP)-273.15)
    dif <- Data$PT - Data$ATX
    Valid <- Data$Valid
    Valid[abs(dif) > 25] <- FALSE
    DBS <- with(Data[Valid, ], data.frame(PT-ATX, ATX))
    bs <- binStats (DBS, bins=15)
    # plotWAC(bs$ybar, bs$xc, xlab='DT')
    
    bs$sigma[bs$nb > 1] <- bs$sigma[bs$nb > 1] / sqrt(bs$nb [bs$nb > 1])
    g <- ggplot(data=bs)
    g <- g + geom_errorbarh (aes (y=xc, x=ybar, xmin=ybar-sigma, 
                                  xmax=ybar+sigma), na.rm=TRUE) 
    xlow <- floor(min (bs$ybar-bs$sigma, na.rm=TRUE))
    xhigh <- ceiling(max (bs$ybar+bs$sigma, na.rm=TRUE))
    ymin <- min (bs$xc, na.rm=TRUE) - 5
    ymax <- max (bs$xc, na.rm=TRUE) + 5
    if (xhigh < 4) {xhigh <- 4}
    if (xlow > -2) {xlow <- -2}
    g <- g + xlim(xlow, xhigh) + ylim (ymax, ymin) + theme_WAC() 
    g <- g + xlab('ATHE-ATx [deg. C]') + ylab('ATx [deg. C]') 
    g <- g + geom_point (aes (x=bs$ybar, y=bs$xc), size=3, colour='blue', na.rm=TRUE)
    g <- g + geom_label (aes (x=3.9, y=bs$xc, label=sprintf('%d', bs$nb)))
    g <- g + ggtitle (sprintf ('mean %.2f +/- %.2f', mean(DBS[, 1], na.rm=TRUE),
                               sd(DBS[, 1], na.rm=TRUE)/sqrt(nrow(DBS))))
    g <- g + theme (plot.title=element_text(size=14))
    print (g)
  })
  output$INSpitch <- renderPlot ({
    VL <- c('PITCH', 'PITCH_IRS2', 'ROLL', 'ROLL_IRS2', 'THDG', 'THDG_IRS2', 'TASX')
    RdataFile <- sprintf ('Data/dataINS%s.Rdata', input$ProjectPP)
    if (file.exists (RdataFile)) {
      load (RdataFile)
    } else {
      DataPP <- makeDataFile (input$ProjectPP, 'ALL', VL)
      save (DataPP, file=RdataFile)
    }
    if (input$AllPP) {
      Data <- DataPP
    } else {
      Data <- DataPP[DataPP$RF == input$FlightPP, ]
    }
    g <- ggplot(data=Data) + geom_histogram (aes(PITCH-PITCH_IRS2, ..density..), fill='blue', binwidth=0.01)
    g <- g + ggtitle(sprintf ('mean %.2f +/- %.2f', mean(Data$PITCH-Data$PITCH_IRS2, na.rm=TRUE),
                              sd(Data$PITCH-Data$PITCH_IRS2, na.rm=TRUE)))
    g + theme_WAC() + theme (plot.title=element_text(size=14))
  })
  
  output$ATcmpr <- renderPlot ({
    ProjectPPDir <- input$ProjectPP
    ATsc <- input$ATsc
    if (grepl('HIPPO', ProjectPPDir)) {ProjectPPDir <- 'HIPPO'}
    fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), ProjectPPDir, 
                        input$ProjectPP, input$FlightPP)
    # VL <- standardVariables (c('PS_A', 'QC_A', 'TAS_A',
    #                            'RTX', 'PALT', 'ROLL', 'QCF', 'QCR'))
    VL <- c('ATX', 'EWX', 'GGALT', 'LATC', 'MACHX', 'PALT', 'PSXC',
            'QCF', 'QCR', 'ROLL', 'TASX')
    ## add temperatures:
    TCN <- c('ATH1', 'ATH2', 'ATH3', 'ATH4', 'ATF1', 'ATF2', 'AT_A', 'AT_A2', 
             'ATFH1', 'ATFH2', 'ATHR1', 'ATHR2')
    FI <- DataFileInfo (fnamePP)
    
    VL <- c(VL, TCN[which(TCN %in% FI$Variables)])
    
    RdataFile <- sprintf ('Data/dataATHeq%s.Rdata', input$ProjectPP)
    if (file.exists (RdataFile)) {
      load (RdataFile)
    } else {
      DataPP <- makeDataFile (input$ProjectPP, 'ALL', VL)
      save (DataPP, file=RdataFile)
    }
    ch <- c('ATX', sort (TCN[which(TCN %in% names(DataPP))]))
    
    if (input$AllPP) {
      Data <- DataPP
    } else {
      Data <- DataPP[DataPP$RF == input$FlightPP, ]
    }
    ATsc <- input$ATsc
    print (c('Tvars=', ATsc))
    if (length(ATsc) < 2) {return(' ')}
    ## do each vs first, up to three:
    DBS1 <- data.frame(Data[, ATsc[1]] - Data[, ATsc[2]], Data$ATX)
    bs1 <- binStats (DBS1, bins=15)
    # plotWAC(bs$ybar, bs$xc, xlab='DT')
    
    g <- ggplot(data=bs1)
    g <- g + geom_errorbarh (aes (y=xc, x=ybar, xmin=ybar-sigma,
                                  xmax=ybar+sigma), na.rm=TRUE)
    g <- g + geom_point (aes (x=bs1$ybar, y=bs1$xc), size=3, colour='blue', na.rm=TRUE)
    g <- g + geom_label (aes (x=3.9, y=bs1$xc, label=sprintf('%d', bs1$nb)))
    g <- g + geom_path (aes (x=bs1$ybar, y=bs1$xc), colour='blue', na.rm=TRUE)
    if (length(ATsc) > 2) {
      DBS2 <- data.frame(Data[, ATsc[1]] - Data[, ATsc[3]], Data$ATX)
      bs2 <- binStats (DBS2, bins=15)
      g <- g + geom_errorbarh (data=bs2, aes (y=xc, x=ybar, xmin=ybar-sigma,
                                              xmax=ybar+sigma), na.rm=TRUE)
      g <- g + geom_point (data=bs2, aes (x=bs2$ybar, y=bs2$xc), size=3, colour='forestgreen', na.rm=TRUE)
    }
    if (length(ATsc) > 3) {
      DBS3 <- data.frame(Data[, ATsc[1]] - Data[, ATsc[4]], Data$ATX)
      bs3 <- binStats (DBS3, bins=15)    
      g <- g + geom_errorbarh (data=bs3, aes (y=xc, x=ybar, xmin=ybar-sigma,
                                              xmax=ybar+sigma), na.rm=TRUE)
      g <- g + geom_point (data=bs3, aes(x=bs3$ybar, y=bs3$xc), size=3, colour='darkorange', na.rm=TRUE)
    }
    xlow <- floor(min (bs1$ybar-bs1$sigma, na.rm=TRUE))
    xhigh <- ceiling(max (bs1$ybar+bs1$sigma, na.rm=TRUE))
    ymin <- min (bs1$xc, na.rm=TRUE) - 5
    ymax <- max (bs1$xc, na.rm=TRUE) + 5
    if (xhigh < 4) {xhigh <- 4}
    if (xlow > -2) {xlow <- -2}
    g <- g + xlim(xlow, xhigh) + ylim (ymax, ymin) + theme_WAC()
    g <- g + xlab(sprintf ('%s-%s [deg. C]', ATsc[1], ATsc[2])) + ylab('ATX [deg. C]')
    g <- g + ggtitle (sprintf ('%s-%s mean %.2f +/- %.2f', ATsc[1], ATsc[2], mean(DBS1[, 1], na.rm=TRUE),
                               sd(DBS1[, 1], na.rm=TRUE)))
    g <- g + theme (plot.title=element_text(size=14))
    print (g)
  })
  
  output$INSpitch <- renderPlot ({
    VL <- c('PITCH', 'PITCH_IRS2', 'ROLL', 'ROLL_IRS2', 'THDG', 'THDG_IRS2', 'TASX')
    RdataFile <- sprintf ('Data/dataINS%s.Rdata', input$ProjectPP)
    if (file.exists (RdataFile)) {
      load (RdataFile)
    } else {
      DataPP <- makeDataFile (input$ProjectPP, 'ALL', VL)
      save (DataPP, file=RdataFile)
    }
    if (input$AllPP) {
      Data <- DataPP
    } else {
      Data <- DataPP[DataPP$RF == input$FlightPP, ]
    }
    g <- ggplot(data=Data) + geom_histogram (aes(PITCH-PITCH_IRS2, ..density..), fill='blue', binwidth=0.01)
    g <- g + ggtitle(sprintf ('mean %.2f +/- %.2f', mean(Data$PITCH-Data$PITCH_IRS2, na.rm=TRUE),
                              sd(Data$PITCH-Data$PITCH_IRS2, na.rm=TRUE)))
    g + theme_WAC() + theme (plot.title=element_text(size=14))
  })
  
  output$INSroll <- renderPlot ({
    VL <- c('PITCH', 'PITCH_IRS2', 'ROLL', 'ROLL_IRS2', 'THDG', 'THDG_IRS2', 'TASX')
    RdataFile <- sprintf ('Data/dataINS%s.Rdata', input$ProjectPP)
    if (file.exists (RdataFile)) {
      load (RdataFile)
    } else {
      DataPP <- makeDataFile (input$ProjectPP, 'ALL', VL)
      save (DataPP, file=RdataFile)
    }
    if (input$AllPP) {
      Data <- DataPP
    } else {
      Data <- DataPP[DataPP$RF == input$FlightPP, ]
    }
    g <- ggplot(data=Data) + geom_histogram (aes(ROLL-ROLL_IRS2, ..density..), fill='blue', binwidth=0.01)
    g <- g + ggtitle(sprintf ('mean %.2f +/- %.2f', mean(Data$ROLL-Data$ROLL_IRS2, na.rm=TRUE),
                              sd(Data$ROLL-Data$ROLL_IRS2, na.rm=TRUE)))
    g + theme_WAC() + theme (plot.title=element_text(size=14))
  })
  
  output$INShdg <- renderPlot ({
    VL <- c('PITCH', 'PITCH_IRS2', 'ROLL', 'ROLL_IRS2', 'THDG', 'THDG_IRS2', 'TASX')
    RdataFile <- sprintf ('Data/dataINS%s.Rdata', input$ProjectPP)
    if (file.exists (RdataFile)) {
      load (RdataFile)
    } else {
      DataPP <- makeDataFile (input$ProjectPP, 'ALL', VL)
      save (DataPP, file=RdataFile)
    }
    if (input$AllPP) {
      Data <- DataPP
    } else {
      Data <- DataPP[DataPP$RF == input$FlightPP, ]
    }
    Data$DTHDG <- Data$THDG - Data$THDG_IRS2Cp <- SpecificHeats (Data$EWX / Data$PSXC)[, 1]
    Data$X <- Data$TASX^2 / (2 * Cp)
    # Data$RTHR1 <- ShiftInTime (Data$RTHR1, .shift=-2300)
    r <- setRange(Data, 41449, 42329)
    cf <- coef(lm (RTHR1 ~ X, data=Data[r,]))
    xp <- c(7,25)
    yp <- cf[1] + cf[2] * xp
    d <- data.frame(xp=xp, yp=yp)
    Xlab <- expression(paste(V^2,'/(2',c[p],')', sep=''))
    g <- ggplot (data=Data[r, ], aes(x=X, y=RTHR1)) + geom_point(colour='blue') 
    g <- g + geom_path(data=d, aes(x=xp, y=yp), colour='darkorange', lwd=1.5, lty=2) 
    g + ylab('Recovery Temperature [K]') + xlab(Xlab) + theme_WAC()
    
    rp <- !is.na(Data$DTHDG) & Data$DTHDG > 180
    rn <- !is.na(Data$DTHDG) & Data$DTHDG < -180
    Data$DTHDG[rp] <- Data$DTHDG[rp] - 360
    Data$DTHDG[rn] <- Data$DTHDG[rn] + 360
    g <- ggplot(data=Data) + geom_histogram (aes(DTHDG, ..density..), fill='blue', binwidth=0.01)
    g <- g + xlim(-1,1)
    g <- g + ggtitle(sprintf ('mean %.2f +/- %.2f', mean(Data$DTHDG, na.rm=TRUE),
                              sd(Data$DTHDG, na.rm=TRUE)))
    g + theme_WAC() + theme (plot.title=element_text(size=14))
  })
  
  output$plotCR <- renderPlot ({
    print (sprintf ('entry to plotCR, selCR is %s', input$selCR))
    if (input$selCR == 'none') {
      plot (0.5, 0.5, type='n')
      title ('no maneuver for this selection')
    } else {
      item <- as.integer (input$selCR)
      if (countCR == 1) {
        minT <<- DCR$Time[1]; maxT <<- DCR$Time[nrow(DCR)]
        print (item); print (minT); print (maxT)
        if (Trace) {print (sprintf ('in plotCR, item/minT/maxT=%d %s %s', item, minT, maxT))}
        # mint <- as.POSIXlt (minT, tz='UTC'); maxT <- as.POSIXlt (maxT, tz='UTC')
        r1CR <<- getIndex(DCR, CR$Start[item]); r2CR <<- getIndex(DCR, CR$End[item])
        selT1 <- DCR$Time[getIndex(DCR, CR$Start[item])]
        selT2 <- DCR$Time[getIndex(DCR, CR$End[item])]
        updateSliderInput (session, 'sliderCR', min=minT, max=maxT, value=c(selT1, selT2))
        print (c('updating time slider, limits are:', minT, maxT))
        r3CR <<- getIndex(DCR, CR$Other1[item]); r4CR <<- getIndex (DCR, CR$Other2[item])
        if (Trace) {print(sprintf('countCR section r4CR is %d', r4CR))}
        countCR <<- countCR + 1
      }
      r1CR <<- which(DCR$Time >= input$sliderCR[1])[1]
      r2CR <<- which(DCR$Time >= input$sliderCR[2])[1]
      
      if (is.na(r1CR)) {r1CR <<- 1}
      if (is.na(r2CR)) {r2CR <<- nrow(DCR)}
      r1 <- r1CR:r2CR
      ## save the selected limits in the CR data.frame
      irev <- which(Maneuvers$Project == CR$Project[item] & Maneuvers$End == CR$End[item] & 
                      Maneuvers$Type == CR$Type[item] & Maneuvers$Flight == CR$Flight[item])[1]
      if (!is.na(irev)) {irev <<- irev}
      ## get the appropriately formatted four times:
      CR$Start[item] <<- as.integer(gsub(':', '', formatTime(DCR$Time[r1CR])))
      CR$End[item] <<- as.integer(gsub(':', '', formatTime(DCR$Time[r2CR])))
      if (Trace) {print (sprintf ('new maneuver %d limits are %d %d', irev,
                                  CR$Start[item], CR$End[item]))}
      ## this is not saved in the Maneuvers database until 'save times' is clicked with this item selected
      # re <- r[-which(r %in% rp)]
      # print (c('re ', re))
      DCR$selected <- rep(0, nrow(DCR))
      # if (length(re) > 0) {DCR$selected[re] <- NA}
      ## adjust for SSRD shift:
      if (input$plotTypeCR == 'track') {
        plotTrack (DCR, xc=NA, .Spacing=2, lty=3)
        # plotTrack (DCR, xc=NA, .Spacing=100, .Range=r1, .Add=TRUE, col='red', lwd=5)  ## xc=NA is flag to plot a drifting track
        # plotTrack (DCR, xc=NA, .Spacing=100, .Range=r2, .Add=TRUE, col='forestgreen', lwd=3)
        ## find the wind from GPS only
        # define a chisquare error function:
        csq <- function (x, .d) {
          roll <- .d$ROLL * pi / 180
          gamma <- (.d$THDG + x[4] + .d$SSLIP*cos(roll) - .d$ATTACK*sin(roll)) * pi / 180 
          dvx <- (.d$TASX+x[3])*sin(gamma) -x[1] - .d$GGVEW
          dvy <- (.d$TASX+x[3])*cos(gamma) -x[2] - .d$GGVNS
          chisq <- sum (dvx**2 + dvy**2)
        }
        
        NL <- nrow(DCR$GGVEW)
        wx <- 10.
        wy <- 10.
        dV <- 0.5
        dG <- 0.5
        DCR$AKRD <- DCR$ATTACK
        A <- nlm (csq, c(wx, wy, dV, dG), DCR, hessian=TRUE)
        rms <- sqrt(A$minimum / nrow (DCR))
        bestFit <- A$estimate
        print (bestFit)
        
        # best-fit wind:
        bestWD <- atan2(-bestFit[1], -bestFit[2]) / Cradeg + 180. %% 360
        if (bestWD > 360) {bestWD <- bestWD - 360}
        bestWS <- sqrt(bestFit[1]**2 + bestFit[2]**2)
        WDM <- DCR$WDC * pi / 180.
        WSM <- DCR$WSC
        WEW <- WSM * sin (WDM)
        WNS <- WSM * cos (WDM)
        WEWbar <- mean (WEW, na.rm=TRUE)
        WNSbar <- mean (WNS, na.rm=TRUE)
        WDMbar <- atan2 (WEWbar, WNSbar) * 180. / pi
        if (WDMbar < 0) {WDMbar <- WDMbar + 360}
        WSMbar <- mean (sqrt(WEW^2+WNS^2), na.rm=TRUE)
        DCR$WDC[DCR$WDC < 180] <- DCR$WDC[DCR$WDC < 180] + 360
        aveWD <- mean (DCR$WDC, na.rm=TRUE) %% 360
        aveWS <- mean (DCR$WSC, na.rm=TRUE)
        title (main=sprintf ("mean measured wind is %.1f / %.1f; wind determined from drift: %.1f / %.1f", 
                             WDMbar, WSMbar, bestWD, bestWS), sub='drifting plot')
      } else {
        # with (DCR, plotWAC(data.frame(Time, THDG, WDC, WSC), legend.position='right'))
        nbins=20
        cfit <- function (D) {
          mwd <- mean (D$WD, na.rm=TRUE)
          D$xi <- (Cradeg * (D$THDG + D$SSLIP * cos (D$ROLL*Cradeg) - D$ATTACK * sin (D$ROLL*Cradeg))+2*pi) %% (2*pi)
          Dc <- D[abs(D$ROLL) > 26, ]
          DR <- Dc[Dc$ROLL > 0, ]
          DL <- Dc[Dc$ROLL < 0, ]
          DL$xi <- (DL$xi - mean(DL$WD, na.rm=TRUE)*Cradeg + 2*pi) %% (2*pi)
          DR$xi <- (DR$xi - mean(DR$WD, na.rm=TRUE)*Cradeg + 2*pi) %% (2*pi)
          Dc$xi <- (Dc$xi - mean(Dc$WD, na.rm=TRUE)*Cradeg + 2*pi) %% (2*pi)
          fmC <- lm (WS ~ I(cos(xi))+I(TASX*sin(xi)), data=Dc)
          fmL <- lm (WS ~ I(cos(xi))+I(TASX*sin(xi)), data=DL)
          fmR <- lm (WS ~ I(cos(xi))+I(TASX*sin(xi)), data=DR)
          cfC <- coefficients (fmC)
          cfL <- coefficients (fmL)
          cfR <- coefficients (fmR) 
          xi <- (0:360)*pi/180
          EBL <- binStats (DL[, c("WS", "xi")], bins=nbins)
          EBL$xc <- EBL$xc / Cradeg
          EBR <- binStats (DR[, c("WS", "xi")], bins=nbins)
          EBR$xc <- EBR$xc / Cradeg
          EBC <- binStats (Dc[, c("WS", "xi")], bins=nbins)
          EBC$xc <- EBC$xc / Cradeg 
          return(list(cfL, cfR, cfC, EBL, EBR, EBC))
        }
        DN <- WindProcessor (DCR, CompF=FALSE)
        DCR$WS <- DN$WSN
        DCR$WD <- DN$WDN
        # first call is to get the time shift
        cflist <- cfit(DCR)
        thdg <- DCR$THDG[abs(DCR$ROLL) > 25]
        Rate <- DataFileInfo(fnameDCR, LLrange=FALSE)$Rate
        dthdg <- abs(diff(thdg)) * Rate
        dthdg[dthdg > 10] <- NA
        TRate <- mean(dthdg, na.rm=TRUE) * Cradeg
        shiftTHDG <- (cflist[[1]][3] - cflist[[2]][3]) / (2*TRate) * 1000
        print (sprintf ('THDG shift is %.0f', shiftTHDG))
        DCR$THDG <- ShiftInTime (DCR$THDG, .rate=Rate, .shift=shiftTHDG, .mod=360)
        ## fit again
        DN <- WindProcessor (DCR, CompF=FALSE)
        DCR$WS <- DN$WSN
        DCR$WD <- DN$WDN
        cflist <- cfit(DCR)
        ## adjust the wind if necessary, and recalculate
        # Dc$TASX <- Dc$TASX + 0.1917  #0.4069
        # Dc$THDG <- Dc$THDG + 0.0026/Cradeg   #((0.0037)) / Cradeg
        cfL <- cflist[[1]]
        cfR <- cflist[[2]]
        cfC <- cflist[[3]]
        EBL <- cflist[[4]]
        EBR <- cflist[[5]]
        EBC <- cflist[[6]]
        DCR$xi <- (Cradeg * (DCR$THDG + DCR$SSLIP * cos (DCR$ROLL*Cradeg) - DCR$ATTACK * sin (DCR$ROLL*Cradeg))+2*pi) %% (2*pi)
        Dc <- DCR[abs(DCR$ROLL) > 25, ]
        DR <- Dc[Dc$ROLL > 0, ]
        DL <- Dc[Dc$ROLL < 0, ]
        xi <- (0:360)*pi/180
        Ec <- EL <- ER <- data.frame ("xi"=xi)
        Ec$WSfit <- cfC[1] + cfC[2] * cos(xi)+cfC[3]*mean(Dc$TASX, na.rm=TRUE)*sin(xi)
        EL$WSfit <- cfL[1] + cfL[2] * cos(xi)+cfL[3]*mean(DL$TASX, na.rm=TRUE)*sin(xi)
        ER$WSfit <- cfR[1] + cfR[2] * cos(xi)+cfR[3]*mean(DR$TASX, na.rm=TRUE)*sin(xi)
        ymin <- 16; ymax <- 19.5
        DL$xi <- (DL$xi - mean(DL$WD, na.rm=TRUE)*Cradeg + 2*pi) %% (2*pi)
        DR$xi <- (DR$xi - mean(DR$WD, na.rm=TRUE)*Cradeg + 2*pi) %% (2*pi)
        Dc$xi <- (Dc$xi - mean(Dc$WD, na.rm=TRUE)*Cradeg + 2*pi) %% (2*pi)
        if (input$plotTypeCR == 'WS fit') {
          meanv <- mean(Dc$TASX, na.rm=TRUE)
          clr <- c("left", "right", "std")
          col <- c ('blue', 'darkgreen')
          p <- ggplot(EBL, aes(x=xc))
          p <- p + geom_errorbar(aes(ymin=ybar-sigma, ymax=ybar+sigma, colour=clr[1])) # + ylim(ymin, ymax)
          p <- p + scale_x_continuous (breaks=c(0,90,180,270,360))
          p <- p + geom_point (aes(y = ybar, colour=clr[1], shape=clr[1]), size=2.5)
          p <- p + geom_line  (data=EL, aes(x=xi/Cradeg, y=WSfit), colour='darkorange', lty=1, lwd=1.5)
          p <- p + geom_errorbar(data=EBR, aes(ymin=ybar-sigma, ymax=ybar+sigma, colour=clr[2]))
          p <- p + geom_point (data=EBR, aes(y = ybar, colour=clr[2], shape=clr[2]), size=2.5)
          p <- p + geom_line  (data=ER, aes(x=xi/Cradeg, y=WSfit), colour='darkorange', lty=1, lwd=1.5)
          DL$WSCC <- DL$WS - cfL[2]*cos(DL$xi) - cfL[3]*DL$TASX*sin(DL$xi)
          DR$WSCC <- DR$WS - cfR[2]*cos(DR$xi) - cfR[3]*DR$TASX*sin(DR$xi)
          EBRC <- binStats (DR[, c("WSCC", "xi")], bins=nbins)
          EBRC$xc <- EBRC$xc / Cradeg
          EBLC <- binStats (DL[, c("WSCC", "xi")], bins=nbins)
          EBLC$xc <- EBLC$xc / Cradeg
          # p <- p + geom_point (data=EBLC, aes(x=xc, y=ybar), colour='black', size=2.5)
          # p <- p + geom_line (data=EBLC, aes(x=xc, y=ybar), colour='black')
          # p <- p + geom_point (data=EBRC, aes(x=xc, y=ybar), colour='magenta', size=2.5)
          # p <- p + geom_line (data=EBRC, aes(x=xc, y=ybar), colour='black')
          # p <- p + geom_errorbar(data=EBc, aes(ymin=ybar-sigma, ymax=ybar+sigma), col='red')
          # p <- p + geom_point (aes(y = ybar), pch=19, col='red', size=2.5)
          # p <- p + geom_line  (data=Ec, aes(x=xi/Cradeg, y=WSfit), colour='darkorange', lty=2, lwd=2)
          p <- p + xlab(expression(paste(xi," [", degree, "]"))) + ylab ("wind speed [m/s]")
          p <- p + scale_colour_manual("turn direction:", labels = clr, values = col)
          p <- p + scale_shape_manual ("turn direction:", labels = clr, values = c(19,19))
          p <- p + theme_WAC() + theme (legend.background=element_rect(colour='black', size=0.3, fill="ivory")) 
          p + ggtitle(sprintf ('Estimated TAS error: %.2f +/- %.2f m/s', -(cfL[2]+cfR[2])/2, abs(cfL[2]-cfR[2])/2),
                      subtitle=sprintf ('error in modified HDG: %.2f +/- %.2f deg.; THDG time shift %d ms', 
                                        (cfL[3]+cfR[3])/2/Cradeg, abs(cfL[3]-cfR[3])/2/Cradeg, as.integer(shiftTHDG))) +
            labs (caption=sprintf ('WS std. dev. before correction %.2f; after correction %.2f. \nCorresponding value from TASX is %.2f',
                                   (sd(DL$WS, na.rm=TRUE)+sd(DR$WS, na.rm=TRUE))/2,
                                   (sd(DL$WSCC, na.rm=TRUE)+sd(DR$WSCC, na.rm=TRUE))/2,
                                   (sd(DCR$TASX[abs(DCR$ROLL) < 3], na.rm=TRUE)+sd(DR$TASX, na.rm=TRUE)/2)))
        } else {  ## 'SSRD offset' case
          dbeta <- Dc$SSLIP -(Dc$PITCH-Dc$ATTACK*cos(Dc$ROLL*pi/180))/sin(Dc$ROLL*pi/180)
          # hist (dbeta)
          dbm <-mean (dbeta, na.rm=TRUE)
          dbsd <- sd   (dbeta, na.rm=TRUE) / sqrt (length(dbeta))
          cssrd <- attr(DCR$SSRD, which='SSRD')
          if (is.null(cssrd[1])) {cssrd <- attr (DCR$SSRD, 'Calib')}
          ttl <- sprintf ('mean SSRD error is %.2f +/- %.2f; orig. SSRD offset %.2f; new %.2f\nsuggested heading error is %.2f deg.', 
                          dbm, dbsd, cssrd[1], cssrd[1]-dbm, (cfL[3]+cfR[3])/(2*Cradeg)-dbm*cos(27*pi/180))
          print (ttl)
          hist(dbeta, breaks=30, freq=FALSE, xlab='sideslip error [deg.]', main=ttl)
        }
      }
    }
  })
  
  output$plotRH <- renderPlot ({
    print (sprintf ('entry to plotRH, selRH is %s', input$selRH))
    if (input$selRH == 'none') {
      plot (0.5, 0.5, type='n')
      title ('no maneuver for this selection')
    } else {
      item <- as.integer (input$selRH)
      if (countRH == 1) {
        minT <<- DRH$Time[1]; maxT <<- DRH$Time[nrow(DRH)]
        print (item); print (minT); print (maxT)
        if (Trace) {print (sprintf ('in plotRH, item/minT/maxT=%d %s %s', item, minT, maxT))}
        # mint <- as.POSIXlt (minT, tz='UTC'); maxT <- as.POSIXlt (maxT, tz='UTC')
        r1RH <<- getIndex(DRH, RH$Start[item]); r2RH <<- getIndex(DRH, RH$End[item])
        selT1 <- DRH$Time[getIndex(DRH, RH$Start[item])]
        selT2 <- DRH$Time[getIndex(DRH, RH$End[item])]
        updateSliderInput (session, 'sliderRH', min=minT, max=maxT, value=c(selT1, selT2))
        print (c('updating time slider, limits are:', minT, maxT))
        r3RH <<- getIndex(DRH, RH$Other1[item]); r4RH <<- getIndex (DRH, RH$Other2[item])
        if (Trace) {print(sprintf('countRH section r4RH is %d', r4RH))}
        countRH <<- countRH + 1
      }
      if (input$setRHT == 'leg 1') {
        r1RH <<- which(DRH$Time >= input$sliderRH[1])[1]
        r2RH <<- which(DRH$Time >= input$sliderRH[2])[1]
        {r3RH <<- getIndex(DRH, RH$Other1[item])}
        {r4RH <<- getIndex(DRH, RH$Other2[item])}
        print (sprintf (' leg 1 start time=%s, end time is %s', DRH$Time[r1RH], DRH$Time[r2RH]))
        print (sprintf ('e new r1/4 range %d:%d %d:%d', r1RH, r2RH, r3RH, r4RH))
      } else {
        r3RH <<- which(DRH$Time >= input$sliderRH[1])[1]
        r4RH <<- which(DRH$Time >= input$sliderRH[2])[1]
        print (input$sliderRH[2]); print(r4RH)
        if (is.na(r3RH) || (r3RH < r2RH)) {r3RH <<- r2RH}
        print (sprintf ('new leg 2 range %d:%d', r3RH, r4RH))
      }
      if (is.na(r1RH)) {r1RH <<- 1}
      if (is.na(r2RH)) {r2RH <<- nrow(DRH)}
      if (is.na(r3RH)) {r3RH <<- 1}
      if (is.na(r4RH)) {r4RH <<- nrow(DRH)}
      r1 <- r1RH:r2RH
      r2 <- r3RH:r4RH
      ## save the selected limits in the RH data.frame
      irev <- which(Maneuvers$Project == RH$Project[item] & Maneuvers$End == RH$End[item] & 
                      Maneuvers$Type == RH$Type[item] & Maneuvers$Flight == RH$Flight[item])[1]
      if (!is.na(irev)) {irev <<- irev}
      ## get the appropriately formatted four times:
      RH$Start[item] <<- as.integer(gsub(':', '', formatTime(DRH$Time[r1RH])))
      RH$End[item] <<- as.integer(gsub(':', '', formatTime(DRH$Time[r2RH])))
      RH$Other1[item] <<- as.integer(gsub(':', '', formatTime(DRH$Time[r3RH])))
      RH$Other2[item] <<- as.integer(gsub(':', '', formatTime(DRH$Time[r4RH])))
      if (Trace) {print (sprintf ('new maneuver %d limits are %d %d %d %d', irev,
                                  RH$Start[item], RH$End[item], RH$Other1[item], RH$Other2[item]))}
      ## this is not saved in the Maneuvers database until 'save times' is clicked with this item selected
      # re <- r[-which(r %in% rp)]
      # print (c('re ', re))
      DRH$selected <- rep(0, nrow(DRH))
      # if (length(re) > 0) {DRH$selected[re] <- NA}
      ## adjust for SSRD shift:
      DRH$Wperp <- with(DRH, (WSC*(sin(WDC*pi/180) * cos(THDG*pi/180)-cos(WDC*pi/180)*sin(THDG*pi/180))))
      DRH$Wpar <- with(DRH, (WSC*(cos(WDC*pi/180)*cos(THDG*pi/180) + sin(WDC*pi/180)*sin(THDG*pi/180))))
      if (input$sliderRHSS != 0) {
        DRH$Wperp <- DRH$Wperp + input$sliderRHSS * pi/180 * DRH$TASX
      }
      if (input$plotTypeRH == 'track') {
        plotTrack (DRH, xc=NA, .Spacing=2, lty=3)
        plotTrack (DRH, xc=NA, .Spacing=100, .Range=r1, .Add=TRUE, col='red', lwd=5)  ## xc=NA is flag to plot a drifting track
        plotTrack (DRH, xc=NA, .Spacing=100, .Range=r2, .Add=TRUE, col='forestgreen', lwd=3)
        Wperp1 <- with(DRH[r1,], mean (Wperp, na.rm=TRUE))
        Wperp2 <- with(DRH[r2,], mean (Wperp, na.rm=TRUE))
        Wpar1 <- with(DRH[r1,], mean(Wpar, na.rm=TRUE))
        Wpar2 <- with(DRH[r2,], mean(Wpar, na.rm=TRUE))
        longDiff <- (Wpar1 + Wpar2) / 2
        latDiff <- (Wperp1 + Wperp2) / 2
        SDperp1 <- with(DRH[r1,], sd (Wperp, na.rm=TRUE))
        SDperp2 <- with(DRH[r2,], sd (Wperp, na.rm=TRUE))
        SDpar1 <- with(DRH[r1,], sd(Wpar, na.rm=TRUE))
        SDpar2 <- with(DRH[r2,], sd(Wpar, na.rm=TRUE))
        SDperp <- sqrt(SDperp1^2/length(r1) + SDperp2^2/length(r2))
        SDpar <- sqrt(SDpar1^2/length(r1) + SDpar2^2/length(r2))
        ttext <- sprintf ("L1 %s %s L2 %s %s\nlongitudinal difference %.1f (%.1f) lateral %.1f (%.1f) m/s", 
                          formatTime (DRH$Time[r1[1]]), formatTime(DRH$Time[r2RH]),
                          formatTime (DRH$Time[r2[1]]), formatTime(DRH$Time[r4RH]),
                          longDiff, SDpar, latDiff, SDperp)
        title(main=ttext, sub='drifting', cex.sub=0.9)
      } else if (input$plotTypeRH == 'wind') {
        with (DRH, plotWAC(data.frame(Time, THDG, WDC, WSC), legend.position='right'))
      } else {  ## reverse-time case
        r <- 1:nrow(DRH)
        rr <- r1[length(r1)]-(r-r2[1])
        rr[rr > nrow(DRH)] <- nrow(DRH)
        rr[rr < 1] <- 1
        DRH$Wperpr <- -DRH$Wperp[rr]
        DRH$Wparr <- -DRH$Wpar[rr]
        ggplotWAC(with(DRH[r1,], data.frame(Time,Wperp,Wperpr, Wpar, Wparr)), ylab='W component [m/s]', panels=2, labelL=c('leg1', 'leg2'),
                  labelP=c('lateral', 'longitudinal'))
        
      }

    }
  })
  
  
  output$plotYM <- renderPlot ({
    # print ('entered plotYM with selYM, Proj, setYMT, sliderYM, sliderTHDGYM=')
    # isolate (print (c(input$selYM, input$ProjectPP, input$setYMT, 
    #                   input$sliderYM[1], input$sliderYM[2], input$sliderTHDGYM)))
    # item <- as.integer(input$selYM)
    # if (length(item) < 1) {return('no plot selected')}
    # itemx <<- item
    # if (is.na(itemx) || length(itemx) < 1 || itemx == 0) {return('no plot selected')}
    # if (itemx != itemYM) {
    #   countYM <<- 1
    #   itemYM <<- item
    # }
    # ProjDir <- input$ProjectPP
    # if (grepl('HIPPO', ProjDir)) {ProjDir <- 'HIPPO'}
    # VL <- c('TASX', 'GGALT', 'SSRD', 'BDIFR', 'QCF', 'WDC', 'WSC', 'THDG', 'VYC')
    # START <- AddT (as.integer (YM$Start[item]), -120)
    # END <- AddT (as.integer (YM$End[item]), 120)
    # DYM <- dataDYM(ProjDir, input$ProjectPP, YM$Flight[item], VL, START, END)
    # print (c('nrow(DYM)', nrow(DYM), START, END))
    # print (sprintf ('time range is %s--%s', DYM$Time[1], DYM$Time[nrow(DYM)]))
    print (sprintf ('entry to plotYM, selYM is %s, length(DYM) is %d', input$selYM, length(DYM)))
    if (input$selYM == 'none') {
      plot (0.5, 0.5, type='n')
      title ('no maneuver for this selection')
    } else {
      if (countYM == 1) {
        minT <<- DYM$Time[1]; maxT <<- DYM$Time[nrow(DYM)]
        # mint <- as.POSIXlt (minT, tz='UTC'); maxT <- as.POSIXlt (maxT, tz='UTC')
        updateSliderInput (session, 'sliderYM', min=minT, max=maxT, value=c(minT, maxT))
        # print (c('updating time slider, limits are:', minT, maxT))
        r1YM <<- 1; r2YM <<- nrow(DYM); r3YM <<- 1; r4YM <<- r2YM
        countYM <<- countYM + 1
      }
      if (input$setYMT == 'environment') {
        r1YM <<- which(DYM$Time >= input$sliderYM[1])[1]
        r2YM <<- which(DYM$Time >= input$sliderYM[2])[1]
        print (sprintf (' start time=%s, end time is %s', DYM$Time[r1YM], DYM$Time[r2YM]))
        r3YM <<- r1YM
        r4YM <<- r2YM
        print (sprintf ('e new r1/4 range %d:%d %d:%d', r1YM, r2YM, r3YM, r4YM))
      } else {
        r3YM <<- which(DYM$Time >= input$sliderYM[1])[1]
        r4YM <<- which(DYM$Time >= input$sliderYM[2])[1]
        if (is.na(r3YM) || (r3YM < r1YM)) {r3YM <<- r1YM}
        if (is.na(r4YM) || (r4YM > r2YM)) {r4YM <<- r2YM}
        print (sprintf ('new r3/4 range %d:%d', r3YM, r4YM))
      }
      if (is.na(r1YM)) {r1YM <<- 1}
      if (is.na(r2YM)) {r2YM <<- nrow(DYM)}
      if (is.na(r3YM)) {r3YM <<- 1}
      if (is.na(r4YM)) {r4YM <<- nrow(DYM)}
      r <- r1YM:r2YM
      rp <- r3YM:r4YM
      re <- r[-which(r %in% rp)]
      # print (c('re ', re))
      DYM$selected <- rep(0, nrow(DYM))
      if (length(re) > 0) {DYM$selected[re] <- NA}
      ## adjust for time shifts:
      if (input$sliderTHDGYM != 0) {
        DYMP <- DYM
        DYMP$THDG <- ShiftInTime(DYM$THDG, .shift=input$sliderTHDGYM)
        ## adjust WDC/WSC
        fys <- function (D) {
          x <- with(D, atan2((TASX*cos((THDG+SSRD)*pi/180)-GGVNS), 
                             (TASX*sin((THDG+SSRD)*pi/180)-GGVEW))*180/pi)
          x[x < 0] <- x[x < 0] + 360
          return(x)
        }
        fys2 <- function (D) {
          x <- with(D, sqrt((TASX*cos((THDG+SSRD)*pi/180)-GGVNS)^2 + 
                              (TASX*sin((THDG+SSRD)*pi/180)-GGVEW)^2))
          return(x)
        }
        WDC <- DYM$WDC + fys(DYMP) - fys(DYM)
        WSC <- DYM$WSC + fys2(DYMP) - fys2(DYM)
      } else {
        WDC <- DYM$WDC
        WSC <- DYM$WSC
      }
      psibar <- mean(DYM$THDG[r], na.rm=TRUE) * pi/180
      psi <- DYM$THDG * pi / 180
      delta <- WDC * pi / 180
      DYM$Lat <- WSC * (sin (psibar - pi/2) * sin(delta) + cos(psibar - pi/2)*cos(delta))
      DYM$Lat <- DYM$Lat - mean(DYM$Lat[r], na.rm=TRUE)
      DYM$VYC <- DYM$VYC - mean(DYM$VYC[r], na.rm=TRUE)
      DYM$SScomp <- DYM$TASX * DYM$SSRD * pi / 180
      # print (summary(r))
      # print (summary(rp))
      if (length(r) > 2) {
        with(DYM[r,], plotWAC(data.frame(Time, SScomp, Lat, selected), col=c('blue', 'red', 'forestgreen'), 
                              ylab='Sideslip-induced crosswind [m/s]', 
                              lwd=c(2,2,2), lty=c(1,2,1), ylim=c(-4,4)))
        cvf <- ccf(DYM$Lat[rp], DYM$SScomp[rp], plot=FALSE)
        Rmax <- max(abs(cvf$acf), na.rm=TRUE)
        sdSS <- sd(DYM$SScomp[rp], na.rm=TRUE)
        sdLat <- sd(DYM$Lat[rp], na.rm=TRUE)
        sdLate <- sd(DYM$Lat[re], na.rm=TRUE)
        text(DYM$Time[r[1]], 1, labels=sprintf ('          sd ratio %.2f', sdLat/sdLate))
        sdTrans <- Rmax * sd (DYM$Lat[rp], na.rm=TRUE)
        transmission <- sdTrans / sdSS
        title(sprintf ('transmission %.1f%% SS comp. std dev %.2f transmitted std dev %.2f m/s HDG shift %d',
                       transmission*100, sdSS, sdTrans, input$sliderTHDGYM), cex=0.8)
        # print (summary(DYM$Lat[rp])); print (summary (DYM$SScomp[r]))
      }
      # DG <- with(DYM, data.frame(Time, SSRD, VYC))
      # ggplotWAC(DG)
    }
  })
    
  output$plotPM <- renderPlot ({
    item <- as.integer(input$selPM)
    if (length(item) < 1) {return('no plot selected')}
    itemx <<- as.integer(input$selPM)
    if (is.na(itemx) || length(itemx) < 1 || itemx == 0) {return('no plot selected')}
    if (itemx != itemPM) {
      countPM <<- 1
      itemPM <<- item
    }
    ProjDir <- input$ProjectPP
    if (grepl('HIPPO', ProjDir)) {ProjDir <- 'HIPPO'}
    VL <- c('TASX', 'PSXC', 'GGALT', 'PITCH', 'ADIFR', 'QCF', 'AKRD', 'WIC')
    START <- AddT (as.integer (PM$Start[item]), -120)
    END <- AddT (as.integer (PM$End[item]), 120)
    DPM <- dataDPM(ProjDir, input$ProjectPP, PM$Flight[item], VL, START, END)
    if (countPM == 1) {
      minT <- DPM$Time[1]; maxT <- DPM$Time[nrow(DPM)]
      updateSliderInput (session, 'sliderPM', min=minT, max=maxT)
      r1PM <<- 1; r2PM <<- nrow(DPM); r3PM <<- 1; r4PM <<- r2PM
      countPM <<- countPM + 1
    }
    if (input$setPMT == 'environment') {
      r1PM <<- which(DPM$Time >= input$sliderPM[1])[1]
      r2PM <<- which(DPM$Time >= input$sliderPM[2])[1]
    } else {
      r3PM <<- which(DPM$Time >= input$sliderPM[1])[1]
      r4PM <<- which(DPM$Time >= input$sliderPM[2])[1]
      # print (sprintf ('new r3/4 range %d:%d', r3PM, r4PM))
    }
    if (is.na(r1PM)) {r1PM <<- 1}
    if (is.na(r2PM)) {r2PM <<- nrow(DPM)}
    # DPM$ROC <- c(0, diff(DPM$GGALT)) ## done in dataDPM now
    DPM$selected <- rep(-10, nrow(DPM))
    DPM$selected[c(1:r3PM, r4PM:nrow(DPM))] <- NA
    ## adjust for time shifts:
    if (input$sliderPitchPM != 0) {
      P <- ShiftInTime(DPM$PITCH, .shift=input$sliderPitchPM)
      DPM$WIC <- DPM$WIC - DPM$TASX * (P - DPM$PITCH) * pi / 180
    }
    if (input$sliderROCPM != 0) {
      R <- ShiftInTime(DPM$ROC, .shift=input$sliderROCPM)
      DPM$WIC <- DPM$WIC + R - DPM$ROC
    }
    DG <- with(DPM[(r1PM:r2PM),], data.frame(Time, PITCH, ROC, WIC, selected))
    r <- r1PM:r2PM
    rp <- r3PM:r4PM
    re <- r[-rp]
    VAR <- var(DPM$WIC[rp], na.rm=TRUE)
    SDWIC <- sd(DPM$WIC[rp], na.rm=TRUE)
    SDWICE <- sd(DPM$WIC[re], na.rm=TRUE)
    SDROC <- sd(DPM$ROC[rp], na.rm=TRUE)
    cvf <- ccf(DPM$WIC[rp], DPM$ROC[rp], plot=FALSE)
    maxcvf <- max(abs(cvf$acf))
    Rmax <- maxcvf^2
    VarCaused <- VAR * Rmax
    sdCaused <- sqrt(VarCaused)
    transmission <- 100 * sdCaused / SDROC
    ggplotWAC(DG) + ggtitle(sprintf('transmission: %.2f%%; standard deviations: WIC %.2f ROC %.2f', 
                                    transmission, SDWIC, SDROC), subtitle=sprintf ('standard deviation of WIC in environment %.2f', SDWICE))
  })
  
  output$plotSR <- renderPlot ({
    item <- as.integer(input$selSR)
    if (length(item) < 1) {return('no plot selected')}
    ProjDir <- input$ProjectPP
    if (grepl('HIPPO', ProjDir)) {ProjDir <- 'HIPPO'}
    M <- 1
    if (grepl('angle', input$plotTypeSR)) {M <- 2}
    if (grepl('airspeed', input$plotTypeSR)) {
      M <- 3
      fn <- sprintf ('%s%s/%s%s.nc', DataDirectory (), ProjDir, input$ProjectPP, SR$Flight[item])
      VCH <- sort((DataFileInfo(fn))$Variables)
      selV <- input$varSR
      updateSelectInput(session, 'varSR', choices=VCH, selected=selV)
    }
    VL <- c('TASX', 'EWX', 'PSXC', 'RTX', 'GGALT', 'PITCH', 'ADIFR', 'QCF', 'AKRD', 'WIC', input$varSR)
    DSR <- getNetCDF (sprintf ('%s%s/%s%s.nc', DataDirectory (), ProjDir, input$ProjectPP, SR$Flight[item]), 
                      VL, Start=as.integer(SR$Start[item]), End=as.integer(SR$End[item]))
    if (M == 3) {
      DSR$dV <- c(0, diff(DSR$TASX))
      DSR$RT <- ShiftInTime(DSR[, input$varSR], .shift=input$sliderSR)
      fm <- lm(RT ~ TASX+I(TASX^2)+I(TASX^3), data=DSR)
      rms <- summary(fm)$sigma
      g <- ggplot (data=DSR, aes(x=TASX, y=RT)) + ylab(sprintf('shifted %s', input$varSR)) 
      g <- g + geom_point (data=DSR[DSR$dV > 0, ], colour='forestgreen')
      g <- g + geom_point (data=DSR[DSR$dV <= 0, ], colour='red') + theme_WAC()
      g + ggtitle(sprintf('fit rms is %.4f; delay is %d', rms, input$sliderSR))
    } else if (M == 2) {
      DSR$dH <- c(0, diff(DSR$GGALT))
      DSR$AOAREF <- with(DSR, PITCH - dH * 180 / (TASX * pi))
      DG <- with(DSR, data.frame(Time, AKRD, AOAREF, WIC))
      fm <- lm(AOAREF ~ (ADIFR / QCF), data=DSR)
      cf <- coef(fm)
      rms <- summary(fm)$sigma
      ggplotWAC(DG)+ggtitle(sprintf('fit coef %.3f %.3f rms %.2f deg.', 
                                    cf[1], cf[2], rms))
    } else {
      Cp <- SpecificHeats (DSR$EWX / DSR$PSXC)[, 1]
      DSR$X <- DSR$TASX^2 / (2 * Cp)
      DSR$RT <- ShiftInTime(DSR$RTX, .shift=input$sliderSR)
      cf <- coef(fm <- lm (RT ~ X, data=DSR))
      rms <- summary(fm)$sigma
      RTSEL <- sub('. ', '', attr(DSR$RTX, 'Depend'))
      xp <- c(min(DSR$X, na.rm=TRUE), max(DSR$X, na.rm=TRUE))
      yp <- cf[1] + cf[2] * xp
      d <- data.frame(xp=xp, yp=yp)
      Xlab <- expression(paste(V^2,'/(2',c[p],')', sep=''))
      g <- ggplot (data=DSR, aes(x=X, y=RT)) + geom_point(colour='blue') 
      g <- g + geom_path(data=d, aes(x=xp, y=yp), colour='darkorange', lwd=1.5, lty=2)
      g <- g + ggtitle(sprintf('RTX is %s; fit coefficients %.3f %.3f rms %.2f; delay %d ms',
                               RTSEL, cf[1], cf[2], rms, input$sliderSR))
      g <- g + ylab('Recovery Temperature [K]') + xlab(Xlab) + theme_WAC()
      g
    }
  })
  
  output$RS3apng <- renderImage ({
    list(src = sprintf('~/RStudio/RSessions/RSessions/Session3/S%02d.png', input$S3aframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  
  output$RS3bpng <- renderImage ({
    list(src = sprintf('~/RStudio/RSessions/RSessions/Session3/S%02d.png', input$S3bframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  
  output$RS3cpng <- renderImage ({
    list(src = sprintf('~/RStudio/RSessions/RSessions/Session3/S%02d.png', input$S3cframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  
  output$RS3dpng <- renderImage ({
    list(src = sprintf('~/RStudio/RSessions/RSessions/Session3/S%02d.png', input$S3dframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  
  output$RS4png <- renderImage ({
    list(src = sprintf('~/RStudio/RSessions/RSessions/Session4/S%02d.png', input$S4frame),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS5png <- renderImage ({
    list(src = sprintf('~/RStudio/RSessions/RSessions/Session5/S%02d.png', input$S5frame),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS5apng <- renderImage ({
    list(src = sprintf('~/RStudio/RSessions/RSessions/Session5/S%02d.png', input$S5aframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS5bpng <- renderImage ({
    list(src = sprintf('~/RStudio/RSessions/RSessions/Session5/S%02d.png', input$S5bframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS5cpng <- renderImage ({
    list(src = sprintf('~/RStudio/RSessions/RSessions/Session5/S%02d.png', input$S5cframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS6png <- renderImage ({
    list(src = sprintf('~/RStudio/RSessions/RSessions/Session6/S%02d.png', input$S6frame),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS7png <- renderImage ({
    list(src = sprintf('~/RStudio/RSessions/RSessions/Session7/S%02d.png', input$S7frame),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS8png <- renderImage ({
    list(src = sprintf('~/RStudio/RSessions/RSessions/Session8/S%02d.png', input$S8frame),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  
  outputOptions (output, 'display', priority=-10)
  outputOptions (output, 'stats', priority=-10)
  outputOptions (output, 'listing', priority=-10)
  outputOptions (output, 'hist', priority=-10)
  outputOptions (output, 'barWvsZ', priority=-10)
  
  ## additions for DPcheck:
  observeEvent (input$plot_brushTDP, {
    xmin <- as.integer(input$plot_brushTDP$xmin)
    xmax <- as.integer(input$plot_brushTDP$xmax)
    T1 <- as.POSIXlt(xmin, origin='1970-01-01', tz='UTC')
    T2 <- as.POSIXlt(xmax, origin='1970-01-01', tz='UTC')
    TB1 <- T1$hour*10000 + T1$min*100 + T1$sec
    TB2 <- T2$hour*10000 + T2$min*100 + T2$sec
    #   print (sprintf ('brush times are %d %d', TB1, TB2))
    updateSliderInput (session, 'dqftimes', value=c(T1, T2))
    dfqtimes <<- c(T1, T2)
  }) 
  
  observeEvent (input$searchTDP, {
    project <- isolate (input$ProjectKP)
    flight <- isolate (input$FlightKP)
    constructDQF(project, flight)
    if (nrow(DQF) > 0) {
      chDQF <- vector('character', nrow(DQF))
      for (i in 1:nrow(DQF)) {
        u <- ifelse (DQF$Use[i], 'Y', 'N')
        print (s <- sprintf ('%s %d %s-%s', u, i, 
                             formatTime(DQF$Start[i]), formatTime(DQF$End[i])))
        chDQF[i] <- sprintf ('%d', i)
        names(chDQF)[i] <- s
      }
      updateRadioButtons(session, inputId='overshoot', choices=chDQF, selected='1')
      minT <- DataTDP$Time[which (DataTDP$Time >= DQF$Start[1])[1]]
      step <- 10
      minT <- minT - as.integer (minT) %% step - step
      maxT <- DataTDP$Time[which (DataTDP$Time >= DQF$End[1])[1]]
      maxT <- maxT - as.integer (maxT) %% step + step
      times <- c(minT-120, maxT+120)
      updateSliderInput (session, 'timesTDP', value=times)
      updateSliderInput (session, 'dqftimes', min=times[1], max=times[2], value=c(DQF$Start[1], DQF$End[1]))
    }
  })
  
  observeEvent (input$autoFlag, {
    i1 <- i2 <- 1
    iL <- nrow(DataTDP)
    while (!is.na(i1)) {
      i1 <- which(DataTDP$DPLQUAL[i2:iL] != 0 & DataTDP$TASX[i2:iL] > 100)[1]+i2-1
      DQ <- DataTDP$DPLQUAL[i1]
      if (is.na(i1)) {break}
      i2 <- which (DataTDP$DPLQUAL[i1:iL] != DQ)[1]-1+i1
      if (mean(DataTDP$MIRRTMP_DPL[i1:i2], na.rm=TRUE) > -30) {
        print (sprintf ('DQ flag %s--%s', 
                        DataTDP$Time[i1], DataTDP$Time[i2]))
        DQF <<- rbind (DQF, DataTDP.frame(Start=DataTDP$Time[i1], End=DataTDP$Time[i2], 
                                      qfStart=DataTDP$Time[i1], qfEnd=DataTDP$Time[i2], 
                                      Use=TRUE, Flag=-DQ/10))
        # with(DataTDP[(i1-120):(i2+120),], plotWAC (DataTDP.frame (Time, MIRRTMP_DPL, MT_DPL,
        #                                                     DPERR, ATX, DPLQUAL)))
        # abline(v=DataTDP$Time[i1], lwd=0.5, lty=2); abline(v=DataTDP$Time[i2], lwd=0.5, lty=2)
      }
    }
  })
  
  observeEvent (input$addVXL, {
    showNotification ('Generating VCSEL prediction of DPL mirror temperature: This takes a minute or so. Please wait for this message to disappear before continuing.', duration=NULL, id='notice', type='message')
    DataTDP <<- addDPERR (DataTDP)
    removeNotification (id='notice')
    reac$newplotTDP <- reac$newplotTDP + 1
  })
  
  observeEvent (input$resetTDP, {
    itm <- isolate (input$overshoot)
    if (length(itm) > 0 && itm != 'none') {
      itm <- as.integer(itm)
      times <- c(DQFsave$Start[itm], DQFsave$End[itm])
      updateSliderInput (session, 'dqftimes', value=times)
      DQF$Use[itm] <<- FALSE
      DQF$Start[itm] <<- DQFsave$Start[itm]
      DQF$End[itm] <<- DQFsave$End[itm]
      if (nrow(DQF) >= itm) {
        print (s <- sprintf ('N %d %s-%s', itm, 
                             formatTime(DQF$Start[itm]), formatTime(DQF$End[itm])))
        chDQF[itm] <<- sprintf ('%d', itm)
        names(chDQF)[itm] <<- s
        updateRadioButtons(session, inputId='overshoot', choices=chDQF, selected=itm)
      }
    }
  })
  
  observeEvent (input$nextTDP, {
    times <- input$timesTDP
    dt <- difftime (times[2], times[1])
    times[1] <- times[1] + dt
    times[2] <- times[2] + dt
    updateSliderInput (session, 'timesTDP', value=times)
  })
  
  observeEvent (input$prevTDP, {
    times <- input$timesTDP
    dt <- difftime (times[2], times[1])
    times[1] <- times[1] - dt
    times[2] <- times[2] - dt
    updateSliderInput (session, 'timesTDP', value=times)
  })
  
  observeEvent (input$saveTDP, {
    project <- isolate (input$ProjectKP)
    flight <- isolate (input$FlightKP)
    fileDQF <- sprintf ('DQF%srf%02d.Rdata', project, flight)
    save(DQF, file=fileDQF)
    print (sprintf ('%s saved', fileDQF))
  })
  
  reac <- reactiveValues (newplotTDP=0)
  
  
  observe ({
    itm <- input$overshoot
    print (sprintf ('entry to overshoot observer, overshoot item = %s', itm))
    if (itm != 'none' && length(itm) > 0) {
      itm <- as.integer(itm)
      # if (itm == itmL) {DQF$Use[itm] <<- !DQF$Use[itm]}
      # itmL <<- itm
      minT <- DataTDP$Time[which (DataTDP$Time >= DQF$Start[itm])[1]]
      step <- 10
      minT <- minT - as.integer (minT) %% step - step
      maxT <- DataTDP$Time[which (DataTDP$Time >= DQF$End[itm])[1]]
      maxT <- maxT - as.integer (maxT) %% step + step
      times <- c(minT-90, maxT+90)
      updateSliderInput (session, 'timesTDP', value=times)
      updateSliderInput (session, 'dqftimes', min=times[1], max=times[2], 
                         value=c(DQF$Start[itm], DQF$End[itm]))
      if (nrow(DQF) > 0) {
        chDQF <- vector('numeric')
        for (i in 1:nrow(DQF)) {
          u <- ifelse (DQF$Use[i], 'Y', 'N')
          print (s <- sprintf ('%s %d %s-%s', u, i, 
                               formatTime(DQF$Start[i]), formatTime(DQF$End[i])))
          chDQF[i] <- sprintf ('%d', i)
          names(chDQF)[i] <- s
        }
        updateRadioButtons(session, inputId='overshoot', choices=chDQF, selected=itm)
        chDQF <<- chDQF
      }
    }
  })
  
  observe ({
    print (sprintf ('entry to dqftimes observer, value is %s %s', 
                    input$dqftimes[1], input$dqftimes[2]))
    itm <- isolate(input$overshoot)
    if (length(itm) > 0 && itm != 'none') {
      itm <- as.integer(itm)
      times <- input$dqftimes
      if (nrow(DQF) >= itm && (times[1] != DQFsave$Start[itm] || times[2] != DQFsave$End[itm])) {
        DQF$Use[itm] <<- TRUE
        DQF$Start[itm] <<- times[1]
        DQF$End[itm] <<- times[2]
        print (s <- sprintf ('Y %d %s-%s', itm, 
                             formatTime(DQF$Start[itm]), formatTime(DQF$End[itm])))
        chDQF[itm] <<- sprintf ('%d', itm)
        names(chDQF)[itm] <<- s
        updateRadioButtons(session, inputId='overshoot', choices=chDQF, selected=itm)
      }
    }
  })
  
  output$dewpointPlot <- renderPlot({
    reac$newplotTDP
    if (!exists('DataTDP')) {
      getDataTDP(input$ProjectKP, input$FlightKP)
      times <- c(DataTDP$Time[1], DataTDP$Time[nrow(DataTDP)])
      updateSliderInput (session, 'timesTDP', min=times[1], max=times[2], value=times)
    }
    print (sprintf ('TDP time interval is %s -- %s', input$timesTDP[1], input$timesTDP[2]))
    r1 <- which (DataTDP$Time >= input$timesTDP[1])[1]
    r2 <- which (DataTDP$Time >= input$timesTDP[2])[1]
    print (c('TDP r1 r2', r1,r2))
    if ('MT_DPL' %in% names (DataTDP)) {
      with (DataTDP[r1:r2,], plotWAC (data.frame (Time, MIRRTMP_DPL, MT_DPL, 
                                               DPERR, ATX, CBAL, DPLQUAL), 
                                   col=c('blue', 'forestgreen', 'red', 'cyan', 'brown', 'magenta'),
                                   lty=c(1,1,1,1,2,2), lwd=c(2,2,1,2,2,1),
                                   ylim=c(-40,40), legend.position='topleft'))
    } else {
      with (DataTDP[r1:r2,], plotWAC (data.frame (Time, MIRRTMP_DPL, DPERR, ATX, CBAL, DPLQUAL), 
                                   col=c('blue', 'red', 'cyan', 'brown', 'magenta'),
                                   lty=c(1,1,1,2,2), lwd=c(2,1,2,2,1),
                                   ylim=c(-40,40), legend.position='topleft'))
    }
    abline(h=15, col='magenta', lwd=2, lty=3)
    abline(h=0)
    abline(v=input$dqftimes[1], lwd=0.5, lty=2); abline(v=input$dqftimes[2], lwd=0.5, lty=2)
  })
  
  ## in-cloud stuff:
  getDataIC <- reactive({                     ## data
    Project <- input$ProjectKP
    Flight <- input$FlightKP
    if (grepl('HIPPO', Project)) {
      ProjDir <- 'HIPPO'
    } else {
      ProjDir <- Project
    }
    
    dfile <- sprintf ('inCloud/DataIC%s.Rdata', Project)
    if (file.exists(dfile)) {
      load(dfile)    # loads DataIC
    } else {
      DataIC <- data.frame()
      VarListIC <- standardVariables (c('CONCD_', 'PLWCD_', 'DBARD_', 'CCDP_'))
      #Data <- getNetCDF(fname, VarListIC)
      Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjDir),
                              sprintf ("%srf...nc$", Project)))
      if (!is.na (Fl[1])) {
        for (Flt in Fl) {
          FltIC <- sub('.*rf', '', sub ('.nc$', '', Flt))
          FltIC <- as.integer (FltIC)
          fname <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
                            ProjDir, Project, FltIC)
          print (fname)
          DataIC <- rbind (DataIC, getNetCDF(fname, VarListIC, F=FltIC))
        }
      }
      namesData <- names(DataIC)
      namesData[which(grepl('CONCD_', namesData))] <- 'CONCD'
      namesData[which(grepl('PLWCD_', namesData))] <- 'PLWCD'
      namesData[which(grepl('DBARD_', namesData))] <- 'DBARD'
      namesData[which(grepl('CCDP_', namesData))] <- 'CCDP'
      names(DataIC) <- namesData
      save(DataIC, file=dfile)
    }
    if (!input$allIC) {
      Data <- DataIC[DataIC$RF == Flight,]
    } else {
      Data <- DataIC
    }
    Data$InCloud <- Data$CONCD > input$concd & Data$PLWCD > input$lwcd
    Data$InCloud[is.na(Data$InCloud)] <- FALSE
    # require csec-prior-seconds to also be in cloud:
    ix <- which(Data$InCloud)
    ixx <- ix;
    for (i in ixx) {
      tst <- TRUE
      for (j in 1:input$csec) {
        tst <- tst & Data$InCloud[i-j]
      }
      if (!tst) {
        ix[which(i == ix)] <- -1
      }
      # if (!Data$InCloud[i-2] || !Data$InCloud[i-1]) {
      #   ix[which(i == ix)] <- -1
      # }
    }
    ix <- ix[ix > 0]
    Data <<- Data
    times <- c(Data$Time[1], Data$Time[nrow(Data)])
    print (sprintf ('times = %s %s', times[1], times[2]))
    updateSliderInput (session, 'timesIC', min=times[1], max=times[2], value=times)
    Data$IC <- rep(FALSE, nrow(Data))
    Data$IC[ix] <- TRUE
    return (Data)
  })
  
  observeEvent (input$plot_brushIC, {
    xmin <- as.integer(input$plot_brushIC$xmin)
    xmax <- as.integer(input$plot_brushIC$xmax)
    T1 <- as.POSIXlt(xmin, origin='1970-01-01', tz='UTC')
    T2 <- as.POSIXlt(xmax, origin='1970-01-01', tz='UTC')
    TB1 <- T1$hour*10000 + T1$min*100 + T1$sec
    TB2 <- T2$hour*10000 + T2$min*100 + T2$sec
    print (sprintf ('brush times are %d %d', TB1, TB2))
    updateSliderInput (session, 'timesIC', value=c(T1, T2))
  }) 
  
  observeEvent (input$resetTIC, {
    step <- 60
    minT <- Data$Time[1]
    minT <- minT - as.integer (minT) %% step + step
    maxT <- Data$Time[nrow(Data)]
    maxT <- maxT - as.integer (maxT) %% step
    times <- c(minT, maxT)
    updateSliderInput (session, 'timesIC', value=times)
  })
  
  observeEvent (input$nextIC, {
    times <- isolate(input$timesIC)
    dt <- difftime (times[2], times[1])
    times <- times + dt
    updateSliderInput (session, 'timesIC', value=times)
  } )
  
  observeEvent (input$prevIC, {
    times <- isolate(input$timesIC)
    dt <- difftime (times[2], times[1])
    times <- times - dt
    updateSliderInput (session, 'timesIC', value=times)
  } )
  
  
  # observeEvent (input$add, {
  #   DFP <- isolate(input$Project)
  #   DFF <- isolate (input$Flight)
  #   times <- isolate (input$times)
  #   print (sprintf ('adding to CloudEvent data.frame, values are %s %d %s %s', DFP, DFF, times[1], times[2]))
  #   print ('before adding:')
  #   print (CloudEvents)
  #   CloudEvents <<- rbind(CloudEvents, data.frame(Project=DFP, Flight=DFF, Start=times[1], End=times[2]))
  #   print ('after')
  #   print (CloudEvents)
  # })
  
  observeEvent (input$resetIC, {
    itm <- isolate (input$inCloud)
    if (length(itm) > 0 && itm != 'none') {
      itm <- as.integer(itm)
      times <- c(BadCloudEventsSave$Start[itm], BadCloudEventsSave$End[itm])
      updateSliderInput (session, 'timesIC', value=times)
      BadCloudEvents$Rej[itm] <<- 0
      BadCloudEvents$Start[itm] <<- BadCloudEventsSave$Start[itm]
      BadCloudEvents$End[itm] <<- BadCloudEventsSave$End[itm]
      flight <- isolate(input$FlightKP)
      project <- isolate(input$ProjectKP)
      if (nrow(BadCloudEvents) >= itm) {
        print (s <- sprintf ('N %d %s rf%02d %s-%s', itm, project, flight,
                             formatTime(BadCloudEvents$Start[itm]), formatTime(BadCloudEvents$End[itm])))
        chIC[itm] <<- sprintf ('%d', itm)
        names(chIC)[itm] <<- s
        updateRadioButtons(session, inputId='inCloud', choices=chIC, selected=itm)
      }
    }
  })
  
  observeEvent (input$setBadIC, {
    if (!exists ('BadCloudEvents')) {
      BadCloudEvents <<- data.frame()
    }
    times <- isolate (input$timesIC)
    project <- isolate (input$ProjectKP)
    flight <- isolate (input$FlightKP)
    BadCloudEvents <<- rbind(BadCloudEvents, data.frame(Project=project, Flight=flight, Start=times[1], 
                                                        End=times[2], Rej=1))
    BadCloudEventsSave <<- BadCloudEvents
    if (nrow(BadCloudEvents) > 0) {
      chIC <- vector('character', nrow(BadCloudEvents))
      for (i in 1:nrow(BadCloudEvents)) {
        u <- ifelse (BadCloudEvents$Rej[i], 'Y', 'N')
        print (s <- sprintf ('%s %d %s rf%02d %s-%s', u, i, BadCloudEvents$Project[i], BadCloudEvents$Flight[i],
                             formatTime(BadCloudEvents$Start[i]), formatTime(BadCloudEvents$End[i])))
        chIC[i] <- sprintf ('%d', i)
        names(chIC)[i] <- s
      }
      chIC <<- chIC
      updateRadioButtons(session, inputId='inCloud', choices=chIC, selected=sprintf('%d', nrow(BadCloudEvents)))
      # minT <- DataTDP$Time[which (DataTDP$Time >= DQF$Start[1])[1]]
      # step <- 10
      # minT <- minT - as.integer (minT) %% step - step
      # maxT <- DataTDP$Time[which (DataTDP$Time >= DQF$End[1])[1]]
      # maxT <- maxT - as.integer (maxT) %% step + step
      # times <- c(minT-120, maxT+120)
      # updateSliderInput (session, 'timesTDP', value=times)
      # updateSliderInput (session, 'dqftimes', min=times[1], max=times[2], value=c(DQF$Start[1], DQF$End[1]))
    }
  })
  
  observeEvent (input$saveIC, {
    print ('saving BadCloudEvents data.frame')
    saveICEvents()
  })
  
  observeEvent (input$searchIC, {
    project <- isolate (input$ProjectKP)
    flight <- isolate (input$FlightKP)
    constructIC(project, flight)
    if (nrow(DIC) > 0) {
      chDIC <- vector('character', nrow(DIC))
      for (i in 1:nrow(DIC)) {
        u <- ifelse (DIC$Use[i], 'Y', 'N')
        print (s <- sprintf ('%s %d %s-%s', u, i, 
                             formatTime(DIC$Start[i]), formatTime(DIC$End[i])))
        chDIC[i] <- sprintf ('%d', i)
        names(chDIC)[i] <- s
      }
      updateRadioButtons(session, inputId='inCloud', choices=chDIC, selected='1')
      minT <- DataIC$Time[which (DataIC$Time >= DIC$Start[1])[1]]
      step <- 10
      minT <- minT - as.integer (minT) %% step - step
      maxT <- DataIC$Time[which (DataIC$Time >= DIC$End[1])[1]]
      maxT <- maxT - as.integer (maxT) %% step + step
      times <- c(minT-120, maxT+120)
      updateSliderInput (session, 'timesIC', value=times)
    }
  })
  
  output$inCloudPlot <- renderPlot({
    input$ProjectKP
    input$FlightKP
    data <- getDataIC()
    times <- input$timesIC
    i1 <- which(data$Time >= times[1])[1]
    i2 <- which(data$Time >= times[2])[1]
    d <- data[i1:i2,]
    ## make sensitive to changes in radioButtons:
    input$inCloud
    ## eliminate times included in BadCloudEvents
    if (exists('BadCloudEvents')) {
      for (i in 1:nrow(BadCloudEvents)) {
        if (BadCloudEvents$Rej[i] < 1) {next}
        j1 <- which(d$Time >= BadCloudEvents$Start[i])[1] 
        j2 <- which(d$Time >= BadCloudEvents$End[i])[1] 
        print (sprintf ('event %d j1=%d, j2=%d times are %s--%s', i, j1, j2, BadCloudEvents$Start[i], BadCloudEvents$End[i]))
        if (!is.na(j1) && !is.na(j2) && j2-j1 > 0) {
          d <- d[-c(j1:j2),]
        }
      }
    }
    if (sum(d$IC, na.rm=TRUE) < 2) {
      plot(c(-1,1), c(-1,1), type='n')
      text (0,0,labels='no in-cloud data for this flight')
    } else {
      # fm <- lm(ATX ~ DPXC, data=d[d$IC,])
      d$ATX <- setNA(d$ATX, 0)
      d$DPXC <- setNA(d$DPXC, 0)
      df <- with(d[d$IC,], DemingFit(ATX, DPXC))
      rsd <- df[1]+df[2]*d$ATX[d$IC]-d$DPXC[d$IC]
      # print(df)        
      # print(rsd)
      irm <- which(abs(rsd) > input$sigIC*df[3])
      # print (sprintf( 'irm is %d', irm))
      dd <- d[d$IC,]
      if (length(irm) > 0) {
        dd <- dd[-irm,]
      }
      if (nrow(dd) < 2 || sum(dd$IC, na.rm=TRUE) < 2) {
        plot(c(-1,1), c(-1,1), type='n')
        text (0,0,labels='no in-cloud data for this flight')
      } else {
        if (input$pltIC == 'scatterplot') {
          with(dd, plotWAC(data.frame(DPXC, ATX), xlab='dew point', type='p', pch=20))
          lines(c(-50,50), c(-50,50), col='darkorange', lwd=2, lty=2)
          df <- with(dd, DemingFit(ATX, DPXC))
          rmsu <- with(dd, sqrt(mean((ATX-DPXC)^2, na.rm=TRUE)))
          title(sprintf ('Deming fit: ATX=%.2f+%.2f*DPXC, rms=%.2f, 1:1 rms=%.2f for %d points', df[1], df[2], df[3], rmsu, sum(dd$IC)))
          print (df)
        } else {
          hist (dd$ATX-dd$DPXC, breaks=c(min(min(dd$ATX-dd$DPXC, na.rm=TRUE),-2.1),(0:40)*0.1-2,max(max(dd$ATX-dd$DPXC, na.rm=TRUE),2.1)), 
                xlim=c(-2,2), xlab='ATX-DPXC',
                main=sprintf ('mean %.2f sd %.2f', mean(dd$ATX-dd$DPXC, na.rm=TRUE),
                              sd(dd$ATX-dd$DPXC, na.rm=TRUE)))
        }
      }
    }
  })
  
  output$ICtimePlot <- renderPlot({
    input$ProjectKP
    input$FlightKP
    data <- getDataIC()
    times <- input$timesIC
    i1 <- which(data$Time >= times[1])[1]
    i2 <- which(data$Time >= times[2])[1]
    pv <- input$pvarIC
    if (pv == 'ATX/DPXC') {
      pv <- c('ATX', 'DPXC')
    }
    plotWAC(data[i1:i2,c('Time',pv)])
    d <- data[i1:i2,]
    d[!d$IC, pv[1]] <- NA
    lines(d$Time, d[,pv[1]], lwd=3, col='red')
  })
  
  output$quickPlot <- renderPlot ({
    reac$quick
    if (Trace) {print ('quickPlot: entered')}
    Data <- data ()
    if (!(quickPlotVar %in% names (Data))) {
      isolate(reac$newdata <- reac$newdata + 1)
      isolate (reac$quick <- reac$quick + 1)
      return ()
    }
    times <- isolate(input$times)
    Data <- Data[Data$Time >= times[1] & Data$Time <= times[2], ]
    plotWAC(Data[, c('Time', quickPlotVar)])
  })
  
  
}

