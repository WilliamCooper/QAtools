
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
                              ' ', ' ', ' ', sep='<br/>')}
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
  
  observe ({                              ## typeFlight
    if (Trace) {print (sprintf ('entered typeFlight observer with value %s', input$typeFlight))}
    typeFlight <<- input$typeFlight
    reac$newdata <- TRUE
  })
  
  observe ({
    if (input$ProjectPP != ProjectPP) {
      ProjectPP <<- input$ProjectPP
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
  
  
  observe ({                          ## global time
    if (Trace) {print ('entering global-time observer')}
    times <<- input$times
  })
  
  ################ REACTIVES ########################
  
  reac <- reactiveValues (newdata=FALSE, newdisplay=FALSE)
  
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
    VL <- c('PSXC', 'PS_A', 'QCXC', 'QC_A', 'TASX', 'GGVSPD', 'ADIFR', 'QCF', 'ROLL')
    # print (sprintf('entered PSplot, inputs %s %s %s', input$ProjectPP, input$FlightPP, input$AllPP))
    if (input$AllPP) {
      ## loop through all the flights in this project:
      Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjectPPDir),
                              sprintf ("%srf...nc$", input$ProjectPP)))
      if (!is.na (Fl[1])) {
        Data <- data.frame()
        for (Flt in Fl) {
          FltPP <- sub('.*rf', '', sub ('.nc$', '', Flt))
          FltPP <- as.integer (FltPP)
          fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
                              ProjectPPDir, input$ProjectPP, FltPP)
          Data <- rbind (Data, qualifyPS(fnamePP, VL, FltPP))
        }
      }
      fnamePP <<- 'All'
    } else {
      fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
                          ProjectPPDir, input$ProjectPP, input$FlightPP)
      Data <- qualifyPS (fnamePP, VL, input$FlightPP)
      fnamePP <<- fnamePP
    }
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
    VL <- c('PSXC', 'PS_A', 'QCXC', 'QC_A', 'TASX', 'GGVSPD', 'ADIFR', 'QCF', 'ROLL')
    if (input$AllPP) {
      ## DataPP is already in global environment
    } else if (fname != fnamePP) {
      Data <- qualifyPS (fnamePP, VL, input$Flight) 
      DataPP <<- Data
      fnamePP <<- fname
    }
    cf <- c(-2.2717351e+00, 1.0044060e+00, 1.7229198e-02, -3.1450368e-06) #CSET only
    cf <- c(-2.6239872e+00, 1.0063093e+00, 1.6020764e-02, -4.6657542e-06)  #CSET+ORCAS+DEEPWAVE
    DataPP$PSFIT <- with(DataPP, cf[1] + PS_A * (cf[2] + cf[4] * PS_A) + cf[3] * QC_A)
    bs <- with(DataPP, binStats(data.frame(PSXC-PSFIT, PSXC), bins=10))
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
    VL <- c('PSXC', 'PS_A', 'QCXC', 'QC_A', 'TASX', 'GGVSPD', 'ADIFR', 'QCF', 'ROLL')
    # print (sprintf('entered PSplot, inputs %s %s %s', input$ProjectPP, input$FlightPP, input$AllPP))
    # print (sprintf ('QCplot %s %s %d', fnamePP, input$ProjectPP, input$FlightPP))
    if (input$AllPP) {
      ## loop through all the flights in this project:
      Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjectPPDir),
                              sprintf ("%srf...nc$", input$ProjectPP)))
      if (!is.na (Fl[1])) {
        Data <- data.frame()
        for (Flt in Fl) {
          FltPP <- sub('.*rf', '', sub ('.nc$', '', Flt))
          FltPP <- as.integer (FltPP)
          fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
                              ProjectPPDir, input$ProjectPP, FltPP)
          Data <- rbind (Data, qualifyPS(fnamePP, VL, FltPP))
        }
      }
      fnamePP <<- 'All'
      DataPP <<- Data
    } else {
      fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
                          ProjectPPDir, input$ProjectPP, input$FlightPP)
      DataPP <<- qualifyPS (fnamePP, VL, input$FlightPP)
      fnamePP <<- fnamePP
    }
    cfq <- c(2.7809637e+00, 9.7968460e-01, -6.7437126e-03, 4.8584555e-06)
    DataPP$QCFIT <- with(DataPP, cfq[1] + PS_A * (cfq[3] + cfq[4] * PS_A) + cfq[2] * QC_A)
    M <- with(DataPP,
              sprintf('mean and std dev: %.2f +/- %.2f hPa', 
                      mean(QCXC-QCFIT, na.rm=TRUE), sd(QCXC-QCFIT, na.rm=TRUE)))
    b <- ceiling(with(DataPP, (max(QCXC-QCFIT, na.rm=TRUE)-min(QCXC-QCFIT, na.rm=TRUE))*20))
    with(DataPP, hist (QCXC-QCFIT, breaks=b, xlim=c(-2,2), xlab='QCXC-QCFIT [hPa]',
                       freq=FALSE, main=M))
  })
  
  output$QCSplot <- renderPlot ({
    ProjectPPDir <- input$ProjectPP
    if (grepl('HIPPO', ProjectPPDir)) {ProjectPPDir <- 'HIPPO'}
    fname <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), input$ProjectPP, input$ProjectPP, input$FlightPP)
    VL <- c('PSXC', 'PS_A', 'QCXC', 'QC_A', 'TASX', 'GGVSPD', 'ADIFR', 'QCF', 'ROLL')
    if (input$AllPP) {
      ## DataPP is already in global environment
    } else if (fname != fnamePP) {
      Data <- qualifyPS (fnamePP, VL, input$Flight) 
      DataPP <<- Data
      fnamePP <<- fname
    }
    cfq <- c(2.7809637e+00, 9.7968460e-01, -6.7437126e-03, 4.8584555e-06)
    DataPP$QCFIT <- with(DataPP, cfq[1] + PS_A * (cfq[3] + cfq[4] * PS_A) + cfq[2] * QC_A)
    bs <- with(DataPP, binStats(data.frame(QCXC-QCFIT, QCXC), bins=10))
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
    VL <- standardVariables (c('AT_A', 'PS_A', 'QC_A', 'TAS_A', 'GGVSPD', 'ROLL'))
    # print (sprintf('entered PSplot, inputs %s %s %s', input$ProjectPP, input$FlightPP, input$AllPP))
    if (input$AllPP) {
      ## loop through all the flights in this project:
      Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjectPPDir),
                              sprintf ("%srf...nc$", input$ProjectPP)))
      if (!is.na (Fl[1])) {
        Data <- data.frame()
        for (Flt in Fl) {
          FltPP <- sub('.*rf', '', sub ('.nc$', '', Flt))
          FltPP <- as.integer (FltPP)
          fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
                              ProjectPPDir, input$ProjectPP, FltPP)
          Data <- rbind (Data, qualifyPS(fnamePP, VL, FltPP))
        }
      }
      fnamePP <<- 'All'
    } else {
      fnamePP <- sprintf ('%s%s/%srf%02d.nc', DataDirectory(), 
                          ProjectPPDir, input$ProjectPP, input$FlightPP)
      Data <- qualifyPS (fnamePP, VL, input$FlightPP)
      fnamePP <<- fnamePP
    }
    cfA <- c(0.28178716560, 1.01636832046, -0.00012560891)  ## const, AT_A, AT_A^2
    Data$ATFIT <- with(Data, cfA[1] + AT_A * (cfA[2] + cfA[3] * AT_A))
    DataPP <<- Data
    # with(Data, plotWAC(data.frame(Time, PSXC-PS_A), ylim=c(-2,2), ylab='PSXC-PS_A'))
    M <- with(Data,
              sprintf('mean and std dev: %.2f +/- %.2f hPa', 
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
    VL <- standardVariables (c('AT_A', 'PS_A', 'QC_A', 'TAS_A', 'GGVSPD', 'ROLL'))
    if (input$AllPP) {
      ## DataPP is already in global environment
    } else if (fname != fnamePP) {
      Data <- qualifyPS (fnamePP, VL, input$Flight) 
      DataPP <<- Data
      fnamePP <<- fname
    }
    cfA <- c(0.28178716560, 1.01636832046, -0.00012560891)  ## const, AT_A, AT_A^2
    DataPP$ATFIT <- with(DataPP, cfA[1] + AT_A * (cfA[2] + cfA[3] * AT_A))
    bs <- with(DataPP, binStats(data.frame(ATX-ATFIT, PSXC), bins=10))
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
    Data$DTHDG <- Data$THDG - Data$THDG_IRS2
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
  
  outputOptions (output, 'display', priority=-10)
  outputOptions (output, 'stats', priority=-10)
  outputOptions (output, 'listing', priority=-10)
  outputOptions (output, 'hist', priority=-10)
  outputOptions (output, 'barWvsZ', priority=-10)
}

