savePDF <- function(Data, inp) {
  print ('entered savePDF')
  FlightPDF <<- sprintf ('%s%02d', inp$typeFlight, inp$Flight)
  plotfile <<- sprintf("%s%s%02dPlots.pdf", inp$Project, inp$typeFlight, inp$Flight)
  unlink (plotfile)
  cairo_pdf (filename = plotfile, onefile=TRUE, width = 10, height=7.5, 
             pointsize=10, family='sans', fallback_resolution = 50)
  ## enable something like the next to get individual png files instead of one large pdf
  #### png (file = sprintf ("./Figures/WINTER%s-%%02d.png", Flight))
  print (sprintf ("saving plots to file %s", plotfile))
  DataV <- limitData (Data, inp)
  t1 <- times[1]
  t <- as.POSIXlt (t1)
  StartTime <<- as.integer (10000*t$hour+100*t$min+t$sec)
  DataV <- DataV[(DataV$Time > times[1]) & (DataV$Time < times[2]), ]
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
  ## transfer the attributes to DataV (for now, main use is CDP sizes)
  DataV <- transferAttributes (DataV, Data)  
  for (np in c(1:30)) {
    # Skip plots if VRPlot[[]] is missing
    if (length(VRPlot[[np]]) < 2 && (is.na(VRPlot[[np]][1]) || (VRPlot[[np]][1] == ''))) {next}
    if (file.exists (sprintf ("./PlotFunctions/RPlot%d.R", np))) {
      sourceFile <- sprintf ('RPlot%d', np)
      if (!exists (sourceFile)) {
        source (sprintf ('./PlotFunctions/%s.R', sourceFile))
      }
      if (testPlot(np) && (length(VRPlot[[np]]) > 0)) {
        print(paste('Plot',np))
        ## eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
        if (np == 1) {
          RPlot1 (DataV, sprintf ('%s%02d', inp$typeFlight, inp$Flight))
        } else {
          eval(parse(text=sprintf("RPlot%d(DataV)", np)))
        }
      }
    }
  }
  dev.off()
  system(sprintf ('cp %s www/latestPlots.pdf', plotfile))
  #   suppressWarnings(if (length (system ('which evince', intern=TRUE)) > 0) {
  #     system (sprintf ('evince %s', plotfile))
  #   })
  # if (suppressWarnings(library(rstudio, logical.return=TRUE))) {
  #   rstudio::viewer ("latestPlots.pdf", height='maximize')
  # }
  print ('finished savePDF')
}
