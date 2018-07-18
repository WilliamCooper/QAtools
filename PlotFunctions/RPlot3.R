### plot 3: plot all temperatures, one plot; plot differences
RPlot3 <- function (data, ...) { 
  layout(matrix(1:3, ncol = 1), widths = 1)
  op <- par (mar=c(5,5,5,1),oma=c(0,3,0,3))
  par(cex.lab=2, cex.main=2)

  ylb <- expression (paste ("ATy  [", degree, "C]"))
  plotWAC (data[, c("Time", VRPlot[[3]])],
           ylab=ylb)

# Report T differences in plot subtitle
  labl <- VRPlot[[3]]
  #labl <- sub("AT", "", labl)
  ir <- which ('ATX' == labl)
  if (length (ir) != 1) {
    ir <- which ('ATH1' %in% labl)
  }
  if (length (ir) != 1) {ir <- 1}
  titl <- "Mean differences: "
  for (i in 1:length(labl)) {
    if (i == ir) {next}
    titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i], labl[ir],
                    mean(data[, VRPlot[[3]][i]] -
                           data[, VRPlot[[3]][ir]], na.rm=TRUE))
  }
  title(main = paste("Temperatures",'\n',titl))
  
#   
# Plot differences
if (length(labl)>1){
  ir <- which ('ATX' == labl)
  if (length (ir) != 1) {
    ir <- which ('ATH1' %in% labl)
  }
  if (length (ir) != 1) {ir <- 1}
 
  nonref <- labl[-ir]
  nonrefID <- match(nonref,labl)

  # Create a dummy data frame for T differences
  tmp<-data[,c("Time",labl[-ir])]
  for (nn in 2:(dim(tmp)[2])){
    tmp[,nn]<-tmp[,nn]-data[,labl[ir]]
    names(tmp)[nn]<-sprintf("%s-%s", names(tmp[nn]), labl[ir]) 
  }

  ylb <- expression (paste ("Differences  [", degree, "C]"))
  plotWAC (tmp, ylab=ylb, ylim=c(-4,4))
 
   # These next lines set grid to yrange spanning -4 to 4.
  for (ny in seq(-4,4,by=1)){
    abline(h=ny, lwd=1, lty=3, col='gray')
  }
  
  # titl <- "Mean differences: "
  # for (i in 1:length(labl)) {
  #   if (i == ir) {next}
  #   titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i], labl[ir],
  #                   mean(data[, VRPlot[[3]][i]] -
  #                          data[, VRPlot[[3]][ir]], na.rm=TRUE))
  # }
  # title(titl)
}
   
    
  
  AddFooter ()
}
