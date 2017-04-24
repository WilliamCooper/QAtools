### plot 3: plot all temperatures, one plot
RPlot3 <- function (data, ...) { 
  par(oma=c(1.1,0,0,0))
  ylb <- expression (paste ("temperature  ATy  [", degree, "C]"))
  plotWAC (data[, c("Time", VRPlot[[3]])],
           ylab=ylb, lty=c(1,1,1,2), lwd=c(2,1.5,1,2,1),
           legend.position='bottomleft')
  labl <- VRPlot[[3]]
  labl <- sub("AT", "", labl)
  titl <- "Mean diff "
  for (i in 2:length(labl)) {
    titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[1],
                    mean(data[, VRPlot[[3]][i]] -
                         data[, VRPlot[[3]][1]], na.rm=TRUE))
  }
  title(titl, cex.main=0.8)
  AddFooter ()
}
