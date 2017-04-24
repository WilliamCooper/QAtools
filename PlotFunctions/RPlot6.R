### plot 6: ambient pressures
RPlot6 <- function (data, ...) { 
  op <- par (mar=c(5,4,1,2)+0.1, oma=c(1.1,0,0,0))
  layout(matrix(1:1, ncol = 1), widths = 1, heights = 5)
  plotWAC (DF <- data[, c("Time", VRPlot[[6]])], ylab='pressure  PSy [hPa]')
  if (('PSXC' %in% VRPlot[[6]]) && ('PS_A' %in% VRPlot[[6]])) {
    points (data$Time, (data$PS_A-data$PSXC)*50+600, type='l', col='brown', lty=2)
  }
  axis (4, at=c(500,600,700), labels=c("-2", "0", "2"), col='brown', 
        col.axis='brown')
  abline (h=500, col='brown', lty=2); abline (h=700, col='brown', lty=2)
  # legend ("bottomleft", 
  #         legend=c("(PSXC-PS_A)*50+600", "+/-2 hPa"),
  #         lty=c(1,2), cex=0.75,
  #         col=c('red', 'red'))
  ir <- which ('PSXC' == VRPlot[[6]])
  if (length (ir) != 1) {
    ir <- which ('PS_A' %in% VRPlot[[6]])
  }
  if (length (ir) != 1) {ir <- 1}
  labl <- VRPlot[[6]]
  labl <- sub("PS", "", labl)
  titl <- "Mean diff: "
  for (i in 1:length(labl)) {
    if (i == ir) {next}
    titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i], labl[ir],
                    mean(data[, VRPlot[[6]][i]] -
                           data[, VRPlot[[6]][ir]], na.rm=TRUE))
  }
  title(titl, cex.main=0.8)
  AddFooter ()
}
