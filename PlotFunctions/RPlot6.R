### plot 6: Ambient (Static) Pressures
RPlot6 <- function (data, ...) { 
  layout(matrix(1:3, ncol = 1), widths = 1)
  op <- par (mar=c(5,5,5,1),oma=c(0,3,0,3))
  par(cex.lab=2, cex.main=2)
# Note: as per custom, y-axis is reversed
    plotWAC (DF <- data[, c("Time", VRPlot[[6]])], 
             ylab='Static Pressures [hPa]',  ylim = rev(range(data[,VRPlot[[6]][1]])))

# # Rather than over-plotting the difference PS_A-PSXC, I'm going to create another panel  
# # >>>>>>>>>>>>
#   if (('PSXC' %in% VRPlot[[6]]) && ('PS_A' %in% VRPlot[[6]])) {
#     points (data$Time, (data$PS_A-data$PSXC)*50+600, type='l', col='brown', lty=2)
#   }
#   axis (4, at=c(500,600,700), labels=c("-2", "0", "2"), col='brown', 
#         col.axis='brown')
#   abline (h=500, col='brown', lty=2); abline (h=700, col='brown', lty=2)
# # <<<<<<<<<<<<

# If there are more pressure estimates than just PSXC, report mean differences
  ir <- which ('PSXC' == VRPlot[[6]])
  if (length (ir) != 1) {
    ir <- which ('PS_A' %in% VRPlot[[6]])
  }
  if (length (ir) != 1) {ir <- 1}
  labl <- VRPlot[[6]]
  #labl <- sub("PS", "", labl)
  titl <- "Mean differences: "
  for (i in 1:length(labl)) {
    if (i == ir) {next}
    titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i], labl[ir],
                    mean(data[, VRPlot[[6]][i]] -
                           data[, VRPlot[[6]][ir]], na.rm=TRUE))
  } 
  title(main = paste("Static Pressures",'\n',titl))
  
  
  # Here is the new panel with pressure differences 
  # >>>>>>>>>>>>
  if (('PSXC' %in% VRPlot[[6]]) && ('PS_A' %in% VRPlot[[6]])) {
    plotWAC(data$Time, (data$PS_A-data$PSXC), ylab='Pressure Difference [hPa]' ,
            ylim=c(-4,4))
 # These next lines set grid to yrange spanning -4 to 4.
    for (ny in seq(-4,4,by=1)){
      abline(h=ny, lwd=1, lty=3, col='gray')
    }
    abline (h=-2, lwd=1.5, lty=2); abline (h=2, lwd=1.5, lty=2)
    title('PS_A minus PSXC')
  }
  # <<<<<<<<<<<<
  
  AddFooter ()
}
