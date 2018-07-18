RPlot14 <- function(data, ...) {
  ## be sure that the variables are in 'VarList'. If not, where VarList
  ## is defined, add the new variable to the variable names or follow the
  ## present definition with VarList <- c(VarList, "NewVariable1", "NewVariable2")
  # next just resets geometry, in case previous plot used multiple panes
  # layout(matrix(1:1, ncol = 1), widths = 1, heights = c(5,6))
  # op <- par (mar=c(5,4,1,2.5)+0.1,oma=c(1.1,0,0,0))

  layout(matrix(1:4, ncol = 1))
  op <- par (mar=c(5,5,5,1),oma=c(0,3,0,3))
  par(cex.lab=2, cex.main=2)
  
# Panel 1: RSTB
  ylb <- expression (paste ("Temperature  [", degree, "C]")) 
  if ("RSTB" %in% VRPlot[[14]]) {
    if ("RSTB1" %in% VRPlot[[14]]) {
      plotWAC (data[, c("Time", "RSTB", "RSTB1")], ylab=ylb)
    } else {
      plotWAC (data[, c("Time", "RSTB")], ylab=ylb)
    }
    # title()
  }
  
# Panel 2: RSTT
  if ("RSTT" %in% VRPlot[[14]]) {
    plotWAC (data[, c("Time", "RSTT")], ylab=ylb)
  }

# Panel 3: VISB    
  if ("VISB" %in% VRPlot[[14]]) {
    plotWAC (data[, c("Time", "VISB")], ylab=ylb)
  } else { 
  
# Panel 3 alternate: IRBC 
  if ("IRBC" %in% VRPlot[[14]] && "IRTC" %in% VRPlot[[14]]) {
    plotWAC (data[, c("Time", "IRBC", "IRTC")], ylab=ylb)
  } 
  }
print(VRPlot[[14]])
# Panel 4: TRSTB
  if ("TRSTB" %in% VRPlot[[14]]) {
    plotWAC (data[, c("Time", "TRSTB")], ylab=ylb)
    # par(new=T)
    # plotWAC(data[,c("Time","GGALT")], axes=FALSE, xlab=NA, ylab=NA, col='black', lwd=1)
    # axis(side=4)
    # mtext(side=4, line=3, 'Altitude [m]')
    
  }
 # AddFooter ()
}
