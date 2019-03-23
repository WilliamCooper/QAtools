
RPlot24 <- function(data, Seq=NA, panl=1, ...) {
  ## This is a sample that can be modified to create a new plot
  # This only finds 'renderPlot' for a call from the 'display' in shiny.
  # For a call from savePDF this will be FALSE.
  shinyDisplay <- any(grepl('renderPlot', sys.calls()))
  
  panel11 <- function (data) {
    np1 <- 1:((length (VRPlot$PV24)+1)/2)
    np2 <- (np1[length (np1)]+1):length (VRPlot$PV24)
    ifelse (exists ('panel1ylim'),
      plotWAC (data[, c("Time", VRPlot$PV24[np1])], ylim=panel1ylim),
      plotWAC (data[, c("Time", VRPlot$PV24[np1])])
    )
  }
  
  panel12 <- function (data) {
    np1 <- 1:((length (VRPlot$PV24)+1)/2)
    np2 <- (np1[length (np1)]+1):length (VRPlot$PV24)
    ifelse (exists ('panel2ylim'),
      plotWAC (data[, c("Time", VRPlot$PV24[np2])], ylim=panel2ylim),
      plotWAC (data[, c("Time", VRPlot$PV24[np2])])
    )
  }
  
  ###########################################################
  if (shinyDisplay) {
    op <- par (mfrow=c(1,1), mar=c(5,5,1,1)+0.1,oma=c(1.1,0,0,0))
    switch(panl,
      {
        op <- par (mar=c(1,5,1,1)+0.1)
        panel11 (data)
      },
      {
        panel12 (data)
        AddFooter()
      }
    )
    
    ###########################################################
  } else {
    ## be sure that the variables are in 'VarList'. If not, where VarList
    ## is defined, add the new variable to the variable names or follow the
    ## present definition with VarList <- c(VarList, "NewVariable1", "NewVariable2")
    # use two-pane layout
    # layout(matrix(1:1, ncol = 1), widths = 1, heights = c(5,6))
    # op <- par (mar=c(5,4,1,2.5)+0.1,oma=c(1.1,0,0,0))
    layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
    op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
    panel11 (data)
    op <- par (mar=c(5,4,1,1)+0.1)
    panel12(data)
    AddFooter ()
  }
}
