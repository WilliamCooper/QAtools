### This is a model for adding a function:
RPlotn <- function(data) {
  ## be sure that the variables are in 'VarList'. If not, where VarList
  ## is defined, add the new variable to the variable names or follow the
  ## present definition with VarList <- c(VarList, "NewVariable1", "NewVariable2")
  # next just resets geometry, in case previous plot used multiple panes
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
  ## simplest plot:
  plotWAC (data[, c("Time", "Var1", "Var2")])
  AddFooter ()
}

