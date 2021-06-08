# functions used later:
hline <<- function(y, col='black', lwd=1, lty=2) {
  abline(h=y, col=col, lwd=lwd, lty=lty)
}
formatTime <- function (time) {
  t <- as.POSIXlt (time)
  tt <- sprintf ("%d:%02d:%02d", t$hour, t$min, t$sec)
  return (tt)
}
## convenience replacements to avoid all the times I have to type "na.rm=TRUE"
mean <- function(x, ..., na.rm = TRUE) {
  suppressWarnings (base::mean(x, ..., na.rm = na.rm))
}
sd <- function (x, ..., na.rm = TRUE) {
  suppressWarnings (stats::sd(x, ..., na.rm = na.rm))
}
min <- function (x, ..., na.rm = TRUE) {
  suppressWarnings (base::min(x, ..., na.rm = na.rm))
}
max <- function (x, ..., na.rm = TRUE) {
  suppressWarnings (base::max(x, ..., na.rm = na.rm))
}
setNA <- function (.x, .v) {
  X <- zoo::na.approx (as.vector (.x), maxgap=1000, na.rm=FALSE)
  X[is.na(X)] <- .v
  return (X)
}
## all possible temperature variables
ATVARS <- c('ATX', 'AT_A', 'AT_A2', 'ATH1', 'ATH2', 'ATH3', 'ATH4', 'ATF1', 'ATF2',
  'ATHR1', 'ATHR2', 'ATHL1', 'ATHL2', 'ATFH1', 'ATFH2')

# source('makeDataFile.R') ## I think this is not needed

