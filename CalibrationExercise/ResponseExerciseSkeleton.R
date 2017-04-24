library(Ranadu)
tau <- 1
N <- 2000
T <- 5
a <- 0.01
b <- 0.04
b <- 0.0001
e <- 0.001
k <- 0.05
I <- 1
kbyI <- k / I
ebyI <- e / I
dt <- T/N
t <- (1:N)*(T+1)/N-1
type <- 'step'
type <- 'impulse'
type <- 'ramp'
type <- 'square wave'
type <- 'triangle'
if (type == 'step') {
  x <- rep(1,N)
} else if (type == 'ramp') {
  x <- t
} else if (type == 'impulse') {
  x <- rep(0,N)
  x[t >= 0 & t < 0.1] <- 1
} else if (type == 'sine wave') {
  freq <- 1
  x <- sin(2*pi*t*freq)
} else if (type == 'square wave') {
  freq <- 1
  x <- sin(2*pi*t*freq)
  x <- ifelse (x >= 0, 1., -1.)
} else if (type == 'triangle') {
  freq <- 1
  x <- asin (sin(2*pi*t*freq)) * 2 / pi
}
x[t <= 0] <- 0
D1 <- 0 # ifelse (input$d1o, (log10 (gamma)+2)/4, 0)
M <- M2 <- Mdot <- rep(0,N)
for (i in 1:(N-1)) {
  # H1 <- (1 - D1) * (x[i]-M[i])/tau
  H1 <- (((x[i]*0.1-b)/a)^2 - M[i]^2) * kbyI * (1 - ebyI)
  M[i+1] <- M[i] + H1 * dt
  # H2 <- ((x[i]-M2[i])*kbym-Dbym*Mdot[i])
  # Mdot[i+1] <- Mdot[i] + H2 * dt
  # M2[i+1] <- M2[i] + Mdot[i] * dt
}
# plot(t,M, type='l', lwd=2, col='blue')
# lines(t,N, col='darkorange',lty=1) 
DP <- data.frame(Time=t, M1=M, x=x*10)
plotWAC(DP, ylab="M or x", xlab=expression(paste("Time / ",tau)), 
        lwd=c(2,2,2), ylim=c(-10,100))
# if (type == 'ramp' || type == 'step' || type == 'impulse') {
#   title(main=sprintf('omega=%.2f gamma %.02f', omega, gamma), cex=0.8)
# } else {
#   title(main=sprintf('frequency=%.2f, omega=%.2f gamma %.02f', freq, omega, gamma), cex=0.8)
# }