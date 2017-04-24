#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## global stuff here
load('CalData.Rdata')
CalData <<- CalData
options("digits"=4)
SummarizeFit <- function(ft) {
  options("digits"=4)
  # print (summary(ft)$call)
  print ("Coefficients:")
  print (summary(ft)$coefficients)
  print (sprintf ("Residual standard deviation: %.3f, dof=%d<br>", summary(ft)$sigma, summary(ft)$df[2]))
  print (sprintf ("R-squared %.3f", summary(ft)$r.squared))
}
with (CalData, {
  fm1 <<- lm (M ~ x);
  fm2 <<- lm (x ~ M);
  fm3 <<- lm (M ~ x + I(x^2));
  fm4 <<- lm (x ~ M + I(M^2))  
})
cf1 <- coef(fm1)
cf2 <- coef(fm2)
cf3 <- coef(fm3)
cf4 <- coef(fm4)


ui <- fluidPage(
  # Application title
  titlePanel("Fitting to the Calibration"),
  tabsetPanel (id='whichTab', type='pills',
               tabPanel ('the exercise',
                         includeHTML('CalibrationExerciseA.html')
               ), 
               tabPanel ('your answer',
                         includeHTML('CalibrationExerciseB.html'),
                         fluidRow (
                           column (4, numericInput ('m55', label='x for M=55:', value=0)),
                           column (8, textOutput ('m55a', container=pre))
                         ),
                         fluidRow (
                           column (8, textInput ('fformula', label='formula: x = ', value='0.9+0.2*M+0.0001*M^2',
                                                 placeholder=' (0.9+0.2*M+0.0001*M**2     '))
                           # column (4, actionButton (inputId='checkIt', label='Check It:'))
                         ),
                         fluidRow (
                           column (4, textOutput ('chksum', container=pre)),
                           column (8, plotOutput ('showfit'))
                         )
               ), 
               tabPanel ('help with fitting',
                         sidebarLayout(
                           sidebarPanel(
                             fluidRow (
                               column (6, actionButton (inputId='manual', label = 'More Info',
                                                        onclick ="window.open('https://drive.google.com/open?id=0B1kIUH45ca5AZWI5QllIdFpFR0U', '_blank')")),
                               column (6, checkboxInput('reverse', label='M=f(x)', value=FALSE))),
                             numericInput ('fitOrder', label='Order of Polynomial', 
                                           min=1, max=5, step=1, value=1), 
                             includeHTML ('CalibrationExerciseInfo.html'),
                             
                             width=4
                           ),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             plotOutput("calibrationPlot"),
                             htmlOutput ('fitSummary', container=pre)
                             # includeHTML ("TransferFunctionInfo.html"), width=6
                           )
                         )
               ),               
               tabPanel ('our solution',
                         includeHTML ('CalibrationExerciseC.html')
               ),
               tabPanel ('more',
                         includeHTML ('CalibrationExerciseD.html'),
                         column(6, plotOutput('hrplot'))
                         ),
               tabPanel ('notes',
                         includeHTML ('CalibrationExerciseE.html')
                         )
               
  )
)

server <- function(input, output, session) {
  
  reac <- reactiveValues (newfit=0, chkm55=0, updatefit=0)
  
  output$fitSummary <- renderPrint({
    reac$newfit
    SF <- readLines ('SummaryFile.txt')
    SF <- gsub ('"', '', SF)
    SF <- gsub ('\\[.\\] ', '', SF)
    print(SF)
  })
  
  output$m55a <- renderPrint ({
    reac$chkm55
    pcterror=abs(input$m55-11.96)/11.96 * 100
    if (pcterror < 0.3) {
      print (sprintf ('That\'s within 0.3%% - excellent!'))
      print ('That\'s about the best expected with this calibration.')
      print ('My answer is 11.96')
    } else if (pcterror < 1) {print (sprintf ('That\'s within 1%% - not bad, but you can do better!'))}
    else if (pcterror < 2) {print (sprintf ('That\'s within 2%% - try again'))}
    else if (pcterror < 5) {print (sprintf ('That\'s within 5%% - way off!'))}
  })
  
  output$chksum <- renderPrint ({
    reac$updatefit
    y <- NA
    M <- CalData$M
    e <- paste ('y <- ', input$fformula, sep='')
    try(eval (parse (text=e)), silent=TRUE)
    if (!is.na(y[1])) {
      sf <- sd (y - CalData$x)
      print (sprintf ('standard deviation is %.3f', sf))
    }
  })
  
  observeEvent (input$checkIt, {
    e <- paste ('y <- ', input$fformula, sep='')
    print (sprintf ('in checkIt, e is %s', e))
    y <- NA
    M <- CalData$M
    try(eval (parse (text=e)), silent=TRUE)
    if (!is.na(y[1])) {
      sf <<- sd (y - CalData$x)
      print (sprintf ('sd=%.3f', sf))
    }
    isolate (reac$updatefit <- reac$updatefit + 1)
  })
  
  observeEvent (input$manual, seeManual ())

  output$hrplot <- renderPlot ({
    fm8 <- with(CalData, lm (x ~ MP + I(MP^2) + I(MP^3) + I(MP^4)))
    cf8 <- coef(fm8)
    x2 <- 1:30
    a1 <- 5; a2 <- 3; a3 <- 0.1
    y5a <- a1 + a2 * x2 + a3 * x2^2
    x8 <- cf8[1] + cf8[2]*y5a + cf8[3]*y5a^2 + cf8[4]*y5a^3 + cf8[5]*y5a^4
    plot(x2, y5a, type='l', col='blue', xlab='x', ylab='M')
    points (CalData$x, CalData$MP, pch=19, col='blue')
    lines (x8, y5a, col='red', lwd=2, lty=2)
    legend('bottomright', legend=c('cal points (100-pt ave.)', 'true calibration', '4th-order fit to ave. data'),
           col=c('blue', 'blue', 'red'), lwd=c(NA,1,2), lty=c(NA,1,2), 
           pch=c(19,NA,NA), text.col=c('blue', 'blue', 'red'))
  })
  
  output$showfit <- renderPlot({
    plot(CalData$x, CalData$M, type='p', pch=19, col='blue')
    y <- NA
    with (CalData, {
      e <- paste ('y <- ', input$fformula, sep='');
      suppressMessages (suppressWarnings (
        try (eval (parse (text=e)), silent=TRUE))
      );
      if (!is.na(y[1])) {lines (y, M, col='red', lwd=2)}
      # lines (y, z, col='red', lwd=2)
    })
    abline (h=55, col='forestgreen', lty=2)
  })
  
  output$calibrationPlot <- renderPlot({
    order <- input$fitOrder
    reverse <- input$reverse
    if (reverse) {
      e <- expression ('CalData$M ~ CalData$x')
      if (order > 1) {e <- paste (e, expression (' + I(CalData$x^2)'))}
      if (order > 2) {e <- paste (e, expression (' + I(CalData$x^3)'))}
      if (order > 3) {e <- paste (e, expression (' + I(CalData$x^4)'))}
      if (order > 4) {e <- paste (e, expression (' + I(CalData$x^5)'))}
      if (order > 5) {e <- paste (e, expression (' + I(CalData$x^6)'))}
    } else {
      e <- expression ('CalData$x ~ CalData$M')
      if (order > 1) {e <- paste (e, expression (' + I(CalData$M^2)'))}
      if (order > 2) {e <- paste (e, expression (' + I(CalData$M^3)'))}
      if (order > 3) {e <- paste (e, expression (' + I(CalData$M^4)'))}
      if (order > 4) {e <- paste (e, expression (' + I(CalData$M^5)'))}
      if (order > 5) {e <- paste (e, expression (' + I(CalData$M^6)'))}
    }
    fm <- lm (eval(e))
    SummarizeFit (fm)
    sink (file='SummaryFile.txt')
    SummarizeFit (fm)
    sink (NULL)
    isolate(reac$newfit <- reac$newfit + 1)
    cf <- coef (fm)
    plot(CalData$x, CalData$M, type='p', pch=19, col='blue')
    if (reverse) {
      y <- 0
      z <- (0:500) * (max(CalData$x) - min(CalData$x)) / 500 + min(CalData$x)
      for (i in 1:length(cf)) {y <- y + cf[i]*z^(i-1)}  
      lines (z, y, col='red', lwd=2)
    } else {
      y <- 0
      z <- (0:500) * (max(CalData$M) - min(CalData$M)) / 500 + min(CalData$M)
      for (i in 1:length(cf)) {y <- y + cf[i]*z^(i-1)}
      lines (y, z, col='red', lwd=2)
    }
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

