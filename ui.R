
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

ui <- fluidPage (
  
  # Application title
  titlePanel("QA / QC Functions"),
  tabsetPanel (id='whichTab', type='pills',
               tabPanel ('Guide',
                         includeHTML ('Information.html')),
               tabPanel ('Background',
                         tabsetPanel (id='whichBg', type='pills',
                                      tabPanel ('R sessions (tutorial)',
                                                tabsetPanel (id='whichTab', type='pills',
                                                             tabPanel ('TOC',
                                                                       includeHTML('~/RStudio/RSessions/RSessions/TOC/TOC.html')),
                                                             tabPanel ('Session1',
                                                                       tabsetPanel (id='S1tab', type='pills',
                                                                                    tabPanel ('Getting Started',
                                                                                              includeHTML('~/RStudio/RSessions/RSessions/Session1/Session1a.html')),
                                                                                    tabPanel ('RStudio Tour',
                                                                                              includeHTML('~/RStudio/RSessions/RSessions/Session1/Session1b.html')),
                                                                                    tabPanel ('Some Examples',
                                                                                              includeHTML('~/RStudio/RSessions/RSessions/Session1/Session1c.html'),
                                                                                              tabsetPanel (id='S1ex', type='pills',
                                                                                                           tabPanel ('simple plot',
                                                                                                                     includeHTML ('~/RStudio/RSessions/RSessions/Session1/E1Code.html'),
                                                                                                                     sidebarLayout(
                                                                                                                       sidebarPanel(
                                                                                                                         selectInput (inputId='S1Var', label='variable to plot', 
                                                                                                                                      choices=c('Temperature'='ATX',
                                                                                                                                                'Wind Speed'='WSC',
                                                                                                                                                'Pressure'='PSXC'))
                                                                                                                       ),
                                                                                                                       mainPanel(
                                                                                                                         plotOutput ('S1E1Plot')
                                                                                                                       )
                                                                                                                     )),
                                                                                                           tabPanel ('sounding',
                                                                                                                     includeHTML ('~/RStudio/RSessions/RSessions/Session1/E2Code.html'),
                                                                                                                     plotOutput ('S1E2Plot', width="50%")),
                                                                                                           tabPanel ('stats',
                                                                                                                     includeHTML ('~/RStudio/RSessions/RSessions/Session1/E3Code.html'),
                                                                                                                     dataTableOutput ('S1Stats')),
                                                                                                           tabPanel ('recovery factor',
                                                                                                                     includeHTML ('~/RStudio/RSessions/RSessions/Session1/E4Code.html'))
                                                                                              )
                                                                                    ),
                                                                                    tabPanel ('Text-with-Code',
                                                                                              includeHTML ('~/RStudio/RSessions/RSessions/Session1/Session1d.html')),
                                                                                    tabPanel ('Getting Ranadu',
                                                                                              includeHTML ('~/RStudio/RSessions/RSessions/Session1/Session1e.html')))),
                                                             tabPanel ('Session2',
                                                                       tabsetPanel (id='S2tab', type='pills',
                                                                                    tabPanel ('Vectors and Matrices',
                                                                                              includeHTML('~/RStudio/RSessions/RSessions/Session2/Session2a.html'),
                                                                                              htmlOutput('txtS2a'),
                                                                                              radioButtons ('selS2a', label=NULL, choices=c(
                                                                                                'select a button below'=1,
                                                                                                'a <- 1:12; print(a)'=2,
                                                                                                'dim(a) <- c(3,4); print(a)'=3,
                                                                                                'print (t(a))'=4
                                                                                              ), width='400px')
                                                                                              ),
                                                                                    tabPanel ('The data.frame',
                                                                                              includeHTML('~/RStudio/RSessions/RSessions/Session2/Session2b.html')),
                                                                                    tabPanel ('Addressing and Subsetting data.frames',
                                                                                              includeHTML('~/RStudio/RSessions/RSessions/Session2/Session2c1.html'),
                                                                                              radioButtons ('selS2c1', label=NULL, choices=c(
                                                                                                'select a button below'=1,
                                                                                                'Data$ATX[5]'=2, 
                                                                                                'Data[5, 2]'=3,
                                                                                                'Data[5, ]'=4, 
                                                                                                'Data[5, "ATX"]'=5, 
                                                                                                'Data$ATX'=6 
                                                                                              ), width='800px'),
                                                                                              htmlOutput ('txtS2c1'),
                                                                                              includeHTML('~/RStudio/RSessions/RSessions/Session2/Session2c2.html'),
                                                                                              actionButton ('XS2a', label='See an answer')
                                                                                              ),
                                                                                    tabPanel ('Some Basic Operations',
                                                                                              includeHTML ('~/RStudio/RSessions/RSessions/Session2/Session2d.html')))),
                                                             tabPanel ('Session3',
                                                                       tabsetPanel (id='S3tab', type='pills',
                                                                                    tabPanel ('R as a Calculator',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S3aframe', label='frame #',
                                                                                                            2, min=1, max=3, step=1)),
                                                                                                column (4, helpText ('Suggestion: Place cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS3apng')
                                                                                              ),
                                                                                    tabPanel ('Basic Operators and Precedence',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S3bframe', label='frame #',4,
                                                                                                            min=4, max=12, step=1)),
                                                                                                column (4, helpText ('Suggestion: Place cursor in "frame #" entry box, then',
                                                                                                          'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS3bpng')
                                                                                    ),
                                                                                    tabPanel ('Vectorized Operations',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S3cframe', label='frame #',13,
                                                                                                                         min=13, max=16, step=1)),
                                                                                                column (4, helpText ('Suggestion: Place cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS3cpng')
                                                                                              ),
                                                                                    tabPanel ('Using Variables',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S3dframe', label='frame #',17,
                                                                                                                         min=17, max=21, step=1)),
                                                                                                column (4, helpText ('Suggestion: Place cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS3dpng')
                                                                                              )
                                                                       )
                                                             ),
                                                             tabPanel ('Session4'),
                                                             tabPanel ('Session5'),
                                                             tabPanel ('Session6'),
                                                             tabPanel ('Session7'),
                                                             tabPanel ('Session8'),
                                                             tabPanel ('Session9')
                                                )
                                                
                                                ),
                                      tabPanel ('Cal Exercise',
                                                tabsetPanel (id='whichCE', type='pills',
                                                             tabPanel ('the exercise',
                                                                       includeHTML('CalibrationExercise/CalibrationExerciseA.html')
                                                             ),
                                                             tabPanel ('your answer',
                                                                       includeHTML('CalibrationExercise/CalibrationExerciseB.html'),
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
                                                                           includeHTML ('CalibrationExercise/CalibrationExerciseInfo.html'),
                                                                           
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
                                                                       includeHTML ('CalibrationExercise/CalibrationExerciseC.html')
                                                             ),
                                                             tabPanel ('more',
                                                                       includeHTML ('CalibrationExercise/CalibrationExerciseD.html'),
                                                                       column(6, plotOutput('hrplot'))
                                                             ),
                                                             tabPanel ('notes',
                                                                       includeHTML ('CalibrationExercise/CalibrationExerciseE.html')
                                                             )
                                                             
                                                )
                                      ),
                                      tabPanel('Resolution',
                                               tabsetPanel (id='whichRes', type='pills',
                                                            tabPanel ('objective',
                                                                      includeHTML('Resolution/ResolutionA.html')
                                                            ),
                                                            tabPanel ('explore def. 1',
                                                                      includeHTML('Resolution/ResolutionB.html')
                                                            ),
                                                            tabPanel ('show PDFs',
                                                                      # Sidebar with a slider for separation between measurands
                                                                      sidebarLayout(
                                                                        sidebarPanel(
                                                                          sliderInput("separation",
                                                                                      "d=distance (units of sigma) between measurands:",
                                                                                      min = 0,
                                                                                      max = 5,
                                                                                      value = 1,
                                                                                      step=0.1,
                                                                                      round=-1
                                                                          ),
                                                                          sliderInput("conf", "std dev for conf limit",
                                                                                      min=1, max=4, value=1, step=0.1)
                                                                        ),
                                                                        
                                                                        # Show a plot of the generated distribution
                                                                        mainPanel(
                                                                          plotOutput("resolutionPlot"),
                                                                          includeHTML ('Resolution/ResolutionC.html')
                                                                        )
                                                                      )
                                                            ),
                                                            tabPanel ('alternate definition',
                                                                      includeHTML('Resolution/ResolutionD.html')
                                                            ),
                                                            tabPanel ('meaning #2',
                                                                      sidebarLayout(
                                                                        sidebarPanel(
                                                                          sliderInput("bits",
                                                                                      "n: Number of bits = 2^n",
                                                                                      min = 1,
                                                                                      max = 8,
                                                                                      value = 4),
                                                                          width=3
                                                                        ),
                                                                        
                                                                        # Show a plot of the generated distribution
                                                                        mainPanel(
                                                                          plotOutput("distBins"),
                                                                          includeHTML('Resolution/ResolutionE.html')
                                                                        )
                                                                      )
                                                            ),
                                                            tabPanel ('summary',
                                                                      includeHTML('Resolution/ResolutionF.html')
                                                            )
                                               )
                                      )
                         )
               ),
               tabPanel ('Past Projects',
                         fluidRow (
                           column (2,                                      
                                   selectInput (inputId='ProjectPP', label='Project',
                                                choices=PJ, selected='CSET', width='100px')),
                           column(2,
                                  numericInput (inputId='FlightPP', label='Flight', value=1,
                                                min=1, max=99, step=1, width='80px')),
                           column (3,
                                   checkboxInput('AllPP', label='ALL? [takes a minute]', value=FALSE))),
                         tabsetPanel (id='whichPP', type='pills',
                                      tabPanel ('PSXC',
                                                tabsetPanel (id='whichPP2', type='pills',
                                                             tabPanel ('compare to PS_A',
                                                                       actionButton ('infoPP', label='Help info'),
                                                                       fluidRow(
                                                                         column(6, plotOutput (outputId='PSplot')),
                                                                         column(6, plotOutput (outputId='PSSplot'))
                                                                       )
                                                             ),
                                                             tabPanel ('hydrostatic-eq check',
                                                                       actionButton ('infoPHE', label='Help info'),
                                                                       fluidRow(
                                                                         column(6, plotOutput (outputId='PSHeq'))
                                                                       )
                                                             ))),
                                      tabPanel ('QCXC',
                                                actionButton ('infoQC', label='Help info'),
                                                fluidRow(
                                                  column(6, plotOutput (outputId='QCplot')),
                                                  column(6, plotOutput (outputId='QCSplot'))
                                                )
                                      ),
                                      tabPanel ('ATX',
                                                tabsetPanel (id='whichATX', type='pills',
                                                             tabPanel ('prediction from AT_A',
                                                                       actionButton ('infoAT', label='Help info'),
                                                                       fluidRow(
                                                                         column(6, plotOutput (outputId='ATplot')),
                                                                         column(6, plotOutput (outputId='ATSplot'))
                                                                       )
                                                             ),
                                                             tabPanel ('Comparison among ATs',
                                                                       sidebarLayout (
                                                                         sidebarPanel(
                                                                           selectInput ('ATsc', label='T probes; Ctrl-click to add',
                                                                                        choices=c('ATX', 'AT_A'),
                                                                                        selected='ATX', multiple=TRUE,
                                                                                        selectize=FALSE)
                                                                         ), mainPanel (
                                                                           plotOutput (outputId='ATcmpr')
                                                                         )
                                                                       )
                                                             ),
                                                             tabPanel ('hydrostatic-eq check',
                                                                       fluidRow (
                                                                         column(2, actionButton ('infoAHE', label='Help info')),
                                                                         column(4, selectInput ('ATsel', label='AT variable:', choices=c('ATX')))
                                                                       ),
                                                                       fluidRow(
                                                                         column(6, plotOutput (outputId='ATHeq'))
                                                                       )
                                                             ))),
                                      tabPanel ('INS/IRU comparisons',
                                                tabsetPanel (id='whichINS', type='pills',
                                                             tabPanel ('pitch and roll',
                                                                       fluidRow(
                                                                         column(6, plotOutput (outputId='INSpitch')),
                                                                         column(6, plotOutput (outputId='INSroll'))
                                                                       )
                                                                       ),
                                                             tabPanel ('heading',
                                                                       fluidRow (
                                                                         column(6, plotOutput (outputId='INShdg'))
                                                                       )
                                                                       )
                                                             ))
                         )),
               tabPanel ('Tools',
                         tabsetPanel (id='whichTool', type='pills',
                                      tabPanel('Add Var',
                                               actionButton ('createV', 'create new variable'),
                                               helpText (h4('Usage:'),
                                                         'Provide a name for the new variable,',
                                                         'then give a formula for calculating it. The',
                                                         'formula can use names of existing variables',
                                                         'if they are separated by spaces from other',
                                                         'text or numbers, and it can use standard',
                                                         'math operations like +-*/ or math functions',
                                                         'like sin(). An example is shown. Click the',
                                                         'top button once the formula is ready.',
                                                         h4('Warning:'), 'If the',
                                                         'variable already exists it will be replaced.'),
                                               textInput ('newvar', 'name of new variable', value='AOAREF'),
                                               textInput ('formla', 'formula for new variable',
                                                          value='PITCH - GGVSPD / TASX * 180 / pi')) , 
                                      tabPanel('lm fit',
                                               # include ('lmFit/lmFitA.html'),
                                               sidebarLayout (
                                                 sidebarPanel (h4('linear fits'),
                                                               checkboxInput ('limitsFit', 'apply restrictions?', value=TRUE),
                                                               selectInput ('response', label='response variable',
                                                                            choices=sort(VLALL)),
                                                               helpText(h4('formula use:'),
                                                                        'the fit formula can take several forms:',
                                                                        tags$ul(
                                                                          tags$li('a single variable'),
                                                                          tags$li('multiple variables separated by + signs (e.g., A+B)'),
                                                                          tags$li('additional expressions enclosed in isolating expressions ',
                                                                                  tags$strong('+I()'),', where the enclosed formula may',
                                                                                  'use operations like *, /, etc., or ^ for powers (e.g., A^2 for the square of A)'),
                                                                          tags$li('you can also use new variables previously defined by the utility \"create new variable\"')
                                                                        )),
                                                               textInput ('fformula', 'fit formula (example shown)', placeholder='I(ADIFR/QCF)+I(MACHX^3)'),
                                                               actionButton ('lfit', 'show linear fit'),
                                                               actionButton ('dfit', 'show Deming fit')
                                                 ),
                                                 mainPanel (plotOutput (outputId='fitplot'),
                                                            htmlOutput (outputId='fittext'),
                                                            tableOutput (outputId='coeftable'))
                                               )),
                                      tabPanel ('Maneuver study',
                                                tabsetPanel (id='whichMan', type='pills',
                                                             tabPanel ('Speed Run'),
                                                             tabPanel ('Pitch'),
                                                             tabPanel ('Yaw'),
                                                             tabPanel ('Circle'),
                                                             tabPanel ('Reverse Heading')
                                                             )
                                                ),
                                      tabPanel ('Calculator'),
                                      tabPanel('Other Programs'
                                               # actionButton (inputId='Ran', label = 'Start Ranadu')
                                      ))),
               tabPanel ('Review',
                         # titlePanel (tags$h1 ('Data Review')),
                         tags$script(HTML("$(function() {
                                            $(document).keyup(function(e) {
                                            if (e.which == 115) {
                                            $('#resetT').click()
                                            }
                                            });
                                            })")),
                         navlistPanel (tabPanel ('project, flight, and plot', fluidRow (
                           column (3, wellPanel (
                             selectInput (inputId='Project', label=NULL,
                                          choices=PJ, width='100px'),
                             actionButton ('reconfigure', 'save config'))
                           ),
                           column (5, wellPanel (
                             fluidRow (
                               column (4, numericInput (inputId='Flight', label='Flight', value=1,
                                                        min=1, max=99, step=1, width='80px')),
                               column (2, radioButtons ('typeFlight', label=NULL, choices=c('rf', 'tf', 'ff'),
                                                        width='70px', inline=TRUE)),
                               # column (2, checkboxInput ('Production', label='PR')),
                               column (4, numericInput (inputId='plot', label='plot', value=1,
                                                        min=1, max=49, step=1, width='80px'))))),
                           column(4, wellPanel (
                             fluidRow (
                               column (4, actionButton (inputId='savePDF', label='PDF', icon=icon('file-pdf-o'))),
                               column (4, actionButton (inputId='savePNG', label='PNG', icon=icon('file-image-o'))),
                               column (4, actionButton (inputId='saveRdata', label='R', icon=icon('file-archive-o')))
                             ),
                             fluidRow (
                               selectInput ('addVar', label=NULL,
                                            choices=c('add var',sort(FI$Variables)))))))),
                           tabPanel ('time range, restrictions', 
                                     column(6, wellPanel(
                                       sliderInput("times", label=NA, min=minT, max=maxT,
                                                   value=c(minT, maxT),
                                                   animate=TRUE,
                                                   step=step,  
                                                   timeFormat='%T', dragRange=TRUE,
                                                   timezone='+0000'))),
                                     column (1, shinyBS::bsButton ('resetT', label='rst', size='extra-small')),
                                     column (5, wellPanel ( fluidRow (
                                       column (3, numericInput ('minTAS', 'tas min', 110, width='90px')),
                                       column (3, numericInput ('maxROLL', 'roll', 5, width='90px')),
                                       column (3, numericInput ('minZ', 'Zmin-km', 2, width='90px')),
                                       column (3, numericInput ('maxROC', 'abs ROC', 5, width='90px')))
                                     ))), widths=c(3,9)),
                         
                         
                         sidebarLayout (sidebarPanel(width=3,
                                                     textOutput ('M1'),
                                                     selectInput ('Rplot', label='plot class',
                                                                  selectize=FALSE, size=14,
                                                                  choices=c('track','temperature','humidity',
                                                                            'pressure',
                                                                            'wind',
                                                                            'radiation',
                                                                            'particles',
                                                                            'skew-T',
                                                                            'potential T',
                                                                            'CDP',
                                                                            'UHSAS/PCASP',
                                                                            '2DC',
                                                                            'air chemistry',
                                                                            'extras')),
                                                     checkboxInput ('limits','apply restrictions'),
                                                     uiOutput("ui2"),
                                                     actionButton ('ncplot', 'see in ncplot'),
                                                     actionButton ('Xanadu', 'see in Xanadu'),
                                                     actionButton ('maneuvers', 'see maneuvers'),
                                                     actionButton ('manual', 'see manual')),
                                        mainPanel( tabsetPanel (tabPanel ('plot', plotOutput (outputId='display',
                                                                                              brush=brushOpts(id='plot_brush', delay=3000, delayType='debounce', resetOnNew=TRUE))),
                                                                tabPanel ('stats', dataTableOutput ('stats')),
                                                                tabPanel ('histograms', plotOutput (outputId='hist')),
                                                                tabPanel ('soundings', plotOutput (outputId='barWvsZ')),
                                                                tabPanel ('listing', dataTableOutput ('listing')))))
               ),
               tabPanel ('Known Problems',
                         tabsetPanel (id='whichKnown', type='pills',
                                      tabPanel ('DP overshoot'),
                                      tabPanel ('Supersaturation'),
                                      tabPanel ('T probe icing/wetting'),
                                      tabPanel ('Radome blockage'),
                                      tabPanel ('missing/frozen var'),
                                      tabPanel ('bad DP cavity P')
                                      )
                         ),
               tabPanel ('Special',
                         tabsetPanel (id='whichSpec', type='pills',
                                      tabPanel('KalmanFilter',
                                               tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
                                               # Application title
                                               # titlePanel("Kalman-Filter Processor"),
                                               fluidRow (
                                                 column (6, actionButton ('Run', h3("Click Here to Run the Kalman Processor"),
                                                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                               ),
                                               sidebarLayout(
                                                 sidebarPanel(h4('Run Arguments:'),
                                                              fluidRow (
                                                                column (7, selectInput (inputId='ProjectKF', label=NULL,
                                                                                        choices=PJ, selected=Project, width='100px')),
                                                                column (5, checkboxInput ('simple', label='Only Simple?', value=FALSE))),
                                                              fluidRow (
                                                                column (5, numericInput (inputId='FlightKF', label='Flight', value=Flight,
                                                                                         min=1, max=99, step=1, width='80px')),
                                                                column (3, checkboxInput ('ALL', label='ALL?',
                                                                                          value=FALSE)),
                                                                column (3, checkboxInput ('NEXT', label='Next',
                                                                                          value=FALSE))
                                                              ),
                                                              fluidRow (
                                                                column (3, checkboxInput ('newAK', label='AK?', value=newAK)),
                                                                column (3, checkboxInput ('newSS', label='SS?', value=newSS)),
                                                                column (6, numericInput ('NSTEP', label='step (s)', value=NSTEP, min=5,
                                                                                         max=60, step=1, width='80pc'))
                                                              ),
                                                              fluidRow (
                                                                column (4, checkboxInput ('genPlot', label='plots?', value=genPlot)),
                                                                column (4, numericInput ('viewPlot', label='view', value=1,
                                                                                         min=1, max=8, step=1, width='80'))
                                                              )
                                                 ),
                                                 
                                                 mainPanel(
                                                   textOutput('runPar'),
                                                   plotOutput("resultPlot")
                                                 )
                                               )
                                      ),
                                      tabPanel ('Add filtered WIF/WIX'),
                                      tabPanel ('Comp-filter AKRD'),
                                      tabPanel ('Add height-above-terrain')
                                      
                         )
               )
  )
)



# # Sidebar with a slider input for number of bins
# sidebarLayout(
#   sidebarPanel(
#     sliderInput("bins",
#                 "Number of bins:",
#                 min = 1,
#                 max = 50,
#                 value = 30)
#   ),
# 
#   # Show a plot of the generated distribution
#   mainPanel(
#     plotOutput("distPlot")
#   )
# )
# ))
