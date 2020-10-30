
# This is the user-interface definition for the QAtools Shiny app.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


ui <- fluidPage (
  # Application title
  titlePanel("QA / QC Tools"),
  tabsetPanel (id='whichTab', type='pills',
    tabPanel ('Guide',
      includeHTML ('HTML/Information.html')),
    tabPanel ('Background',
      helpText('Suggestion: start with the "Review" tab for all plots,',
        'then "Past Projects" tab, then review "Known Problems".',
        'The next tabs provide guidance for some specific variables.'),
      tabsetPanel (id='whichBg', type='pills',
        tabPanel ('Suggested checks',
          tabsetPanel (id='whichsugg', type='pills',
            tabPanel ('PSXC/QCXC',
              includeHTML('HTML/SuggestionPQ.html')
            ),
            tabPanel ('ATX',
              includeHTML('HTML/SuggestionATX.html')
            ),
            tabPanel ('Humidity',
              includeHTML('HTML/SuggestionHumidity.html')
            ),
            tabPanel ('INS performance',
              includeHTML('HTML/SuggestionINS.html')
            ),
            tabPanel ('WIC',
              includeHTML('HTML/SuggestionWIC.html')
            ),
            tabPanel ('WD/WS',
              includeHTML('HTML/SuggestionWDWS.html')
            ),
            tabPanel ('LWC and particles',
              includeHTML('HTML/SuggestionLWC.html')
            ),
            tabPanel ('...')
          )
        ),
        # tabPanel ('R sessions (tutorial)',
        #   tabsetPanel (id='whichTab', type='pills',
        #     tabPanel ('TOC',
        #       includeHTML('../RSessions/RSessions/TOC/TOC.html')),
        #     tabPanel ('Getting Started',
        #       tabsetPanel (id='S1tab', type='pills',
        #         tabPanel ('Getting Started',
        #           includeHTML('../RSessions/RSessions/Session1/Session1a.html')),
        #         tabPanel ('RStudio Tour',
        #           includeHTML('../RSessions/RSessions/Session1/Session1b.html')),
        #         tabPanel ('Some Examples',
        #           includeHTML('../RSessions/RSessions/Session1/Session1c.html'),
        #           tabsetPanel (id='S1ex', type='pills',
        #             tabPanel ('simple plot',
        #               includeHTML ('../RSessions/RSessions/Session1/E1Code.html'),
        #               sidebarLayout(
        #                 sidebarPanel(
        #                   selectInput (inputId='S1Var', label='variable to plot', 
        #                     choices=c('Temperature'='ATX',
        #                       'Wind Speed'='WSC',
        #                       'Pressure'='PSXC'))
        #                 ),
        #                 mainPanel(
        #                   plotOutput ('S1E1Plot')
        #                 )
        #               )),
        #             tabPanel ('sounding',
        #               includeHTML ('../RSessions/RSessions/Session1/E2Code.html'),
        #               plotOutput ('S1E2Plot', width="50%")),
        #             tabPanel ('stats',
        #               includeHTML ('../RSessions/RSessions/Session1/E3Code.html'),
        #               dataTableOutput ('S1Stats')),
        #             tabPanel ('recovery factor',
        #               includeHTML ('../RSessions/RSessions/Session1/E4Code.html'))
        #           )
        #         ),
        #         tabPanel ('Text-with-Code',
        #           includeHTML ('../RSessions/RSessions/Session1/Session1d.html')),
        #         tabPanel ('Getting Ranadu',
        #           includeHTML ('../RSessions/RSessions/Session1/Session1e.html')))),
        #     tabPanel ('Objects and the data.frame',
        #       tabsetPanel (id='S2tab', type='pills',
        #         tabPanel ('Vectors and Matrices',
        #           includeHTML('../RSessions/RSessions/Session2/Session2a.html'),
        #           htmlOutput('txtS2a'),
        #           radioButtons ('selS2a', label=NULL, choices=c(
        #             'select a button below'=1,
        #             'a <- 1:12; print(a)'=2,
        #             'dim(a) <- c(3,4); print(a)'=3,
        #             'print (t(a))'=4
        #           ), width='400px')
        #         ),
        #         tabPanel ('The data.frame',
        #           includeHTML('../RSessions/RSessions/Session2/Session2b.html')),
        #         tabPanel ('Addressing and Subsetting data.frames',
        #           includeHTML('../RSessions/RSessions/Session2/Session2c1.html'),
        #           radioButtons ('selS2c1', label=NULL, choices=c(
        #             'select a button below'=1,
        #             'Data$ATX[5]'=2, 
        #             'Data[5, 2]'=3,
        #             'Data[5, ]'=4, 
        #             'Data[5, "ATX"]'=5, 
        #             'Data$ATX'=6,
        #             'attach(Data); ATX[5]'=7,
        #             'with(Data, print(ATX[5])'=8
        #           ), width='800px'),
        #           htmlOutput ('txtS2c1'),
        #           includeHTML('../RSessions/RSessions/Session2/Session2c2.html'),
        #           actionButton ('XS2a', label='See an answer')
        #         ),
        #         tabPanel ('Some Basic Operations',
        #           includeHTML ('../RSessions/RSessions/Session2/Session2d.html')))),
        #     tabPanel ('Basics',
        #       tabsetPanel (id='S3tab', type='pills',
        #         tabPanel ('R as a Calculator',
        #           fluidRow (
        #             column (3, numericInput ('S3aframe', label='frame #',
        #               2, min=1, max=3, step=1)),
        #             column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
        #               'use the up/down arrow keys to step through the frames.'))),
        #           imageOutput('RS3apng')
        #         ),
        #         tabPanel ('Basic Operators and Precedence',
        #           fluidRow (
        #             column (3, numericInput ('S3bframe', label='frame #',4,
        #               min=4, max=12, step=1)),
        #             column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
        #               'use the up/down arrow keys to step through the frames.'))),
        #           imageOutput('RS3bpng')
        #         ),
        #         tabPanel ('Vectorized Operations',
        #           fluidRow (
        #             column (3, numericInput ('S3cframe', label='frame #',13,
        #               min=13, max=16, step=1)),
        #             column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
        #               'use the up/down arrow keys to step through the frames.'))),
        #           imageOutput('RS3cpng')
        #         ),
        #         tabPanel ('Using Variables',
        #           fluidRow (
        #             column (3, numericInput ('S3dframe', label='frame #',17,
        #               min=17, max=21, step=1)),
        #             column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
        #               'use the up/down arrow keys to step through the frames.'))),
        #           imageOutput('RS3dpng')
        #         )
        #       )
        #     ),
        #     tabPanel ('R packages',
        #       tabsetPanel (id='S4tab', type='pills',
        #         tabPanel ('All',
        #           fluidRow (
        #             column (3, numericInput ('S4frame', label='frame #',2,
        #               min=1, max=11, step=1)),
        #             column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
        #               'use the up/down arrow keys to step through the frames.'))),
        #           imageOutput('RS4png')
        #         ))),
        #     tabPanel ('Plotting',
        #       tabsetPanel (id='S5tab', type='pills',
        #         tabPanel ('All',
        #           fluidRow (
        #             column (3, numericInput ('S5frame', label='frame #',2,
        #               min=1, max=15, step=1)),
        #             column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
        #               'use the up/down arrow keys to step through the frames.'))),
        #           imageOutput('RS5png')
        #         ),
        #         tabPanel ('Base Graphics',
        #           fluidRow (
        #             column (3, numericInput ('S5aframe', label='frame #',3,
        #               min=3, max=9, step=1)),
        #             column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
        #               'use the up/down arrow keys to step through the frames.'))),
        #           imageOutput('RS5apng')
        #         ),
        #         tabPanel ('plotWAC',
        #           fluidRow (
        #             column (3, numericInput ('S5bframe', label='frame #',8,
        #               min=8, max=9, step=1)),
        #             column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
        #               'use the up/down arrow keys to step through the frames.'))),
        #           imageOutput('RS5bpng')
        #         ),
        #         tabPanel ('ggplot',
        #           fluidRow (
        #             column (3, numericInput ('S5cframe', label='frame #',10,
        #               min=10, max=15, step=1)),
        #             column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
        #               'use the up/down arrow keys to step through the frames.'))),
        #           imageOutput('RS5cpng')
        #         )
        #       )
        #     ),
        #     tabPanel ('Fitting',
        #       tabsetPanel (id='S6tab', type='pills',
        #         tabPanel ('All',
        #           fluidRow (
        #             column (3, numericInput ('S6frame', label='frame #',2,
        #               min=1, max=21, step=1)),
        #             column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
        #               'use the up/down arrow keys to step through the frames.'))),
        #           imageOutput('RS6png')
        #         ))
        #     ),
        #     tabPanel ('Reproducible Research',
        #       tabsetPanel (id='S7tab', type='pills',
        #         tabPanel ('All',
        #           fluidRow (
        #             column (3, numericInput ('S7frame', label='frame #',2,
        #               min=1, max=13, step=1)),
        #             column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
        #               'use the up/down arrow keys to step through the frames.'))),
        #           imageOutput('RS7png')
        #         ))
        #     ),
        #     tabPanel ('Data Review',
        #       tabsetPanel (id='S8tab', type='pills',
        #         tabPanel ('All',
        #           fluidRow (
        #             column (3, numericInput ('S8frame', label='frame #',2,
        #               min=1, max=13, step=1)),
        #             column (4, helpText ('Suggestion: Click cursor in "frame #" entry box',
        #               'use the up/down arrow keys to step through the frames.'))),
        #           imageOutput('RS8png')
        #         ))
        #     ),
        #     tabPanel ('Shiny apps',
        #       tabsetPanel (id='S9tab', type='pills',
        #         tabPanel ('All',
        #           fluidRow (
        #             column (6, helpText ('Suggestion: Use fit-to-page button, bottom right'))),
        #           tags$iframe(style="height:800px; width:100%; scrolling=yes", 
        #             src="Session9.pdf")
        #           # imageOutput('RS9png')
        #         ))
        #     )
        #   )
        # ),
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
            choices=PJ, width='140px')),
        column(2,
          numericInput (inputId='FlightPP', label='Flight', value=1,
            min=1, max=99, step=1, width='80px')),
        column (2, radioButtons ('typeFlightPP', label=NULL, choices=c('rf', 'tf', 'ff'),
          width='70px', inline=FALSE)),
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
          )),
        tabPanel ('Maneuver study',
          tabsetPanel (id='whichMan', type='pills',
            tabPanel ('search for maneuvers',
              # selectInput (inputId='ProjectM', label='Project',
              #              choices=PJ, selected='CSET', width='100px'),
              checkboxGroupInput ('manS', 'types of maneuver', choices=c('speed run', 'pitch', 'yaw', 'circle', 'reverse heading'), 
                selected=c('speed run', 'pitch', 'yaw', 'circle', 'reverse heading'), inline=TRUE),
              actionButton ('Tmaneuvers', 'Search'),
              bsModal ('warnSearch', 'Warning', 'Tmaneuvers', includeHTML('HTML/warnSearch.html'),
                actionButton("yes_button", "Yes"),
                actionButton("no_button", "No"))
            ),
            tabPanel ('Speed Run',
              sidebarLayout (
                sidebarPanel (h4('select speed run'),
                  radioButtons('selSR', label=NULL, choices=c('none'='0')),
                  fluidRow (
                    column(8, selectInput('plotTypeSR', label='type of plot', 
                      choices=CHP)),
                    column(4, actionButton('infoSR', label='Info'))
                  ),
                  sliderInput('sliderSR', label='set delay [ms]', min=-5000, max=5000, step=50, value=0),
                  selectInput('varSR', label='other variable (type-3 plot)', 
                    choices=sort(VLALL), selected='RTX')
                ),
                mainPanel(
                  plotOutput (outputId='plotSR')
                )
              )
            ),
            tabPanel ('Pitch',
              sidebarLayout (
                sidebarPanel (h4('select pitch maneuver'),
                  radioButtons('selPM', label=NULL, choices=c('none'='0')),
                  fluidRow (
                    # column(8, selectInput('plotTypePM', label='type of plot', 
                    #                      choices=CHP)),
                    column(4, actionButton('infoPM', label='Info')),
                    column(6, selectInput('setPMT', label='set time for:',
                      choices=c('pitches', 'environment'),
                      selected='environment'))
                  ),
                  sliderInput('sliderPM', label='set interval', min=minT, max=maxT, 
                    step=10, value=c(minT, maxT),
                    timeFormat='%T',
                    timezone='+0000'
                  ),
                  sliderInput('sliderPitchPM', label='PITCH delay [ms]', min=-300, max=300,
                    step=10, value=0),
                  sliderInput('sliderROCPM', label='ROC delay [ms]', min=-200, max=200,
                    step=10, value=0)
                  
                ),
                mainPanel(
                  plotOutput (outputId='plotPM',
                    brush=brushOpts(id='plot2_brush', delay=3000, delayType='debounce', resetOnNew=TRUE))
                )
              )
            ),
            tabPanel ('Yaw',
              sidebarLayout (
                sidebarPanel (h4('select yaw maneuver'),
                  fluidRow (
                    column(6, radioButtons('selYM', label=NULL, choices=c('none'='0'))),
                    column(6, actionButton ('delYM', label='delete it'))),
                  fluidRow (
                    column(4, actionButton('infoYM', label='Info')),
                    column(6, selectInput('setYMT', label='set time for:',
                      choices=c('environment', 'yaws'),
                      selected='environment'))
                  ),
                  sliderInput('sliderYM', label='set interval', min=minT, max=maxT, 
                    step=10, value=c(minT, maxT),
                    timeFormat='%T',
                    timezone='+0000'
                  ),
                  sliderInput('sliderTHDGYM', label='THDG delay [ms]', min=-300, max=300,
                    step=10, value=0)
                  
                ),
                mainPanel(
                  plotOutput (outputId='plotYM',
                    brush=brushOpts(id='plot3_brush', delay=3000, delayType='debounce', resetOnNew=TRUE))
                )
              )
            ),
            tabPanel ('Circle',
              sidebarLayout (
                sidebarPanel (h4('select circle maneuver'),
                  radioButtons('selCR', label=NULL, choices=c('none'='0')),
                  fluidRow (
                    column(2, actionButton('infoCR', label='Info')),
                    column(3, selectInput('plotTypeCR', label='plot',
                      choices=c('track', 'WS fit', 'SSRD offset'),
                      selected='track')),
                    # column(3, selectInput('setCRT', label='set time for:',
                    #   choices=c('leg 1', 'leg 2'),
                    #   selected='leg 1')),
                    column(2, actionButton('saveCR', label='save\ntimes')),
                    column(2, actionButton('delCR', label='delete'))
                  ),
                  sliderInput('sliderCR', label='set interval', min=minT, max=maxT, 
                    step=5, value=c(minT, maxT),
                    timeFormat='%T',
                    timezone='+0000'
                  ) #,
                  # sliderInput('sliderCRSS', label='SSRD offset [deg]', min=-0.2, max=0.2,
                  #   step=0.01, value=0)
                  
                ),
                mainPanel(
                  plotOutput (outputId='plotCR', height='600px',
                    brush=brushOpts(id='plot5_brush', delay=3000, delayType='debounce', resetOnNew=TRUE))
                )
              )
            ),
            tabPanel ('Reverse Heading',
              sidebarLayout (
                sidebarPanel (h4('select reverse-heading maneuver'),
                  radioButtons('selRH', label=NULL, choices=c('none'='0')),
                  fluidRow (
                    column(2, actionButton('infoRH', label='Info')),
                    column(3, selectInput('plotTypeRH', label='plot',
                      choices=c('track', 'wind', 'reverse time'),
                      selected='track')),
                    column(3, selectInput('setRHT', label='set time for:',
                      choices=c('leg 1', 'leg 2'),
                      selected='leg 1')),
                    column(2, actionButton('saveRH', label='save\ntimes')),
                    column(2, actionButton('delRH', label='delete'))
                  ),
                  sliderInput('sliderRH', label='set interval', min=minT, max=maxT, 
                    step=5, value=c(minT, maxT),
                    timeFormat='%T',
                    timezone='+0000'
                  ),
                  sliderInput('sliderRHSS', label='SSRD offset [deg]', min=-0.2, max=0.2,
                    step=0.01, value=0)
                  
                ),
                mainPanel(
                  plotOutput (outputId='plotRH', height='600px',
                    brush=brushOpts(id='plot4_brush', delay=3000, delayType='debounce', resetOnNew=TRUE))
                )
              )
            )
          )
        ))),
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
        tabPanel ('Calculator',
          fluidRow (
            column(6,textInput ('cformula', 'R expression', placeholder='sin(28*pi/180)')),
            column(6, helpText(h4('DataRef as defined in the "Review" tab is available:',
              'Select a project and flight there, then try',
              'formatTime(DataRef$Time[1])',
              'DataRef$ATX[nrow(DataRef)/2]',
              'etc., here.')))),
          htmlOutput ('txtCalc1')
        ),
        # tabPanel ('Basic time-series plot',
        #           selectInput ('bplotVar', label='Variable(s)',
        #                        choices='ATX')
        #           
        #           ),
        tabPanel('Other Programs',
          tabsetPanel (id='whichOtherP', type='pills',
            tabPanel ('start ncplot'),
            tabPanel ('Basic time-series plot'),
            tabPanel ('Basic size-distribution'),
            tabPanel ('Ranadu',
              includeHTML('HTML/Ranadu.html')
            )
            # actionButton (inputId='Ran', label = 'Start Ranadu')
          )))),
    tabPanel ('Review',
      # titlePanel (tags$h1 ('Data Review')),
      # On some PCs, must use Fn F4
      tags$script(HTML("$(function() {
                                            $(document).keyup(function(e) {
                                            if (e.which == 115) {
                                            $('#resetT').click()
                                            }
                                            });
                                            })")),
      navlistPanel (tabPanel ('project, flight, and plot', fluidRow (
        column (3, wellPanel (
          fluidRow (
            column (6, selectInput (inputId='Project', label=NULL,
              choices=PJ, selected = PJ[6], width='140px')),
            column (4, checkboxInput ('HR', label='25Hz'))),
          actionButton ('reconfigure', 'save config'))
        ),
        column (5, wellPanel (
          fluidRow (
            column (3, numericInput (inputId='Flight', label='Flight', value=1,
              min=1, max=99, step=1, width='80px')),
            column (2, radioButtons ('typeFlight', label=NULL, choices=c('rf', 'tf', 'ff'),
              width='70px', inline=FALSE)),
            # column (2, checkboxInput ('Production', label='PR')),
            column (3, numericInput (inputId='plot', label='plot', value=1,
              min=1, max=49, step=1, width='80px')),
            column(3, wellPanel (
              actionButton ('qcheck', label='quick', icon=icon('file-image-o'), width='80px')
              ))))),
        column(4, wellPanel (
          fluidRow (
            column (4, actionButton (inputId='savePDF', label='PDF', icon=icon('file-pdf-o'))), #, onclick="window.open('latestPlots.pdf')")),
            column (4, actionButton (inputId='savePNG', label='PNG', icon=icon('file-image-o'))),
            column (4, actionButton (inputId='saveRdata', label='R', icon=icon('file-archive-o')))
          ),
          # fluidRow (
          #   actionButton("vpdf", "show", onclick = "window.open('latestPlots.pdf')")
          # ),
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
      bsModal("quickCheck", title=NULL, trigger='qcheck', size = "large", plotOutput("quickPlot")),
      
      
      
      sidebarLayout ( sidebarPanel(width=3, 
        # textOutput ('M1'),
        selectInput ('Rplot', label='plot class',
          selectize = FALSE, size=length(PlotTypes),
          choices=PlotTypes),
        fluidRow (
          column(6, checkboxInput ('limits','apply restrictions')),
          column(6, checkboxInput('ybrush', 'brush for ordinate?', value=FALSE))
        ),
        uiOutput("ui2"),
        actionButton ('ncplot', 'see in ncplot'),
        actionButton ('Xanadu', 'see in Xanadu'),
        actionButton ('maneuvers', 'see maneuvers'),
        actionButton ('manual', 'see manual')),
        mainPanel( tabsetPanel (tabPanel ('plot', 
          verticalLayout(
            plotOutput (outputId='display', 
              dblclick = "plot_dblclick", 
              brush=brushOpts(id='panel_brush', delay=1000, delayType='debounce',
                resetOnNew=TRUE), inline=TRUE),
            plotOutput (outputId='display2', 
              dblclick = "plot_dblclick", 
              brush=brushOpts(id='panel_brush', delay=1000, delayType='debounce',
                resetOnNew=TRUE),inline=TRUE), #,
            plotOutput (outputId='display3',
              dblclick = "plot_dblclick", 
              brush=brushOpts(id='panel_brush', delay=1000, delayType='debounce',
                resetOnNew=TRUE), inline=TRUE),
            plotOutput (outputId='display4',
              dblclick = "plot_dblclick", 
              brush=brushOpts(id='panel_brush', delay=1000, delayType='debounce',
                resetOnNew=TRUE), inline=TRUE)
            )
          ),
          tabPanel ('stats', dataTableOutput ('stats')),
          tabPanel ('histograms', plotOutput (outputId='hist')),
          tabPanel ('soundings', plotOutput (outputId='barWvsZ')),
          tabPanel ('listing', dataTableOutput ('listing')))))
    ),
    tabPanel ('Known Problems',
      fluidRow (
        column (2,                                      
          selectInput (inputId='ProjectKP', label='Project',
            choices=PJ, selected='ORCAS', width='140px')),
        column(2,
          numericInput (inputId='FlightKP', label='Flight', value=3,
            min=1, max=99, step=1, width='80px')),
        column (2, radioButtons ('typeFlightKP', label=NULL, choices=c('rf', 'tf', 'ff'),
          width='70px', inline=FALSE))
        # column(2, checkboxInput('KPtf', 'test flight', value=FALSE))
      ),                         
      tabsetPanel (id='whichKnown', type='pills',
        tabPanel ('DP overshoot/SS',
          fluidRow(
            column (1, actionButton ('infoTDP', label='Info')),
            column (1, actionButton ('searchTDP', label='search')),
            column (2, actionButton ('addVXL', label='add VSCEL prediction')),
            column (2, actionButton ('accAllTDP', label='accept all')),
            column (1, actionButton ('acceptTDP', label='accept')),
            column (1, actionButton ('rejectTDP', label='reject')),
            column (1, actionButton ('newTDP', label='define new')),
            column (1, actionButton ('nextTDP', label='next')),
            column (1, actionButton ('autoFlag', label='auto Flag')),
            column (1, actionButton ('saveTDP', label='save'))
          ),
          # Sidebar with slider inputs for display interval and flag interval
          sidebarLayout(
            sidebarPanel(
              fluidRow(
                column(8, selectInput ('sliderChoiceTDP', 'brush affects which slider?',
                choices=c('display time range', 'data-quality-flag time range'),
                selected='data-quality-flag time range')),
                column(4, actionButton ('resetSliderAB', label='reset this slider'))
              ),
              sliderInput("timesTDP",
                "time range:",
                min = minT,
                max = maxT, step=60,
                timeFormat='%T',
                timezone='+0000',
                value = c(minT, maxT),
                dragRange=TRUE),
              sliderInput("dqftimes",
                "data-quality-flag time range:",
                min = minT,
                max = maxT,
                timeFormat='%T',
                timezone='+0000', step=1, round=TRUE,
                value = c(minT, maxT)),
              radioButtons ('overshoot', label='list of overshooting candidates',
                choices='none')
            ),
            
            # Show a plot of the dewpoint measurements
            mainPanel(
              plotOutput("dewpointPlot", brush=brushOpts(id='plot_brushTDP', delay=3000, delayType='debounce', resetOnNew=TRUE))
            )
          )
        ),
        tabPanel ('in-cloud check',
          fluidRow (
            column(2, actionButton ('infoIC', label='Info')),
            column(3, numericInput('concd', label='CONCD', value=10, min=10, max=1000, step=10)),
            column(3, numericInput('lwcd', label='PLWCD', value=0.1, min=0.05, max=2, step=0.05)),
            column(2, numericInput('csec', label='seconds', value=2, min=1, max=20, step=1)),
            column(2, numericInput('sigIC', label='sigma to remove', value=3, step=1))
          ),
          fluidRow(
            column (1, actionButton ('resetTIC', label='reset time')),
            column(1, actionButton ('resetIC', label='reset flag')),
            column (1, actionButton ('prevIC', label='prev')),
            column(1, actionButton ('nextIC', label='next')),
            column(2, actionButton ('setBadIC', label='flag interval as bad')),
            column(2, actionButton ('saveIC', label='save reject intervals'))
          ),
          # Sidebar with slider inputs for display interval and flag interval
          sidebarLayout(
            sidebarPanel(
              sliderInput("timesIC",
                "time range:",
                min = minT,
                max = maxT, step=60,
                timeFormat='%F %T',
                timezone='+0000',
                value = c(minT, maxT),
                dragRange=TRUE),
              fluidRow (
                column(6, selectInput ('pltIC', label='plot type', choices=c('scatterplot', 'histogram'),
                  selected='scatterplot')),
                column(6, selectInput ('pvarIC', label='plot variable', choices=c('CONCD', 'PLWCD', 
                  'WIC', 'ATX/DPXC'),
                  selected='CONCD'))
              ),
              checkboxInput('allIC', label='All flights for this project'),
              radioButtons ('inCloud', label='list of rejected times',
                choices=chIC)
            ),
            
            # Show a plot of the measurements
            mainPanel(
              plotOutput("inCloudPlot"),
              plotOutput("ICtimePlot", brush=brushOpts(id='plot_brushIC', delay=3000, delayType='debounce', resetOnNew=TRUE))
            )
          )),
        tabPanel ('out-of-range, frozen, spikes',
          fluidRow(
            column (1, actionButton ('infoFrozen', label='INFO')),
            column(3, numericInput(inputId='nvFrozen', label='raw variable', value=1, min=1, max=nrow(frozenRange))),
            column(3, numericInput('minFrozen', label='min', value=frozenRange$vlow[1])),
            column(3, numericInput('maxFrozen', label='max', value=frozenRange$vhigh[1]))),
          fluidRow(
            column(3, textInput('varFrozen', label='', value=frozenRange$Var[1])),
            column(6, textInput('lnameFrozen', label='', value=frozenRange$lname[1], width='100%')),
            column(1, checkboxInput('rawFrozen', label='house-\nkeeping', value=TRUE)),
            column(2, checkboxInput('keepFrozen', label='keep?', value=TRUE))),
          fluidRow(
            column (1, actionButton ('resetTFrozen', label='reset time')),
            column(1, actionButton ('searchFrozen', label='search')),
            column(2, actionButton ('setBadFrozen', label='flag interval as bad')),
            column(2, actionButton ('autoFrozen', label='add list to BAD')),
            column(2, actionButton ('autoFrozen2', label='add all lists to BAD')),
            column(2, actionButton ('saveFrozen', label='save limits')),
            column(2, actionButton ('saveFrozen2', label='save BAD'))
          ),
          # Sidebar with slider inputs for display interval and flag interval
          sidebarLayout(
            sidebarPanel(
              sliderInput("timesFrozen",
                "time range:",
                min = minT,
                max = maxT, step=60,
                timeFormat='%F %T',
                timezone='+0000',
                value = c(minT, maxT),
                dragRange=TRUE),
              fluidRow (
                column(6, selectInput ('pltFrozen', label='plot type', choices=c('vs time', 'histogram'),
                  selected='vs time')),
                column(6, selectInput ('whichFrozen', label='list type', 
                  choices=c('out-of-range', 'frozen', 'spikes', 'jumps'),
                  selected='frozen'))
              ),
              radioButtons ('QFrozen', label='list to review',
                choices=chFrozen)
            ),
            
            # Show a plot of the measurements
            mainPanel(
              fluidRow (
                column (3, checkboxInput ('useHFrozen', label='include housekeeping\nin searches?', value=FALSE)),
                column (3, numericInput ('secFrozen', label='seconds for frozen', value=60)),
                column (3, numericInput ('sdFrozen', label='sd for spike', value=6)),
                column (3, numericInput ('pcntFrozen', label='% range for jump', value=20))
              ),
              plotOutput("frozenTimePlot", brush=brushOpts(id='plot_brushFrozen', delay=3000, delayType='debounce', resetOnNew=TRUE)),
              fluidRow (
                column(6, checkboxGroupInput ('badFrozen', label='list considered bad',
                  choices=chBAD, width='100%')),
                column(6, htmlOutput (outputId='frozenList')))
              
            ))),
        
        tabPanel ('check DP cavity P',
          sidebarLayout(
            sidebarPanel(
              selectInput ('cavType', 'type of plot', choices=c('scatterplot', 'time series', 'histogram')),
              includeHTML ('HTML/cavInfo.html')
            ),
            mainPanel(
              plotOutput('cavPlot'),
              includeHTML ('HTML/cavInfo2.html')
            )
          )
        )
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
        tabPanel ('Options for Adding Wind Variables',
          tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
          fluidRow (
            column (6, actionButton ('RunWIF', 
              h3("Click Here to Run the Wind-Options Processor"),
              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          ),
          sidebarLayout(
            sidebarPanel(h4('Run Arguments:'),
              fluidRow (
                column (7, selectInput (inputId='ProjectWIF', label=NULL,
                  choices=PJ, width='100px'))
              ),
              fluidRow (
                column (5, numericInput (inputId='FlightWIF', label='Flight', value=Flight,
                  min=1, max=99, step=1, width='80px')),
                column (3, checkboxInput ('ALLWIF', label='ALL?',
                  value=FALSE)),
                column (3, checkboxInput ('NEXTWIF', label='Next',
                  value=FALSE))
              ),
              fluidRow (
                checkboxGroupInput ('choiceADDW',
                  label='processing to add',
                  choices=c('AKY/WIY', 'GustPod', 'pitot-static', 'ROC'), 
                  selected=c('AKY/WIY', 'GustPod', 'pitot-static', 'ROC'), inline=TRUE)
              ),
              fluidRow (
                column (8, checkboxGroupInput ('choicesWIF', 
                  label='variables to plot', 
                  choices=c('WIY', 'WIF', 'WIC', 'WIG', 'AKRD', 'AKY', 'WDC', 'WDG', 'WDTC', 'WSC', 'WSG', 'WSTC', 'ROC', 'GGVSPD'),
                  inline=TRUE)),
                column (4, selectInput ('viewPlotWIF', label='select plot',
                  choices=c('time series', 'histogram')))
              )
            ),
            
            mainPanel(
              textOutput('runParWIF'),
              plotOutput("resultPlotWIF", height='550px'),
              includeHTML ('HTML/WIoptions.html')
            )
          )
          
        ),
        ##########
        tabPanel ('Add height-above-terrain',
          ##########
          tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
          fluidRow (
            column (6, actionButton ('RunHOT', h3("Click Here to Run the HOT Processor"),
              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          ),
          sidebarLayout(
            sidebarPanel(h4('Run Arguments:'),
              fluidRow (
                column (7, selectInput (inputId='ProjectHOT', label=NULL,
                  choices=PJ, selected=Project, width='100px'))
              ),
              fluidRow (
                column (5, numericInput (inputId='FlightHOT', label='Flight', value=Flight,
                  min=1, max=99, step=1, width='80px')),
                column (3, checkboxInput ('ALLHOT', label='ALL?',
                  value=FALSE)),
                column (3, checkboxInput ('NEXTHOT', label='Next',
                  value=FALSE))
              ),
              fluidRow (
                # column (4, checkboxInput ('genPlotHOT', label='select plots', 
                # value=genPlot)),
                column (6, selectInput ('viewPlotHOT', label='select plot',
                  choices=c('flight track', 'time-height')))
              )
            ),
            
            mainPanel(
              textOutput('runParHOT'),
              plotOutput("resultPlotHOT", height='550px'),
              includeHTML ('HTML/HeightOfTerrain.html')
            )
          )
          
        )
        ##########
        
      )
    )
  )
)




