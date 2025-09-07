box(title = "Input", width = 12, solidHeader = T, status = "primary",
    fluidRow(
      
      column(6, p(HTML("<b>MP file(s)</b>"),
                   span(shiny::icon("info-circle"), id = "info_uu1-1"),
                   fileInput("massPhotometryFile", NULL,accept = c(".h5",".csv"),multiple=TRUE),
                   tippy::tippy_this(elementId = "info_uu1-1",
                                     tooltip = ".h5 (Hierarchical Data Format) file with a 1D dataset called 'masses_kDa'.
                                     You can load many files simultaneously."))),

      column(6, p(HTML('<p style="margin-bottom:0px;"><br></p>'),
                   actionButton("GoLoadExample","Load example",class = "btn-primary")))
    ),

    fluidRow(
      
      conditionalPanel(condition = "output.dataLoaded", 
      
        column(5, p(HTML("<b>Bin width</b>"),
               span(shiny::icon("info-circle"), id = "info_uu1-2"),
               
               numericInput("bin_width", label = NULL, 6, min = 1, max = 20)),
               tippy::tippy_this(elementId = "info_uu1-2",
                                 tooltip = "In kDa. Used to group the data and build the histogram.",placement = "right")),
        
        column(6, p(HTML("<b>Min. observed mass</b>"),
               span(shiny::icon("info-circle"), id = "info_uu1-8"),
               
               numericInput("min_observed_mass", label = NULL, 30, min = -500, max = 500)),
               tippy::tippy_this(elementId = "info_uu1-8",
                                 tooltip = "Minimum mass (kDa) that could be theoretically observed.
                                 Lower (absolute) values are not used for the fitting.",placement = "right")),

        column(6, p(HTML("<b>Fit baseline</b>"),
                    span(shiny::icon("info-circle"), id = "info_uu1-9"),
                    checkboxInput("fit_baseline", label = NULL, value = FALSE)),
               tippy::tippy_this(elementId = "info_uu1-9",
                                 tooltip = "Useful when there is a constant background noise.
                                 More info in the User guide.",placement = "right")),

              column(6, p(HTML('<p style="margin-bottom:0px;"><br></p>'),
                  actionButton(
                    inputId = "run_fitting",label = "Run fitting!",
                    icon("jedi"),
                    style="color: #fff; background-color: #337ab7;
               border-color: #2e6da4"))),

        column(8, p(HTML("<b>Window range</b>"),
                    span(shiny::icon("info-circle"), id = "info_uu1-7"),
                    
                    sliderInput("window_range", NULL,min = 0, max = 0,value = c(0,0))),
               tippy::tippy_this(elementId = "info_uu1-7",
                                 tooltip = "Set the limits to build the histogram. Units are kDa.",
                                 placement = "right")),
        
        column(6, p(HTML("<b>Slider left limit</b>"),
                    span(shiny::icon("info-circle"), id = "info_uu1-10"),
                    
                    numericInput("leftLimitWindowRange", label = NULL, value=0, min = -1e6, max = 0)),
               tippy::tippy_this(elementId = "info_uu1-10",
                                 tooltip = "Set the left limit for the window range slider. 
                                 Changing this value will automatically update the selected window range.",placement = "right")),
        
        column(6, p(HTML("<b>Slider right limit</b>"),
                    span(shiny::icon("info-circle"), id = "info_uu1-11"),
                    
                    numericInput("rightLimitWindowRange", label = NULL, value=0, min = 0, max = 1e6)),
               tippy::tippy_this(elementId = "info_uu1-11",
                                 tooltip = "Set the right limit for the window range slider. 
                                 Changing this value will automatically update the selected window range.",placement = "right")),

        #Rhandsontable for the initial peak guesses
        column(12, rHandsontableOutput('initialPeakGuessesTable',width = "100%"))

      )   
    )
)


