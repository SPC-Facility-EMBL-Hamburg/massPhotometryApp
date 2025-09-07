box(
  title = "Input", width = 12, solidHeader = T, status = "primary",
  fluidRow(

    column(
      6,
      p(HTML("<b>Activate calibration</b>"),
        checkboxInput("activateCalibration", "", FALSE)
      )
    ),

    conditionalPanel(
      condition = "input.activateCalibration",
      column(
        6,
        p(HTML("<b>Calibration method</b>"),
          selectInput("calibrationMethod", NULL,
                      c(
                        "File(s)"    = "calibrationFile",
                        "Parameters" = "custom"
                      )
          )
        )
      )
    ),

    conditionalPanel(
      condition = "input.activateCalibration &&
      input.calibrationMethod == 'custom'",

      column(
        6,
        p(HTML("<b>Slope*10<sup>6</sup></b>"),
          span(shiny::icon("info-circle"), id = "info_uuCalibCustom1"),
          numericInput("slopeCustom", label = NULL, -57, min = -1e6, max = 0)
        ),
        tippy::tippy_this(
          elementId = "info_uuCalibCustom1",
          tooltip = "Slope obtained by fitting a line to the observed ratiometric
          contrasts against known masses (kDa). The calibration function is
          defined as contrast = f(mass) = slope * mass + intercept.",
          placement = "right"
        )
      ),

      column(
        6,
        p(HTML("<b>Intercept*10<sup>6</sup></b>"),
          span(shiny::icon("info-circle"), id = "info_uuCalibCustom2"),
          numericInput("interceptCustom", label = NULL, 47, min = 0, max = 1e6)
        ),
        tippy::tippy_this(
          elementId = "info_uuCalibCustom2",
          tooltip = "Intercept obtained by fitting a line to the observed ratiometric
           contrasts against known masses (kDa). The calibration function is
           defined as contrast = f(mass) = slope * mass + intercept.",
          placement = "right"
        )
      )
    ),

    conditionalPanel(
      condition = "input.activateCalibration &&
      input.calibrationMethod == 'calibrationFile'",

      column(
        10,
        p(HTML("<b>MP file(s)</b>"),
          span(shiny::icon("info-circle"), id = "info_uuCalib1-1"),
          fileInput(
            "massPhotometryFileCalibration", NULL,
            accept = c(".h5", ".csv"),
            multiple = TRUE),
          tippy::tippy_this(
            elementId = "info_uuCalib1-1",
            tooltip = ".h5 (Hierarchical Data Format) or csv file(s) \nwith a 1D dataset called 'contrasts'"
          )
        )
      ),
      # Little hack to use the withBusyIndicatorUI function (loading spinner)
      column(
        1,
        p(HTML("<b><br></b>")),
        withBusyIndicatorUI(
          shinyjs::hidden(actionButton("Go2", "2. Load data!", class = "btn-primary"))
        )
      ),

      conditionalPanel(
        condition = "output.data_loadedCalibration",

        column(
          6,
          p(HTML("<b>Bin width * 10<sup>3</sup></b>"),
            span(shiny::icon("info-circle"), id = "info_uuCalib1-2"),
            numericInput("bin_widthContrast", label = NULL, 0.0004 * 1e3, min = 0, max = 20)),
          tippy::tippy_this(
            elementId = "info_uuCalib1-2",
            tooltip = "Used to group the data and build the histogram. \n",
            placement = "right"
          )
        ),

        column(
          6,
          p(HTML("<b>Max. contrast * 10<sup>3</sup></b>"),
            span(shiny::icon("info-circle"), id = "info_uu1-8-calib"),
            numericInput("max_observed_contrast", label = NULL, -2.2,
                         min = -500, max = 0,step=0.05)),

          tippy::tippy_this(
            elementId = "info_uu1-8-calib",
            tooltip = "Maximum contrast that can be detected due to instrument
            limitations. Higher values are not used for the fitting.",
            placement = "right")
        ),

        column(
          12,
          p(HTML("<b>Window range * 10<sup>3</sup></b>"),
            span(shiny::icon("info-circle"), id = "info_uuCalib1-7"),
            sliderInput("window_rangeContrast", NULL, min = -1e3, max = 0, value = c(-600, 0))
          ),
          tippy::tippy_this(
            elementId = "info_uuCalib1-7",
            tooltip = "Set the limits to build the histogram. \n",
            placement = "right"
          )
        ),

        column(
          5,
          p(HTML("<b>Slider left limit</b>"),
            span(shiny::icon("info-circle"), id = "info_uuCalib1-10"),
            numericInput("leftLimitWindowRangeContrast", label = NULL, -600, min = -1e6, max = 0)),
          tippy::tippy_this(
            elementId = "info_uuCalib1-10",
            tooltip = "Set the left limit for the window range slider. \nChanging this value will automatically update the selected window range.",
            placement = "right"
          )
        ),

        column(
          5,
          p(HTML("<b>Slider right limit</b>"),
            span(shiny::icon("info-circle"), id = "info_uuCalib1-11"),
            numericInput("rightLimitWindowRangeContrast", label = NULL, 0, min = 0, max = 1e6)
          ),
          tippy::tippy_this(
            elementId = "info_uuCalib1-11",
            tooltip = "Set the right limit for the window range slider. \nChanging this value will automatically update the selected window range.",
            placement = "right"
          )
        ),

        column(
          6,
          p(HTML("<b>Fit baseline</b>"),
            span(shiny::icon("info-circle"), id = "info_uu1-baseline-calib"),
            checkboxInput("fit_baseline_calib", label = NULL, value = FALSE)
          ),
          tippy::tippy_this(
            elementId = "info_uu1-baseline-calib",
            tooltip = "Useful when there is a constant background noise.", placement = "right"
          )
        ),

        column(
          6,
          p(HTML('<p style="margin-bottom:0px;"><br></p>'),
            actionButton(
                inputId = "triggerFittingCalib", label = "Run fitting!",
                icon("meteor"),
                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
          )
        ),

        column(12, rHandsontableOutput('initialPeakGuessesTableCalib',width = "100%"))

      )
    )
  )
)
