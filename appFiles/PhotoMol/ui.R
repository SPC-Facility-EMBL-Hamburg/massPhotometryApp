source("ui_files/theme.R")
source("ui_files/logo.R")
source("ui_files/busy_indicator.R")

shinyUI(
  dashboardPage(
    title = paste0(appName),

    dashboardHeader(
      title = logo_grey_light, titleWidth = 200), #logo_grey_light is described in logo.R

    dashboardSidebar(
      collapsed = F,width = 200,

      sidebarMenu(

        menuItem("Analyze", icon = icon("chart-column"), tabName = "menu_input"),
        menuItem("Calibration", icon = icon("scale-balanced"), tabName = "menu_calibration"),
        menuItem("Export", icon = icon("file-export"), tabName = "menu_export"),
        menuItem("User guide", icon = icon("user-circle"), tabName = "menu_user_guide"),
        menuItem("About", icon = icon("circle-info"), tabName = "menu_about")
      )
    ),

    dashboardBody(
      theme_grey_light,
      includeHTML("www/banner.html"),
      includeScript("www/banner.js"),

      tabItems(
        tabItem(
          tabName = "menu_input",
          fluidRow(

            column(
              4,

              source("ui_files/ui_load_input_box.R",local = TRUE)$value,
              source("ui_files/ui_simulate_box.R",local = TRUE)$value

            ),

            column(
              8,

              tags$head(
                tags$style(HTML("
                  /* Custom left modal */
                  .modal-dialog {
                    position: fixed !important;
                    left: 80px;        /* margin from the left */
                    top: 80px;         /* margin from the top */
                    width: 400px;      /* wider modal */
                    margin: 0;         /* no default Bootstrap margin */
                  }
                  .modal-content {
                    border-radius: 12px;
                  }
                "))
              ),

             #Custom CSS to increase plot height
              tags$head(tags$style("
              #counts_plot{height:580px !important;}
              #counts_plotNormalized{height:580px !important;}
              #binding_plot{height:580px !important;}
              #counts_plot_stacked{height:700px !important;}
              #counts_plotNormalized_stacked{height:700px !important;}
              "
              )),

              # Custom CSS to increase font size in rhandsontable
              tags$style(HTML("
              .handsontable td {
              font-size: 15px !important;
              }")),

              # Custom CSS to center text vertically and horizontally in rhandsontable
              tags$style(HTML("
              .handsontable td {
              vertical-align: middle !important; /* vertical centering */
              text-align: center;                /* optional: horizontal centering */}
              ")),

              tabBox(
                title = "", width = 12,id = "tabset_plots",
                tabPanel("All (un)binding events",plotlyOutput("binding_plot")),
                tabPanel("Histogram (subplots)",plotlyOutput("counts_plot_stacked")),
                tabPanel("Normalised histogram (subplots)",plotlyOutput("counts_plotNormalized_stacked")),
                tabPanel("Histogram",plotlyOutput("counts_plot")),
                tabPanel("Normalised histogram",plotlyOutput("counts_plotNormalized"))

              ),

              tabBox(
                title = "", width = 9,id = "tabset_params_legends",
                tabPanel(
                  "Fitted params",
                  div(
                    style = "overflow-x: auto; width: 100%;",
                    tableOutput("fittedParams")
                  )
                ),
                tabPanel("Legends - traces",rHandsontableOutput("legendInfo",width = "100%")),
                tabPanel("Colors - histogram",rHandsontableOutput("legendInfoHist",width = "100%")),
                tabPanel("Histogram - caption",
                         p(HTML("Option 1 - MP mass distribution histogram and Gaussian fit of the
                         sample XX. The plot and analysis was done with the
                         PhotoMol tool (spc.embl-hamburg.de).")),
                         p(HTML("Option 2 - Mass distribution fitting of sample Y (© eSPC, spc.embl-hamburg.de)."))
                ),
                tabPanel("Histogram (all events) - caption",
                         p(HTML("Option 1 - MP binding
                         and unbinding events histogram.
                         Plot was generated using the PhotoMol tool (spc.embl-hamburg.de)")),
                         p(HTML("Option 2 - MP binding
                         and unbinding events histogram (
                         © eSPC, spc.embl-hamburg.de)."))
                )
              ),

              source("ui_files/ui_export_plot_box.R",local = TRUE)$value
            )
          )
        ),

        tabItem(
          tabName = "menu_calibration",
          fluidRow(

            column(
              4,

              source("ui_files/ui_load_input_box_calibration.R",local = TRUE)$value

            ),

            column(
              8,

              #Custom CSS to increase plot height
              tags$head(tags$style("
              #contrast_plot_calib{height:600px !important;}
              #contrast_plot_calib_fitted{height:600px !important;}
              #mass_vs_contrast{height:600px !important;}
              "
              )),

              conditionalPanel(
                condition = "input.calibrationMethod == 'calibrationFile' &&
                input.activateCalibration",

                tabBox(
                  title = "", width = 12,id = "tabset_calib",
                  tabPanel("Histogram",     plotlyOutput("contrast_plot_calib")),
                  tabPanel("Fitted histogram",  plotlyOutput("contrast_plot_calib_fitted")),
                  tabPanel("Masses vs Contrast",     plotlyOutput("mass_vs_contrast")),
                  tabPanel("Calibration parameters", tableOutput("calibParams"))
                ),

                tabBox(
                  title = "", width = 8,id = "tabset_params_legends_calib",
                  tabPanel(
                    "Fitted params",
                    div(
                      style = "overflow-x: auto; width: 100%;",
                      tableOutput("fittedParamsCalib")
                    )
                  ),
                  tabPanel("Legends - traces",rHandsontableOutput("legendInfoCalib",width = "100%")),
                  tabPanel("Colors - histogram",rHandsontableOutput("legendInfoHistCalib",width = "100%"))
                  ),

                  source("ui_files/ui_export_plot_box_calibration.R",local = TRUE)$value

              )
            )
          )
        ),

        tabItem(
          tabName = "menu_export",
          fluidRow(
            source("ui_files/ui_export_fitting_information.R",local=T)$value,
            source("ui_files/ui_export_plots_data.R",local=T)$value
          )
        ),

        tabItem(tabName = "menu_user_guide",includeHTML("docs/user_guide.html")),
        tabItem(tabName = "menu_about",includeHTML("docs/about.html"))
      )
    )
  )
)
