config_dialog <- function(calib=FALSE) {

  if (!calib) {
    req(reactives$legends_hist)
    input_id_1 <- 'mol2changeColor'
    input_id_2 <- 'colorForLegend'
    value <- reactives$selected_color
  } else {
    req(reactives$legends_hist_calib)
    input_id_1 <- 'mol2changeColorCalib'
    input_id_2 <- 'colorForLegendCalib'
    value <- reactives$selected_color_calib
  }

  if (calib) {
    legends_traces <- reactives$legends_config_calib$legends
    legend_hist    <- reactives$legends_hist_calib
  } else {
    legends_traces <- reactives$legends_config$legends
    legend_hist    <- reactives$legends_hist
  }

  if (is.null(legends_traces)) {
    legends <- legend_hist
  } else {
    legends <- c(legends_traces, legend_hist)
  }

  showModal(
    modalDialog(

      fluidRow(

        column(
          width = 12,
          p(HTML("<b>Show peak values</b>"),
            span(shiny::icon("info-circle"), id = "info_uuL-1"),
            column(
              width = 6,
              checkboxInput("show_massesPlot", "In the plot", reactives$plot_config$add_labels)
            ),
            column(
              width = 6,
              checkboxInput("show_massesLegend", "In the legend", reactives$plot_config$addMassesToLegend),
              tippy::tippy_this(
                elementId = "info_uuL-1",
                tooltip = "Display the means of the fitted gaussians.",
                placement = "right")
            )
          )
        ),

        column(
          width = 12,
          p(HTML("<b>Show counts percentage</b>"),
            span(shiny::icon("info-circle"), id = "info_uuL-2"),
            column(
              width = 6,
              checkboxInput("show_percentagePlot", "In the plot",
                            reactives$plot_config$add_percentages
              )
            ),
            column(
              width = 6,
              checkboxInput("show_percentageLegend", "In the legend",
                            reactives$plot_config$addPercentageToLegend
              ),
              tippy::tippy_this(
                elementId = "info_uuL-2",
                tooltip = "Display the percentage of counts for each peak.",
                placement = "right")
            )
          )
        ),

        column(
          12,

          column(
            4,
            p(HTML("<b>Width</b>"),
              span(shiny::icon("info-circle"), id = "info_uu1-13"),
              numericInput('plot_width',NULL, reactives$plot_config$width,min = 1, max = 100),
              tippy::tippy_this(
                elementId = "info_uu1-13",
                tooltip = "Units are pixels * 50",
                placement = "right")
            )
          ),

          column(
            4,
            p(HTML("<b>Height</b>"),
              span(shiny::icon("info-circle"), id = "info_uu1-14"),
              numericInput('plot_height',NULL, reactives$plot_config$height,min = 1, max = 100),
              tippy::tippy_this(
                elementId = "info_uu1-14",
                tooltip = "Units are pixels * 50",
                placement = "right")
            )
          ),

          column(
            4,
            p(HTML("<b>File type</b>"),
              selectInput("plot_type", NULL, c(
                "PNG" = "png",
                "SVG" = "svg",
                "JPEG" = "jpeg"),
              selected=reactives$plot_config$type)
            )
          )
        ),

        column(
          12,
          column(
            6,
            p(HTML("<b>Axis font size</b>"),
              numericInput('plot_axis_size',NULL, reactives$plot_config$axis_size,min = 4, max = 40)
            )
          ),

          column(
            6,
            p(HTML("<b>Legend font size</b>"),
              numericInput('legend_font_size',NULL, reactives$plot_config$legend_font_size,min = 4, max = 40)
            )
          )
        ),

        column(
          12,

          column(
            4,
            p(HTML("<b>Show axis</b>"),
              checkboxInput("show_axis_lines", NULL, reactives$plot_config$show_axis_lines)
            )
          ),

          column(
            4,
            p(HTML("<b>Show x-grid</b>"),
              checkboxInput("show_grid_x", NULL, reactives$plot_config$show_grid_x)
            )
          ),

          column(
            4,
            p(HTML("<b>Show y-grid</b>"),
              checkboxInput("show_grid_y", NULL, reactives$plot_config$show_grid_y)
            )
          )
        ),

        column(
          12,
          column(
            width = 6,
            p(HTML('<b>Set colour</b>'),
              selectInput(inputId=input_id_1,
                          label=NULL,
                          choices=legends,
                          selectize = FALSE)
            )
          ),

          column(
            width = 6,
              p(HTML('<p style="margin-bottom:0px;"><br></p>'),
              colourInput(input_id_2, NULL, value = value)
              )
          )
        ),

        column(
          12,
          column(
            4,
            p(HTML("<b>Tick length</b>"),
              numericInput('ticklen',NULL, reactives$plot_config$ticklen,min = 0, max = 20)
            )
          ),

          column(
            4,
            p(HTML("<b>Tick width</b>"),
              numericInput('tickwidth',NULL, reactives$plot_config$tickwidth,min = 0, max = 20)
            )
          ),

          column(
            4,
            p(HTML("<b>Line width</b>"),
              numericInput('linewidth',NULL, reactives$plot_config$linewidth,min = 1, max = 12)
            )
          ),

        )
      ),

      footer=tagList(
        #actionButton('submitConfig', 'Submit'),
        modalButton('Close')
      )

    )
  )

}

observeEvent(input$configure_plot_btn,{
  config_dialog(calib = FALSE)
})

observeEvent(input$configure_plot_btn_calib,{
  config_dialog(calib = TRUE)
})


observeEvent(input$plot_axis_size,{

  reactives$plot_config$axis_size <- input$plot_axis_size

},ignoreInit = TRUE)

observeEvent(input$legend_font_size,{

  reactives$plot_config$legend_font_size <- input$legend_font_size

},ignoreInit = TRUE)

observeEvent(input$show_axis_lines,{

  reactives$plot_config$show_axis_lines <- input$show_axis_lines

},ignoreInit = TRUE)

observeEvent(input$show_grid_x,{

  reactives$plot_config$show_grid_x <- input$show_grid_x

},ignoreInit = TRUE)

observeEvent(input$show_grid_y,{

  reactives$plot_config$show_grid_y <- input$show_grid_y

},ignoreInit = TRUE)

observeEvent(input$show_massesLegend,{

  reactives$plot_config$addMassesToLegend <- input$show_massesLegend

},ignoreInit = TRUE)

observeEvent(input$show_percentageLegend,{

  reactives$plot_config$addPercentageToLegend <- input$show_percentageLegend

},ignoreInit = TRUE)

observeEvent(input$show_massesPlot,{

  reactives$plot_config$add_labels <- input$show_massesPlot

},ignoreInit = TRUE)

observeEvent(input$show_percentagePlot,{

  reactives$plot_config$add_percentages <- input$show_percentagePlot

},ignoreInit = TRUE)

observeEvent(input$plot_type,{

  reactives$plot_config$type <- input$plot_type

},ignoreInit = TRUE)

observeEvent(input$plot_width,{

  reactives$plot_config$width <- input$plot_width

},ignoreInit = TRUE)

observeEvent(input$plot_height,{

  reactives$plot_config$height <- input$plot_height

},ignoreInit = TRUE)

observeEvent(input$ticklen,{

  reactives$plot_config$ticklen <- input$ticklen

},ignoreInit = TRUE)

observeEvent(input$tickwidth,{

  reactives$plot_config$tickwidth <- input$tickwidth

},ignoreInit = TRUE)

observeEvent(input$linewidth,{

  reactives$plot_config$linewidth <- input$linewidth

},ignoreInit = TRUE)


observeEvent(input$colorForLegend,{

  req(reactives$data_loaded)
  reactives$data_loaded <- FALSE
  reactives$selected_color <- input$colorForLegend

  legends <- reactives$legends_config$legends
  colors <- reactives$legends_config$colors
  sels   <- reactives$legends_config$sels
  leg_sels <- reactives$legends_config$leg_sels

  if (input$mol2changeColor %in% legends) {

    idx <- which(legends == input$mol2changeColor)

    colors[idx] <- input$colorForLegend

    legendDf <- data.frame(
      legends = legends,
      color = colors,
      select = as.logical(sels),
      legend_select = as.logical(leg_sels)
    )

    legendDf <- set_column_names_legend_df(legendDf)

    reactives$legends_config$colors <- colors

    color_cells <- data.frame(col=2,row=1:length(colors))
    output$legendInfo <- renderRHandsontable({
      rhandsontable(
        legendDf,
        rowHeaders=NULL,
        col_highlight = color_cells$col - 1,
        row_highlight = color_cells$row - 1
    ) %>% hot_col(col = c(1,2),
                  renderer = myrenderer) %>%
        hot_col(col = c(3,4),
                renderer = myrendererBoolean) %>%
        hot_col(col = 1, width = 150)})

  } else {

    legends <- reactives$legends_hist
    colors  <- reactives$color_hist

    idx <- which(legends == input$mol2changeColor)

    colors[idx] <- input$colorForLegend

    legendDf <- data.frame(
      Legend = legends,
      Color = colors)

    reactives$color_hist  <- colors

    color_cells <- data.frame(col=2,row=1:length(colors))
    output$legendInfoHist <- renderRHandsontable({
      rhandsontable(
        legendDf,
        rowHeaders=NULL,
        col_highlight = color_cells$col - 1,
        row_highlight = color_cells$row - 1
    ) %>% hot_col(col = c(1,2),
                  renderer = myrenderer) %>%
        hot_col(col = 1, width = 150,readOnly=TRUE)})

  }

  reactives$data_loaded <- TRUE

})

observeEvent(input$colorForLegendCalib,{

  req(reactives$data_loadedCalibration)
  reactives$data_loadedCalibration <- FALSE
  reactives$selected_color_calib <- input$colorForLegendCalib

  legends <- reactives$legends_config_calib$legends
  colors <- reactives$legends_config_calib$colors
  sels   <- reactives$legends_config_calib$sels
  leg_sels <- reactives$legends_config_calib$leg_sels

  if (input$mol2changeColorCalib %in% legends) {

    idx <- which(legends == input$mol2changeColorCalib)

    colors[idx] <- input$colorForLegendCalib

    legendDf <- data.frame(
      legends = legends,
      color = colors,
      select = as.logical(sels),
      legend_select = as.logical(leg_sels)
    )

    legendDf <- set_column_names_legend_df(legendDf)

    reactives$legends_config_calib$colors <- colors

    color_cells <- data.frame(col=2,row=1:length(colors))
    output$legendInfoCalib <- renderRHandsontable({
      rhandsontable(
        legendDf,
        rowHeaders=NULL,
        col_highlight = color_cells$col - 1,
        row_highlight = color_cells$row - 1
    ) %>% hot_col(col = c(1,2),
                  renderer = myrenderer) %>%
        hot_col(col = c(3,4),
                renderer = myrendererBoolean) %>%
        hot_col(col = 1, width = 150)})

  } else {

    legends <- reactives$legends_hist_calib
    colors  <- reactives$color_hist_calib

    idx <- which(legends == input$mol2changeColorCalib)

    colors[idx] <- input$colorForLegendCalib

    legendDf <- data.frame(
      Legend = legends,
      Color = colors)

    reactives$color_hist_calib  <- colors

    color_cells <- data.frame(col=2,row=1:length(colors))
    output$legendInfoHistCalib <- renderRHandsontable({
      rhandsontable(
        legendDf,
        rowHeaders=NULL,
        col_highlight = color_cells$col - 1,
        row_highlight = color_cells$row - 1
    ) %>% hot_col(col = c(1,2),
                  renderer = myrenderer) %>%
        hot_col(col = 1, width = 150,readOnly=TRUE)})

  }

  reactives$data_loadedCalibration <- TRUE

})