reactives <- reactiveValues(
    data_loaded=FALSE,
    data_loadedCalibration=FALSE,
    masses_available=FALSE,
    legends_config=list(
      legends = NULL,
      colors  = NULL,
      sels    = NULL,
      leg_sels= NULL
    ),
    legends_hist=NULL,
    color_hist=NULL,

    legends_config_calib=list(
      legends = NULL,
      colors  = NULL,
      sels    = NULL,
      leg_sels= NULL
    ),

    legends_hist_calib=NULL,
    color_hist_calib=NULL,

    selected_color='#1f77b4',
    selected_color_calib='#1f77b4',

    plot_config=list(
      width=14,
      height=12,
      type="png",
      axis_size=15,
      legend_font_size=14,
      addMassesToLegend=FALSE,
      addPercentageToLegend=FALSE,
      add_labels=TRUE,
      add_percentages=TRUE,
      show_grid_x = TRUE,
      show_grid_y = TRUE,
      show_axis_lines = TRUE,
      tickwidth = 2,
      ticklen = 8,
      linewidth=3
    )
)

create_hist_legend_df <- function() {

  legendDfHist <- data.frame(
    'Legend' = names(photoMolModels$models),
    'Color'  = rep('#1f77b4',length(photoMolModels$models))
  )

  color_cellsHist <- data.frame(col=2,row=1:nrow(legendDfHist))

  output$legendInfoHist <- renderRHandsontable({
    rhandsontable(
      legendDfHist,
      rowHeaders=NULL,
      col_highlight = color_cellsHist$col - 1,
      row_highlight = color_cellsHist$row - 1
  ) %>%
      hot_col(col = c(1,2),renderer = myrenderer) %>%
      hot_col(col = 1, width = 150,readOnly = TRUE)
  })

  reactives$legends_hist <- legendDfHist[,1]
  reactives$color_hist   <- legendDfHist[,2]
}

updateInputBox <- function(){

  create_hist_legend_df()

  hist_counts <- photoMolModels$get_properties('hist_counts')
  hist_mass   <- photoMolModels$get_properties('histogram_centers')

  limits <- lapply(1:length(hist_counts),function(i) get_mass_limits(hist_counts[[i]],hist_mass[[i]]))

  minValue <- min(unlist(limits))
  maxValue <- max(unlist(limits))*1.2

  updateSliderInput(session,"window_range",NULL,min = minValue,
                    max = maxValue,value = c(0,maxValue),step = 5)

  updateNumericInput(session,"leftLimitWindowRange",
                     value = minValue, min = -1e12, max = 0, step = 1)

  updateNumericInput(session,"rightLimitWindowRange",
                     value = maxValue, min = 0, max = 1e12, step = 1)

  pks_initial <- photoMolModels$get_properties('peaks_guess')

  # Start two empty vectors, one will have the file names, the other the peaks guess
  peaks_guess_as_str <- c()

  for (i in 1:length(pks_initial)) {

    peaks <- pks_initial[[i]]

    if (length(peaks) > 0) {

      peaks <- as.integer(peaks)
      peaks <- paste(peaks,collapse=" ")
      peaks_guess_as_str <- c(peaks_guess_as_str,peaks)

    } else {

      peaks_guess_as_str <- c(peaks_guess_as_str,"")

    }

  }

  df <- data.frame(
    'File' = names(photoMolModels$models),
    'Initial peak guesses (kDa)' = peaks_guess_as_str,
    check.names = FALSE
  )

  output$initialPeakGuessesTable <- renderRHandsontable({
    rhandsontable(df,rowHeaders=NULL) %>%
        hot_col(col = 1, readOnly = TRUE) %>%
        hot_rows(rowHeights = 30)
  })

}

# Load example dataset
observeEvent(input$GoLoadExample,{
  
  reactives$data_loaded <- FALSE
  resetPlotsAndTables()

  photoMolModels$models <- dict()
  photoMolModels$import_files("www/demo.h5","example")
  photoMolModels$apply_to_all('count_binding_events')

  photoMolModels$apply_to_all(
    'create_histogram',
    window=c(0,800),
    bin_width=input$bin_width
  )

  photoMolModels$apply_to_all(
    'guess_peaks',
    min_height=14,
    min_distance=4,
    prominence=8
  )

  updateInputBox()
  reactives$masses_available <- TRUE
  reactives$data_loaded <- TRUE
})

resetPlotsAndTables <- function() {

  output$counts_plot           <- NULL
  output$counts_plotNormalized <- NULL
  output$fittedParams          <- NULL
  output$legendInfo            <- NULL
  output$countPlot             <- NULL

  output$counts_plot_stacked           <- NULL
  output$counts_plotNormalized_stacked <- NULL

  return(NULL)

}

output$binding_plot <- renderPlotly({

  req(reactives$data_loaded)

  return(
    plotRefeynMassHist(
      photoMolModels$models,
      reactives$color_hist,
      reactives$plot_config
    ))
})

observeEvent(input$massPhotometryFile,{
  
  req(input$massPhotometryFile)

  resetPlotsAndTables()

  reactives$data_loaded <- FALSE

  photoMolModels$models <- dict()

  photoMolModels$import_files(input$massPhotometryFile$datapath,input$massPhotometryFile$name)

  # Check if contrasts are available
  contrasts <- unlist(photoMolModels$get_properties('contrasts'))

  # Remove NA values from contrasts
  contrasts <- contrasts[!is.na(contrasts)]

  # Verif we have some values
  allContrastsLoaded <- length(contrasts) > 100

  masses <- unlist(photoMolModels$get_properties('masses'))

  # Remove NA values from masses
  masses <- masses[!is.na(masses)]

  # Verify that are no NA values in masses
  allMassesLoaded <- length(masses) > 100

  reactives$masses_available <- allMassesLoaded

  # If contrasts are available, but masses are not, show a warning
  # We can still get masses if the user does the calibration
  if (allContrastsLoaded & !allMassesLoaded) {
    shinyalert(
      title="Contrasts were found in the input file(s), but masses were not.
      Please activate the calibration and use the intercept and slope to convert
      from contrasts to masses.", type = "warning"
    )
  }

  if (allMassesLoaded) {

    photoMolModels$apply_to_all(
      'create_histogram',
      window=c(0,max(masses)*1.2),
      bin_width=input$bin_width
    )

    photoMolModels$apply_to_all(
      'guess_peaks',
      min_height=14,
      min_distance=4,
      prominence=8
    )

    updateInputBox()
    reactives$data_loaded <- TRUE
  }

  axisSize <- floor(18 + ((12 - 18) / (8 - 1)) * (length(photoMolModels$models) - 1))

  updateNumericInput(session,'plot_axis_size',value = axisSize)

},priority = 10)

output$dataLoaded <- reactive({reactives$data_loaded})
outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)

observeEvent(list(input$leftLimitWindowRange,input$rightLimitWindowRange),{
  
  updateSliderInput(session,"window_range",NULL,min = input$leftLimitWindowRange, 
                    max = input$rightLimitWindowRange,value = c(input$leftLimitWindowRange,input$rightLimitWindowRange),
                    step = 1)
})

observeEvent(input$legendInfo,{

  req(input$legendInfo)
  legendInfo <- hot_to_r(input$legendInfo)

  reactives$legends_config$legends <- legendInfo[,1]
  reactives$legends_config$colors  <- legendInfo[,2]
  reactives$legends_config$sels    <- legendInfo[,3]
  reactives$legends_config$leg_sels <- legendInfo[,4]

})

observeEvent(input$legendInfoHist,{

  req(input$legendInfoHist)
  legendInfo <- hot_to_r(input$legendInfoHist)

  reactives$legends_hist <- legendInfo[,1]
  reactives$color_hist <- legendInfo[,2]

})

createPlotsAndTables <- function() {

  lower_limit_histogram <- input$window_range[1]
  upper_limit_histogram <- input$window_range[2]
  window                <- c(lower_limit_histogram,upper_limit_histogram)

  if(lower_limit_histogram >= upper_limit_histogram) return(NULL)

  fit_tables <- list()

  i     <- 0

  models <- photoMolModels$models

  gaussiam_sum_idx <- c()

  legends_count <- 0

  df_peak_guess <- hot_to_r(input$initialPeakGuessesTable)

  for (model in models) {

    i <- i + 1

    model$create_histogram(
      window=window,
      bin_width=input$bin_width
    )

    starting_values <- get_guess_positions(df_peak_guess[i,2])

    if (any(starting_values < input$min_observed_mass)) {
      shinyalert(
        title=paste0(
          "Some initial peak guesses are below the minimum observed
           mass (",input$min_observed_mass," kDa) and will be ignored."),
        type = "warning")
    }

    starting_values <- starting_values[starting_values > input$min_observed_mass]

    if (length(starting_values) == 0) {
      model$fitted_data <- NULL
      next
    }

    # Extract histogram centers to limit the mean and std tolerances
    histogram_centers <- model$histogram_centers
    mean_tolerance <- max(histogram_centers) - min(histogram_centers)
    std_tolerance  <- abs(max(histogram_centers)) / 3

    result <- tryCatch(
      {

        model$fit_histogram(
          peaks_guess=np_array(starting_values),
          mean_tolerance=mean_tolerance,
          std_tolerance=std_tolerance,
          threshold=input$min_observed_mass,
          fit_baseline=input$fit_baseline
        )

      }, error = function(e) {
        if (inherits(e, "python.builtin.RuntimeError")) {
          err <- py_last_error()
          shinyalert(
            paste0("⚠ Fitting error: ", err$value),
            type = 'error'
          )
          return('Error')
        } else {
          stop(e) # rethrow non-Python errors
        }
      }
    )

    if (!is.null(result)) return(NULL)

    table  <- as.data.frame(model$fit_table)

    if (length(models) > 1) table$File <- names(models)[i]

    if (nrow(table) > 0)  {

      have_two_or_more_peaks <- nrow(table) > 1

      if (have_two_or_more_peaks) {

        gaussiam_sum_idx <- c(gaussiam_sum_idx,1+legends_count)

      }

      fit_tables[[length(fit_tables)+1]] <- table

      legends_count <- legends_count + nrow(table) + have_two_or_more_peaks

    }

  }

  if (length(fit_tables) == 0) {
    return(NULL)
  }

  legends <- photoMolModels$create_plotting_config()
  legendDf <- legends[[1]]

  legendDf <- set_column_names_legend_df(legendDf)

  color_cells <- data.frame(col=2,row=1:nrow(legendDf))

  output$legendInfo <- renderRHandsontable({
    rhandsontable(legendDf,
                  rowHeaders=NULL,
                  col_highlight = color_cells$col - 1,
                  row_highlight = color_cells$row - 1
  ) %>% hot_col(col = c(1,2),
                renderer = myrenderer) %>%
      hot_col(col = c(3,4),
              renderer = myrendererBoolean) %>% 
      hot_col(col = 1, width = 150)})

  reactives$legends_config$legends <- legendDf[,1]
  reactives$legends_config$colors  <- legendDf[,2]
  reactives$legends_config$sels    <- as.logical(legendDf[,3])
  reactives$legends_config$leg_sels <- as.logical(legendDf[,4])

  if (length(fit_tables) > 0) {

    fit_table <- do.call(rbind,fit_tables)
    # Render without the amplitude column
    output$fittedParams <- renderTable({

      # Remove the 'Amplitudes' column
      df <- fit_table[,-5]

      # Obtain the peak legends
      legends <- reactives$legends_config$legends

      if (length(gaussiam_sum_idx) > 0) {
        df$Legend <- legends[-gaussiam_sum_idx]
      } else {
        df$Legend <- legends
      }

      # Set shorter column names for the UI
      colnames(df)[1] <- "μ̂ / kDa"
      colnames(df)[2] <- "σ̂ / kDa"

      colnames(df)[5] <- "μ̂ error / %"
      colnames(df)[6] <- "σ̂ error / %"

      # Remove the column numbr 7 - amplitude error
      df <- df[,-7]

      return(df)
      },digits = 0)

  } else {

    output$fittedParams <- NULL

  }

  output$counts_plot <- renderPlotly({
    
    plot <-   plotRefeynFit(
      photoMolModels$models,
      reactives$plot_config,
      reactives$legends_config,
      reactives$color_hist,
      contrasts = FALSE,
      normalize = FALSE,
      stacked = FALSE
    )

    if (input$runSimulation) {
      plot <- addSimulation2plotRefeynFit(
        plot,input$positionSimulate,input$stdSimulate,
        input$amplitudeSimulate,input$leftLimitSimulate
      )
    }

    return(plot)

  })

  output$counts_plot_stacked <- renderPlotly({

    plot <-   plotRefeynFit(
      photoMolModels$models,
      reactives$plot_config,
      reactives$legends_config,
      reactives$color_hist,
      contrasts = FALSE,
      normalize = FALSE,
      stacked = TRUE
    )

    if (input$runSimulation) {
      plot <- addSimulation2plotRefeynFit(
        plot,input$positionSimulate,input$stdSimulate,
        input$amplitudeSimulate,input$leftLimitSimulate
      )
    }

    return(plot)})

  output$counts_plotNormalized <- renderPlotly({
    
    plot <-   plotRefeynFit(
      photoMolModels$models,
      reactives$plot_config,
      reactives$legends_config,
      reactives$color_hist,
      contrasts = FALSE,
      normalize = TRUE,
      stacked = FALSE
    )

    return(plot)})

  output$counts_plotNormalized_stacked <- renderPlotly({
    
    plot <-   plotRefeynFit(
      photoMolModels$models,
      reactives$plot_config,
      reactives$legends_config,
      reactives$color_hist,
      contrasts = FALSE,
      normalize = TRUE,
      stacked = TRUE
    )

    return(plot)
  })

  return(NULL)

}

observeEvent(input$run_fitting,{

  req(reactives$data_loaded)
  req(reactives$masses_available)

  resetPlotsAndTables()

  createPlotsAndTables()

  updateTabsetPanel(session,"tabset_plots",selected = "Histogram (subplots)")

})
