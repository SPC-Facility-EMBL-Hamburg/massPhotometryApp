create_hist_legend_df_calib <- function() {

  legendDfHist <- data.frame(
    'Legend' = names(pmCalibration$models),
    'Color'  = rep('#1f77b4',length(pmCalibration$models))
  )

  color_cellsHist <- data.frame(col=2,row=1:nrow(legendDfHist))

  output$legendInfoHistCalib <- renderRHandsontable({
    rhandsontable(
      legendDfHist,
      rowHeaders=NULL,
      col_highlight = color_cellsHist$col - 1,
      row_highlight = color_cellsHist$row - 1
  ) %>%
      hot_col(col = c(1,2),renderer = myrenderer) %>%
      hot_col(col = 1, width = 150,readOnly = TRUE)
  })

  reactives$legends_hist_calib <- legendDfHist[,1]
  reactives$color_hist_calib   <- legendDfHist[,2]

}

output$contrast_plot_calib <- renderPlotly({

  req(reactives$data_loadedCalibration)
  plot <- plotRefeynMassHist(
    pmCalibration$models,
    reactives$color_hist_calib,
    reactives$plot_config,
    contrasts=TRUE
  )
  return(plot)
})

observeEvent(input$massPhotometryFileCalibration,{
  
  req(input$massPhotometryFileCalibration)
  reactives$data_loadedCalibration <- FALSE

  withBusyIndicatorServer("Go2",{

    pmCalibration$models <- dict()

    pmCalibration$import_files(
      input$massPhotometryFileCalibration$datapath,
      input$massPhotometryFileCalibration$name
    )

    contrasts <- unlist(pmCalibration$get_properties('contrasts'))

    # Remove NA and verify that there are enough contrasts
    contrasts <- contrasts[!is.na(contrasts)]
    if (length(contrasts) < 100) {
      shinyalert(
        'There is no contrast data. Import a valid .h5 or .csv file.',
        type = 'error'
      )
      return(NULL)
    }

    create_hist_legend_df_calib()

    pmCalibration$apply_to_all(
      'create_histogram',
      use_masses=FALSE,
      window=c(min(contrasts)*1.05,0),
      bin_width=input$bin_widthContrast/cstFactorForContrast
    )

    pmCalibration$apply_to_all(
      'guess_peaks',
      min_height=14,
      min_distance=4,
      prominence=8
    )

    pks_initial <- pmCalibration$get_properties('peaks_guess')

    knownMassesInitial_1 <- c(480,148,66,200,120,600,1000,300)
    knownMassesInitial_2 <- 86*1:10

    # Start two empty vectors, one will have the file names, the other the peaks guess
    peaks_guess_as_str <- c()
    known_masses_as_str <- c()

    for (i in 1:length(pks_initial)) {

      peaks <- pks_initial[[i]]
      n_peaks <- length(peaks)

      if (n_peaks > 0) {

        peaks <- peaks*cstFactorForContrast
        peaks <- signif(peaks,2)

        if (n_peaks >= 4) {

          known_masses_as_str <- c(
            known_masses_as_str,
            paste(rev(knownMassesInitial_2[1:n_peaks]),collapse="  ")
          )

        } else {

          known_masses_as_str <- c(
            known_masses_as_str,
            paste(knownMassesInitial_1[1:n_peaks],collapse="  ")
          )

        }

        peaks <- paste(peaks,collapse=" ")
        peaks_guess_as_str <- c(peaks_guess_as_str,peaks)

      } else {

        peaks_guess_as_str <- c(peaks_guess_as_str,"")
        known_masses_as_str <- c(known_masses_as_str,"")

      }

    }

    df <- data.frame(
      'File' = names(pmCalibration$models),
      'Initial peak guesses' = peaks_guess_as_str,
      'Known masses (kDa)' = known_masses_as_str,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Remove the File column if there is only one file
    if (nrow(df) == 1) {
      df <- df[,-1,drop=FALSE]
      df <- rhandsontable(df,rowHeaders=NULL)
    } else {
      df <- rhandsontable(
        df,rowHeaders=NULL
      ) %>%
        hot_col(col = 1, readOnly = TRUE)
    }

    df <- df %>% hot_rows(rowHeights = 30)

    output$initialPeakGuessesTableCalib <- renderRHandsontable({df})

    minLimit <- floor(min(contrasts)*cstFactorForContrast)
    maxLimit <- abs(minLimit)

    updateNumericInput(
      session,"leftLimitWindowRangeContrast",
      value = minLimit, min = -1e6, max = 1e6, step = 1
    )

    updateNumericInput(
      session,"rightLimitWindowRangeContrast",
      value = maxLimit, min = -1e6, max = 1e6, step = 1
    )

    updateSliderInput(
      session,"window_rangeContrast",NULL,
      min = minLimit, max = maxLimit,
      value = c(minLimit,maxLimit),step = 1
    )

    reactives$data_loadedCalibration <- TRUE
    Sys.sleep(1)
    
  })

},priority = 10)

output$data_loadedCalibration <- reactive({reactives$data_loadedCalibration})
outputOptions(output, "data_loadedCalibration", suspendWhenHidden = FALSE)

updateModels <- function(slope,intercept) {

  if (length(photoMolModels$models) > 0) {

  reactives$data_loaded <- FALSE

  photoMolModels$apply_to_all(
    'contrasts_to_masses',
    slope=slope,
    intercept=intercept
  )

  masses <- unlist(photoMolModels$get_properties('masses'))

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

  axisSize <- floor(18 + ((12 - 18) / (8 - 1)) * (length(photoMolModels$models) - 1))

  updateNumericInput(session,'plot_axis_size',value = axisSize)

  reactives$data_loaded <- TRUE
  reactives$masses_available <- TRUE

  }
  return(NULL)
}

observeEvent(list(input$interceptCustom,input$slopeCustom),{
  req(input$activateCalibration)
  req(input$interceptCustom != 0)
  req(input$slopeCustom != 0)

  updateModels(input$slopeCustom/1e6,input$interceptCustom/1e6)

})

observeEvent(
  list(
    input$leftLimitWindowRangeContrast,
    input$rightLimitWindowRangeContrast),{

    if (input$leftLimitWindowRangeContrast < input$rightLimitWindowRangeContrast) {

      updateSliderInput(
        session,"window_rangeContrast",
        NULL,
        min = input$leftLimitWindowRangeContrast,
        max = input$rightLimitWindowRangeContrast,
        value = c(input$leftLimitWindowRangeContrast,
                  input$rightLimitWindowRangeContrast
        ),
        step = 1
      )

    }

})

observeEvent(
  input$triggerFittingCalib,{

  df_peak_guess <- hot_to_r(input$initialPeakGuessesTableCalib)
  peak_guess <- lapply(df_peak_guess[,'Initial peak guesses'],get_guess_positions)
  known_masses <- lapply(df_peak_guess[,'Known masses (kDa)'],get_guess_positions)

  known_standards_all <- c()

  # Verify that the lengths of each entry match
  # Verift that each peak guess is lower than the max contrast
  for (i in 1:length(peak_guess)) {
    if (length(peak_guess[[i]]) != length(known_masses[[i]])) {
      shinyalert(
        'The number of initial peak guesses must match the number of known masses (kDa).',
        type = 'error'
      )
      return(NULL)
    }
    if (any(peak_guess[[i]] > input$max_observed_contrast)) {
      shinyalert(
        'Each initial peak guess must be lower than the maximum observed contrast.',
        type = 'error'
      )
      return(NULL)
    }

    # Contrasts are sorted from lower to higher
    # Masses need to be sorted from higher to lower
    known_masses[[i]] <- rev(known_masses[[i]])
    known_standards_all <- c(known_standards_all,known_masses[[i]])
  }

  lower_limit_histogram <- input$window_rangeContrast[1] / cstFactorForContrast
  upper_limit_histogram <- input$window_rangeContrast[2] / cstFactorForContrast
  window                <- c(lower_limit_histogram,upper_limit_histogram)
  bin_width             <- input$bin_widthContrast / cstFactorForContrast

  fit_tables <- list()

  i     <- 0

  models <- pmCalibration$models

  gaussiam_sum_idx <- c()

  legends_count <- 0

  df_peak_guess <- hot_to_r(input$initialPeakGuessesTable)

  for (model in models) {

    i <- i + 1

    model$create_histogram(
      use_masses=FALSE,
      window=window,
      bin_width=bin_width
    )

    starting_values <- peak_guess[[i]] / cstFactorForContrast

    if (length(starting_values) == 0) {
      model$fitted_data <- NULL
      next
    }

    # Extract histogram centers to limit the mean and std tolerances
    histogram_centers <- model$histogram_centers
    mean_tolerance <- max(histogram_centers) - min(histogram_centers)
    std_tolerance  <- abs(max(histogram_centers)) / 3

    std_tolerance <- max(std_tolerance,0.01)

    result <- tryCatch(
      {

        model$fit_histogram(
          peaks_guess=np_array(starting_values),
          mean_tolerance=mean_tolerance,
          std_tolerance=std_tolerance,
          threshold=input$max_observed_contrast / cstFactorForContrast,
          fit_baseline=input$fit_baseline_calib
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

  pmCalibration$master_calibration(
    calibration_standards=known_standards_all
  )

  legends <- pmCalibration$create_plotting_config()
  legendDf <- legends[[1]]
  legendDf <- set_column_names_legend_df(legendDf)

  color_cells <- data.frame(col=2,row=1:nrow(legendDf))

  output$legendInfoCalib <- renderRHandsontable({
    rhandsontable(legendDf,
                  rowHeaders=NULL,
                  col_highlight = color_cells$col - 1,
                  row_highlight = color_cells$row - 1
  ) %>% hot_col(col = c(1,2),
                renderer = myrenderer) %>%
      hot_col(col = c(3,4),
              renderer = myrendererBoolean) %>%
      hot_col(col = 1, width = 150)})

  reactives$legends_config_calib$legends <- legendDf[,1]
  reactives$legends_config_calib$colors  <- legendDf[,2]
  reactives$legends_config_calib$sels    <- as.logical(legendDf[,3])
  reactives$legends_config_calib$leg_sels <- as.logical(legendDf[,4])

  if (length(fit_tables) > 0) {

    fit_table <- do.call(rbind,fit_tables)
    # Render without the amplitude column
    output$fittedParamsCalib <- renderTable({

      # Remove the 'Amplitudes' column
      df <- fit_table[,-5]

      # Obtain the peak legends
      legends <- reactives$legends_config_calib$legends

      if (length(gaussiam_sum_idx) > 0) {
        df$Legend <- legends[-gaussiam_sum_idx]
      } else {
        df$Legend <- legends
      }

      # Set shorter column names for the UI
      colnames(df)[1] <- "μ̂ * 10³"
      colnames(df)[2] <- "σ̂ * 10³"

      df[,1] <- df[,1]*1e3
      df[,2] <- df[,2]*1e3

      colnames(df)[5] <- "μ̂ error / %"
      colnames(df)[6] <- "σ̂ error / %"

      # Set column 3 and 4 as integers
      df[,3] <- as.integer(df[,3])
      df[,4] <- as.integer(df[,4])

      # Remove the column numbr 7 - amplitude error
      df <- df[,-7]

      return(df)
      },digits = 2)

  } else {

    output$fittedParamsCalib <- NULL

  }

  output$contrast_plot_calib_fitted <- renderPlotly({

    plot <-   plotRefeynFit(
      pmCalibration$models,
      reactives$plot_config,
      reactives$legends_config_calib,
      reactives$color_hist_calib,
      contrasts = TRUE,
      normalize = FALSE,
      stacked = TRUE
    )

    return(plot)

  })

  calibration_dic <- pmCalibration$calibration_dic
  contrasts <- py_to_r(calibration_dic[['exp_points']])
  slope <- calibration_dic[['fit_params']][1]
  intercept <- calibration_dic[['fit_params']][2]

  output$mass_vs_contrast <- renderPlotly({

    plot <- plotMass_vs_contrast(
        known_standards_all,
        contrasts,
        slope,
        intercept,
        reactives$plot_config
        )

    return(plot)

  })

  r_sq <- calibration_dic$fit_r2

  output$calibParams <- renderTable({

    table <- data.frame(slope,intercept,r_sq)
    colnames(table) <- c("Slope * 10⁶","Intercept * 10⁶","R squared")
    return(table)

  },digits = 6)

  updateModels(slope,intercept)

})

observeEvent(input$legendInfoCalib,{

  req(input$legendInfoCalib)
  legendInfo <- hot_to_r(input$legendInfoCalib)

  reactives$legends_config_calib$legends <- legendInfo[,1]
  reactives$legends_config_calib$colors  <- legendInfo[,2]
  reactives$legends_config_calib$sels    <- legendInfo[,3]
  reactives$legends_config_calib$leg_sels <- legendInfo[,4]

})

observeEvent(input$legendInfoHistCalib,{

  req(input$legendInfoHistCalib)
  legendInfo <- hot_to_r(input$legendInfoHistCalib)

  reactives$legends_hist_calib <- legendInfo[,1]
  reactives$color_hist_calib <- legendInfo[,2]

})


