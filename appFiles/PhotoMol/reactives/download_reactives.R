output$download_input_params <- downloadHandler(
  filename = function() {
    paste0("fitting_parameters_PhotoMol_",Sys.Date(),".csv")
  },content = function(file) {

    req(reactives$masses_available)

    parameter <- c(
      "Bin width",
      "Min. observed mass",
      "Fit baseline",
      "Window range"
    )
    
    value    <-  c(
      input$bin_width,
      input$min_observed_mass,
      input$fit_baseline,
      paste0('[ ',input$window_range[1]," : ",input$window_range[2],' ]')
    )

    initialPeakGuessesTable <- hot_to_r(input$initialPeakGuessesTable)

    for (row in 1:nrow(initialPeakGuessesTable)) {

      file_input  <- initialPeakGuessesTable[row,1]
      guess <- initialPeakGuessesTable[row,2]

      parameter <- c(parameter,paste0("Initial guess (",file_input,")"))
      value     <- c(value,guess)
    }

    df <- data.frame(parameter,value)

    write.csv(df,file,row.names = F,quote = F)

  }
)

output$download_params_table <- downloadHandler(filename = function() {
  paste0("fitted_parameters_PhotoMol_",Sys.Date(),".csv")
  },content = function(file) {

  req(reactives$masses_available)

  fit_tables <- list()

  i     <- 0
  cnt1  <- 0

  for (model in photoMolModels$models) {

    i <- i + 1
    py_table <- model$fit_table
    table <- pandas_to_r(py_table)

    if (length(photoMolModels$models) >1) table$File <- names(models)[i]

    if (nrow(table) > 0) {
      cnt1 <- cnt1 + 1
      fit_tables[[cnt1]] <- table
    }

  }

  if (length(fit_tables) > 0) {
    fit_table <- do.call(rbind,fit_tables)
  } else {
    fit_table <- NULL
  }
    
  write.csv(fit_table,file,row.names = F,quote = F)

})

output$download_fitting_params_table_calibration <- downloadHandler(filename = function() {
  paste0("fitting_parameters_calibration_PhotoMol_",Sys.Date(),".csv")
  },content = function(file) {
    
    parameter <- c(
      "Bin width",
      "Window range * 1e3"
    )
    
    value    <-  c(
      input$bin_widthContrast,
      paste0(input$window_rangeContrast[1],"-",input$window_rangeContrast[2])
    )

    initialPeakGuessesTableCalib <- hot_to_r(input$initialPeakGuessesTableCalib)

    # Add a first column with the file if not already present
    if (ncol(initialPeakGuessesTableCalib) == 2) {
        initialPeakGuessesTableCalib <- cbind(
            "File"=names(pmCalibration$models),
            initialPeakGuessesTableCalib
        )
    }

    for (row in 1:nrow(initialPeakGuessesTableCalib)) {

      file_input  <- initialPeakGuessesTableCalib[row,1]
      guess <- initialPeakGuessesTableCalib[row,2]

      parameter <- c(parameter,paste0("Initial guess (",file_input,")"))
      value     <- c(value,guess)

    }

    for (row in 1:nrow(initialPeakGuessesTableCalib)) {

      file_input  <- initialPeakGuessesTableCalib[row,1]
      mass <- initialPeakGuessesTableCalib[row,3]

      parameter <- c(parameter,paste0("Masses (",file_input,")"))
      value     <- c(value,mass)

    }

    df <- data.frame(parameter,value)
    
    write.csv(df,file,row.names = F,quote = F)

  })

output$download_params_table_calibration <- downloadHandler(filename = function() {
  paste0("fitted_parameters_calibration_PhotoMol_",Sys.Date(),".csv")
  },content = function(file) {
    
  req(reactives$data_loadedCalibration)

  fit_tables <- list()

  i     <- 0
  cnt1  <- 0

  for (model in pmCalibration$models) {

    i <- i + 1

    py_table  <- model$fit_table
    table <- pandas_to_r(py_table)

    if (length(pmCalibration$models) >1) table$File <- names(models)[i]

    if (nrow(table) > 0) {
      cnt1 <- cnt1 + 1
      fit_tables[[cnt1]] <- table
    }

  }

  if (length(fit_tables) > 0) {
    fit_table <- do.call(rbind,fit_tables)
  } else {
    fit_table <- NULL
  }

  write.csv(fit_table,file,row.names = F,quote = F)

  })

output$download_mass_histogram <- downloadHandler(filename = function() {
  paste0("mass_histogram_PhotoMol_",Sys.Date(),".csv")
},content = function(file) {
  
  req(reactives$masses_available)

  dfs <- list()


  i <- 0
  for (model in photoMolModels$models) {

    i <- i + 1

    counts <- model$hist_counts
    mass   <- model$histogram_centers

    df <- data.frame(
    "Histogram counts" = counts,
    "Mass (kDa)" = mass,
    "File" = names(photoMolModels$models)[i]
    )

    dfs[[i]] <- df
  }

  df <- do.call(rbind,dfs)

  write.csv(df,file,row.names = F,quote = F)

})


output$download_fitted_gaussians <- downloadHandler(filename = function() {
  paste0("fitted_gaussians_PhotoMol_",Sys.Date(),".csv")
},content = function(file) {

  req(reactives$masses_available)

  legendsAll <- reactives$legends_config$legends

  id_start  <- 1

  fits <- list()

  i <- 0
  for (model in photoMolModels$models) {

    i <- i + 1

    if (!is.null(model$fitted_data)) {

      fit  <- as.data.frame(model$fitted_data)

      id_end <- ifelse(ncol(fit) <= 3,id_start, id_start + ncol(fit) - 2)
      legends      <- legendsAll[id_start:id_end]
      id_start     <- id_end + 1

      if (length(legends) == 1) {
        fit           <- fit[,-ncol(fit)]
        colnames(fit) <- c("Mass kDa",legends)
      } else {
        colnames(fit) <- c("Mass kDa",legends[2:length(legends)],legends[1])
      }

      colnames(fit) <- paste0(
        colnames(fit),
        ' ',
        names(photoMolModels$models)[i]
      )

      fits[[length(fits)+1]] <- fit

    }

  }

  max_rows <- max(sapply(fits,nrow))

  for (i in 1:length(fits)) {
    fit <- fits[[i]]
    if (nrow(fit) < max_rows) {

      # Number of rows to add
      N <- max_rows - nrow(fit)

      # Create a dataframe with N rows of NA values
      na_rows <- data.frame(matrix(NA, nrow = N, ncol = ncol(fit)))

      # Set column names to match the original dataframe
      colnames(na_rows) <- colnames(fit)

      # Add the NA rows to the original dataframe
      fit <- rbind(fit, na_rows)

      fits[[i]] <- fit
    }
  }

  fit   <- do.call(cbind, fits)

  write.csv(fit,file,row.names = F,quote = F)
})


output$download_mass_histogramNormalised <- downloadHandler(filename = function() {
  paste0("mass_histogram_normalised_PhotoMol_",Sys.Date(),".csv")
},content = function(file) {
  
  req(reactives$masses_available)

  dfs <- list()

  i <- 0
  for (model in photoMolModels$models) {

    i <- i + 1

    counts <- model$hist_counts
    mass   <- model$histogram_centers

    counts <- counts / sum(counts)

    df <- data.frame(
      "Normalized histogram counts"=counts,
      "Mass (kDa)"=mass,
      "File"=names(photoMolModels$models)[i]
    )

    dfs[[i]] <- df
  }

  df <- do.call(rbind,dfs)

  write.csv(df,file,row.names = F,quote = F)

})

output$download_fitted_gaussiansNormalised <- downloadHandler(filename = function() {
  paste0("fitted_gaussians_normalised_PhotoMol_",Sys.Date(),".csv")
},content = function(file) {

  req(reactives$masses_available)

  legendsAll <- reactives$legends_config$legends

  id_start  <- 1

  fits <- list()

  i <- 0

  for (model in photoMolModels$models) {

    i <- i + 1

    if (!is.null(model$fitted_data)) {

        # Retrieve the mass data that was used for the fitting
      dfMass <- data.frame("mass"=model$masses)

      dfMass <- dfMass %>%
        filter(mass >= model$hist_window[1]) %>%
        filter(mass <= model$hist_window[2])

      fit  <- as.data.frame(model$fitted_data)

      id_end <- ifelse(ncol(fit) <= 3,id_start, id_start + ncol(fit) - 2)
      legends      <- legendsAll[id_start:id_end]
      id_start     <- id_end + 1

      if (length(legends) == 1) {
        fit           <- fit[,-ncol(fit)]
        colnames(fit) <- c("Mass kDa",legends)
      } else {
        colnames(fit) <- c("Mass kDa",legends[2:length(legends)],legends[1])
      }

      colnames(fit) <- paste0(
        colnames(fit),
        ' ',
        names(photoMolModels$models)[i]
      )

      fit[,2:ncol(fit)] <- fit[,2:ncol(fit)] / nrow(dfMass)

      fits[[length(fits)+1]] <- fit

    }

  }

  max_rows <- max(sapply(fits,nrow))

  for (i in 1:length(fits)) {
    fit <- fits[[i]]
    if (nrow(fit) < max_rows) {

      # Number of rows to add
      N <- max_rows - nrow(fit)

      # Create a dataframe with N rows of NA values
      na_rows <- data.frame(matrix(NA, nrow = N, ncol = ncol(fit)))

      # Set column names to match the original dataframe
      colnames(na_rows) <- colnames(fit)

      # Add the NA rows to the original dataframe
      fit <- rbind(fit, na_rows)

      fits[[i]] <- fit
    }
  }

  fit   <- do.call(cbind, fits)

  write.csv(fit,file,row.names = F,quote = F)

})

