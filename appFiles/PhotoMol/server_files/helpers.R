welcome_message <- function() {
  shinyalert(
    paste(
      "Welcome to PhotoMol! <br><small>This software is free for academic use. By clicking 'I accept,' you agree to the
      <a href='eSPC_academicSoftwareLicenseAgreement_EMBLEM.pdf' target='_blank' rel='noopener noreferrer'>Academic Software License Agreement</a>.
      For non-academic use, please contact
      <a href='https://embl-em.de/company/contact/' target='_blank' rel='noopener noreferrer'>EMBLEM</a>.<br>
      If this tool was useful for your project, please remember to cite the
      <a href='https://www.frontiersin.org/journals/molecular-biosciences/articles/10.3389/fmolb.2022.882288/full' target='_blank' rel='noopener noreferrer'>PhotoMol publication</a>.
      Additionally, please consider starring our
      <a href='https://github.com/SPC-Facility-EMBL-Hamburg/eSPC_biophysics_platform' target='_blank' rel='noopener noreferrer'>GitHub</a>
      repository.</small>"
    ),
    imageWidth = 180,
    imageHeight = 180,
    closeOnClickOutside = FALSE,
    closeOnEsc = FALSE,
    confirmButtonText = "I accept",
    size = "m",
    showCancelButton = TRUE,
    cancelButtonText = "I decline",
    html = TRUE,
    confirmButtonCol = "#8bb8e8",
    callbackR = function(x) {
      if (!x) welcome_message()
    }
  )
}

## Count folders in the current directory
count_folders <- function(dir) {
  
  total_folders <- length(list.files(dir))
  
  return(total_folders)
}

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

## Splits a vector into a list of n elements 
split_vec <- function(vector,chunck_n) {
  sels    <- list()
  chuncks <- ceiling( length(vector) /chunck_n )
  
  for (i in 1:chuncks) {
    idx <- seq(1+chunck_n*(i-1),chunck_n*i)
    sels[[i]] <- na.omit(vector[idx])
  }
  
  return(sels)
}

getFileNameExtension <- function (fn) {
  # remove a path
  splitted    <- strsplit(x=fn, split='/')[[1]]   
  # or use .Platform$file.sep in stead of '/'
  fn          <- splitted [length(splitted)]
  ext         <- ''
  splitted    <- strsplit(x=fn, split='\\.')[[1]]
  l           <-length (splitted)
  if (l > 1 && sum(splitted[1:(l-1)] != ''))  ext <-splitted [l] 
  # the extention must be the suffix of a non-empty name    
  return(ext)
}

# Input:  a string of numbers separated by spaces, i.e.  "1 2 3"
# Output: a vector with the corresponding numbers, i.e. c(1,2,3)
get_guess_positions <- function(guess_positions_list,factor=1) {
  
  guess_positions_list <- trimws(guess_positions_list)
  positions <- as.numeric(strsplit(guess_positions_list,"\\s+")[[1]]) / factor

  # Force a numeric vector
  positions <- c(positions)

  # Sort them from lowest to highest
  positions <- sort(positions)

  return(positions)
  
}

get_mass_limits <- function(hist_counts,hist_mass) {
  
  if (any(hist_counts >= 10)) {
    min <- as.integer(hist_mass[min(which(hist_counts >= 10))]-60)
    max <- as.integer(hist_mass[max(which(hist_counts >= 10))]+100)
  } else {
    min <- as.integer(min(hist_mass) - 20)
    max <- as.integer(max(hist_mass) + 20)
  }
  
  return(list("min"=roundUp(min,10),"max"=roundUp(max,10)))
  
}

roundUp <- function(x,m) m*ceiling(x / m)

get_df_mass <- function(refeyn,contrasts,all=FALSE) {

  dfMass <- data.frame("mass"=refeyn$contrasts)

  if (!contrasts) dfMass <- data.frame("mass"=refeyn$masses)

  if (!all) {

    dfMass <- dfMass %>%
      filter(mass >= refeyn$hist_window[1]) %>%
      filter(mass <= refeyn$hist_window[2])

    }

  # Replace mass with contrasts
  if (contrasts) (dfMass$mass <- dfMass$mass*cstFactorForContrast)

  return(dfMass)

}

get_bin_info <- function(refeyn,contrasts) {

  start <- refeyn$hist_window[1]
  end   <- refeyn$hist_window[2]
  size  <- refeyn$bin_width

  if (contrasts) {
      start <- start*cstFactorForContrast
      end   <- end*cstFactorForContrast
      size  <- size*cstFactorForContrast
  }

  return(list("start"=start,"end"=end,"size"=size))

}

set_column_names_legend_df <- function(df) {

    # Use nicer column names
  colnames(df)[1] <- 'Legend'
  colnames(df)[2] <- 'Color'
  colnames(df)[3] <- 'Show trace'
  colnames(df)[4] <- 'Show legend'

  return(df)

}