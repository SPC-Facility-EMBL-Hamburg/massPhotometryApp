options(shiny.maxRequestSize=30*1024^2)
options(stringsAsFactors = F)

source("server_files/helpers.R")
source("server_files/plot_functions.R")

# Import the Python package
pyphotomol <- import("pyphotomol")

function(input, output, session) {

  photoMolModels <- pyphotomol$MPAnalyzer()
  pmCalibration  <- pyphotomol$MPAnalyzer()

  source(paste0(base_dir,"reactives/reactives.R"           ), local = T)
  source(paste0(base_dir,"reactives/reactivesCalibration.R"), local = T)
  source(paste0(base_dir,"reactives/reactives_plot_configure.R" ), local = T)
  source(paste0(base_dir,"reactives/download_reactives.R"  ), local = T)

}
