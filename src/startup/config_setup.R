options( java.parameters = "-Xmx6g" )
options(warn=1)

options(warn=0)
options(java.parameters = "-Xmx6g")

initialize_project_packages <- function() {
  library(OpenML)
  library(mirt)
  library(caret)
  library(RWeka)
  library(fs)
  library(log4r)
  library(argparser)
  library(rjson)
  library(ggplot2)
  library(gridExtra)
  library(ggpubr)
  library(Hmisc)
  library(dplyr)
  library(ggrepel)
  library(wesanderson)
  library(stringr)
  library(earth)
  library(splitTools)
  library(factoextra)
  library(xtable)
  library(reshape)
  library(ggridges)
  library(fpc)
  library(clusterSim)
  
  
  source("src/startup/logger.R")
  source("src/startup/config_setup.R")
  source("src/startup/file_management.R")
  source("src/startup/utils.R")
  source("src/startup/locker.R")
  
  source("src/experiments/openML.R")
  source("src/experiments/response_matrix.R")
  source("src/experiments/irt.R")
  source("src/experiments/noisy_dataset.R")
  source("src/experiments/training.R")
  source("src/experiments/predictions.R")
  source("src/experiments/predictions_per_bin.R")
  source("src/experiments/process_predictions.R")
  source("src/experiments/taxonomy/aggregate_curves.R")
  source("src/experiments/taxonomy/clustering.R")
  source("src/experiments/taxonomy/cluster_prototypes.R")
  
  
  source("src/analytics/class_distributions.R")
  source("src/analytics/plot.R")
  source("src/analytics/metrics.R")
  source("src/analytics/datasets.R")
  source("src/analytics/irt.R")
  source("src/analytics/datasets_evaluation.R")
  source("src/analytics/robustness_curves.R")
  source("src/analytics/taxonomy/clustering.R")
  source("src/analytics/taxonomy/quality.R")
}

initialize_paths <- function(paths, mylogger=NULL) {
  log_info_message("Creating paths...", mylogger)
  base_dir <- paths[1]
  paths_with_base_dir <- paths
  for (p in names(paths[2:length(paths)])) {
    if (p != "analytics" && p != "data")
      paths_with_base_dir[p] <- path(base_dir, paths[p])
    else {
      for (pa in names(paths[[p]])) {
        paths_with_base_dir[[p]][[pa]] <- path(base_dir, paths[[p]][[pa]])
      }
    }
      
  }
  return(paths_with_base_dir)
}

create_directories <- function(paths, mylogger=NULL) {
  log_info_message("Creating directories...", mylogger)
  for (p in names(paths)) {
    if (p != "analytics" && p != "data") {
      log_info_message(paste(p, ":", paths[[p]]), mylogger)
      dir.create(paths[[p]], recursive=TRUE, showWarnings = FALSE)
    }
    else {
      for (pa in names(paths[[p]])) {
        log_info_message(paste(p, ":", paths[[p]][[pa]]), mylogger)
        dir.create(paths[[p]][[pa]], recursive=TRUE, showWarnings = FALSE)
      }
    }
  }
  return(paths)
}

# Parse command line arguments
# and set global flags
config_setup <- function(description, config_file) {
  initialize_project_packages()
  config <- fromJSON(file=config_file)
  
  # Initialize paths and create directories
  config$paths <- initialize_paths(config$paths)
  create_directories(config$paths)
  # Start logging
  config$logger <- init_logger(config$paths$logs)
  log_info_message("Initialized config setup.", config$logger)
  
  
  # Filter non-active parts of the experiment
  config$experiments$taxonomy$aggregate_curves$methods <- 
    names(which(config$experiments$taxonomy$aggregate_curves$methods == TRUE))
  
  config$experiments$taxonomy$datasets_sizes <- 
    names(which(config$experiments$taxonomy$datasets_sizes == TRUE))
  
  config$experiments$taxonomy$clustering$type <-   
    names(which(config$experiments$taxonomy$clustering$type == TRUE))
  
  return(config)
}
