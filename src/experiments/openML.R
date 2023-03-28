library(OpenML)

get_dataset_from_task <- function(dataset_name, task, mylogger = NULL) {
  dataset <- task$input$data.set$data
  # Correct datasets
  if (dataset_name == "Click_prediction_small.arff" || 
      dataset_name == "JapaneseVowels.arff" || 
      dataset_name == "lung-cancer.arff" ||
      dataset_name == "monks-problems-1.arff" ||
      dataset_name == "monks-problems-2.arff" ||
      dataset_name == "monks-problems-3.arff" ||
      dataset_name == "USPS") {
    # Class attribute must be at the end
    dataset <- cbind(dataset[,2:ncol(dataset)], dataset[,1])
    colnames(dataset)[ncol(dataset)]<-"Class"
  }
  if (dataset_name == "splice.arff" || 
      dataset_name == "synthetic_control.arff") {
    # Remove instance identifiers
    dataset <- dataset[,2:ncol(dataset)]
  }
  if (dataset_name == "collins") {
    # Class at the end 
    dataset <- dataset[,2:ncol(dataset)]
    n_feats <- ncol(dataset) - 1
  }
  # Last column must be Class
  colnames(dataset)[ncol(dataset)]<-"Class"  
  return(dataset)
}

get_runs_from_task <- function(task_id, 
                               path_runs = NULL, 
                               mylogger = NULL) {
  log_info_message(paste("Obtaining runs for task", task_id), mylogger)
  task_runs_total <- load_object(path_runs, 
                       mylogger)
  if (!is.null(task_runs_total)) {
    return(task_runs_total)
  }
  # Initialize task_runs_total
  task_runs_total <- listOMLRuns(task.id=task_id, limit = 1)
  # Download runs
  continue <- TRUE
  offset <- 1
  while (continue) {
    # Get runs list
    task_runs <- listOMLRuns(task.id=task_id, offset = offset)
    # If there are no more runs
    if (nrow(task_runs) == 0) {
      continue <- FALSE
    }
    else {
      # Add obtained runs
      task_runs_total <- rbind(task_runs_total, task_runs)
      offset <- offset + nrow(task_runs)  
    }
  }
  if (is.null(task_runs_total)) {
    log_error_message(sprintf("Could not get the runs for task %d", 
                              task_id), mylogger)
  } 
  else {
    save_object(task_runs_total, path_runs, 
                sprintf("runs for task %d", task_id), mylogger)
  }
  return(task_runs_total)
}

load_dataset <- function(dataset_name, task_id, dataset_path=NULL, 
                         mylogger = NULL) {
  log_info_message(sprintf("Loading dataset... %s", dataset_name), mylogger)
  dataset <- load_object(dataset_path, sprintf("Dataset %s", dataset_name),
                         mylogger = mylogger)
  # If don't have dataset, download and save
  if (is.null(dataset)) {
    task <- getOMLTask(task.id = task_id)
    dataset <- get_dataset_from_task(dataset_name, task)
    dataset <- cbind(data.frame(row_id = rownames(dataset)), dataset)
    save_object(dataset, dataset_path, sprintf("Dataset %s", dataset_name),
                mylogger = mylogger)
  }
  return(dataset)
}

