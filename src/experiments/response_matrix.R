get_response_matrix <- function(dataset, task_runs, num_runs, 
                                path_response_matrix = NULL, 
                                mylogger = NULL) {
  # If computations have been done before, return them
  response_matrix <- load_object(path_response_matrix, 
                                 "response matrix",
                                 mylogger)
  if (is.null(response_matrix)) {
    log_info_message("The response matrix cannot be loaded.", mylogger)
  }
  else {
    return(response_matrix)
  }
  # Initialize result
  irt_input <- data.frame(matrix(nrow = 0, ncol = nrow(dataset)))
  colnames(irt_input) <- rownames(dataset)
  # Sample random runs
  set.seed(123)
  random_runs_id <- sample(task_runs$run.id)
  # Class labels with instance ID
  true_labels <- dplyr::select(dataset, Class) %>%
    dplyr::mutate(row_id = rownames(dataset))
  available_runs <- length(random_runs_id)
  # Iterate runs
  i <- 1
  for (run_id in random_runs_id) {
    log_info_message(sprintf("Collected runs: (%d/%d) from %d available", 
                             i - 1, num_runs, available_runs), mylogger)
    # If we got all runs, finish
    if (i - 1  == num_runs) {
      break
    }
    log_info_message(paste("Run index:", i, "run ID:", run_id), mylogger)
    # Get the run
    run <- tryCatch(
      getOMLRun(run.id = run_id),
      error = function(x) {
        log_info_message(x, mylogger)
        return(NULL)
      })
    # If we got the run, add information
    if (!is.null(run)) {
      pred_data <- run$predictions %>% 
        dplyr::select(row_id, prediction=prediction) %>%
        group_by(row_id) %>% 
        summarise(prediction = Mode(prediction))
      result <- merge(pred_data, true_labels, by="row_id") %>%
        mutate(Hit=as.numeric(prediction == Class))
      # Response from the model for each item
      response <- data.frame(matrix(nrow = 1, ncol = nrow(dataset)))
      # Keep instances ID
      colnames(response) <- result$row_id
      # Add to the total
      response[1,] <- result$Hit
      irt_input <- rbind(irt_input, response)
      i <- i + 1
    }
  }
  # If we cannot get all the runs
  if (i < num_runs) {
    log_warn_message(sprintf("Could not get all the runs.\nRequired runs: %d\nValid runs: %d",
                             num_runs, i),
                     mylogger)
  }
  # Remove constant columns, they cannot feed IRT
  irt_input <- remove_constant_columns(irt_input)
  # Save response matrix
  save_object(irt_input, path_response_matrix, "response matrix", mylogger)
  log_info_message("END get_response_matrix", mylogger)
  return(irt_input)
}

compute_datasets_response_matrix <- function(datasets_data, paths, num_models, mylogger = NULL) {
  log_info_message("STAGE: Computing RESPONSE MATRIX", mylogger)
  log_info_message("-------------------------------------------------", mylogger)
  
  
  for (dataset_name in names(datasets_data)) {
    
    log_info_message(paste("RESPONSE MATRIX: Processing dataset", 
                           paste("<", dataset_name, ">", sep="")), mylogger)
    
    task_id <- datasets_data[[dataset_name]]
    dataset_path <- file_path_dataset[["data"]](paths, dataset_name)
    dataset <- load_dataset(dataset_name, task_id,
                            dataset_path = dataset_path)
    
    set.seed(123)
    path_runs <- file_path_runs[["data"]](paths, dataset_name)
    task_runs <- get_runs_from_task(task_id, path_runs, mylogger)
    path_response_matrix <- 
      file_path_response_matrix[["data"]](paths, dataset_name, num_models)
    
    if (file.exists(path_response_matrix)) {
      log_info_message("Already processed. Next...", mylogger)
      next
    }
    
    path_lock <- file_path_locks(paths, dataset_name, "response-matrix")
    if (lock_process(path_lock, mylogger)) {
      tryCatch(
        
        response_matrix <- get_response_matrix(
          dataset, task_runs, num_models,
          path_response_matrix = path_response_matrix, mylogger = mylogger),
        
        
        error = function(e) {
          log_info_message(paste("Error when computing REPONSE MATRIX for dataset", 
                                 paste("<", dataset_name, ">:", sep = "")), 
                           mylogger);
          log_info_message(e, mylogger)
        })
      unlock_process(path_lock, mylogger)
      log_info_message(paste("REPONSE MATRIX: end dataset", 
                             paste("<", dataset_name, ">", sep = "")), mylogger)
    }
  }
  return(TRUE)
}