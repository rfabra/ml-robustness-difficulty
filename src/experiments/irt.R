library(OpenML)
library(mirt)

modelIRT <- function(dat.irt, 
                     method = "2PL", 
                     n_cycles=300, 
                     remove_abtruse_instances = FALSE, 
                     mylogger=NULL) {
  # Compute IRT
  if (method == "2PL") {
    log_info_message("START. Fitting the 2PL model", mylogger)
    fit <- mirt(dat.irt, 1, itemtype = '2PL', technical = list(NCYCLES = n_cycles))
  } else {
    if (method == "1PL") {
      
    } else {
      log_info_message("START. Fitting the 1PL model", mylogger)
      fit <- mirt(dat.irt, 1, itemtype = '1PL', technical = list(NCYCLES = n_cycles))
    }
  }
  log_info_message("...END fit", mylogger)
  log_info_message("Extracting parameters...", mylogger)
  temp = coef(fit, simplify = T, IRTpars =T)$items
  params <- data.frame(temp[,c("g","b","a")])
  colnames(params)<-c("Gussng","Difficulty","Dscrmn")
  params <- cbind(params, data.frame(row_id = colnames(dat.irt)))
  rownames(params) <- colnames(dat.irt)
  # Remove abtruse instances
  if (remove_abtruse_instances) {
    log_info_message("Filtering abtruse items (with negative discrimination)")
    params <- params[params$Dscrmn >= 0,]
  }
  # Computing the abilities 'ab_vector' of the respondents   
  print("Extracting ability...")
  abil<- as.data.frame(t(fscores(fit)))
  colnames(abil) = rownames(dat.irt)
  rownames(abil) = NULL
  log_info_message("...END IRT function", mylogger)
  return(list("model"=fit, "parameters"=params, "abilities"=abil))
}

incremental_IRT <- function(response_matrix, 
                            n_cycles, 
                            remove_abtruse_instances = TRUE,
                            path_irt = NULL,
                            method = "2PL",
                            mylogger = NULL) {
  log_info_message("Applying incremental IRT...")
  num_examples <- ncol(response_matrix)
  chunk_size <- ceil(sqrt(num_examples))
  # If it's computed, return it
  irt_model <- load_object(path_irt, "IRT model", mylogger)
  if (!is.null(irt_model)) {
    return(irt_model)
  }
  # Iterate through dataset, splitted by chunks
  for (i in seq(1, num_examples, chunk_size)) {
    log_info_message(sprintf("Current chunk %d:%d from %d", i, 
                             min(i + chunk_size - 1, num_examples),
                             num_examples),
                     mylogger)
    # Get current chunk
    current_chunk <- response_matrix[,i:min(i + chunk_size - 1, num_examples)]
    # Compute result
    irt_result <- modelIRT(current_chunk, 
                           method, 
                           n_cycles, 
                           remove_abtruse_instances,
                           mylogger)
    # Add to the previous chunks
    if (!exists("irt_result_total")) {
      irt_result_total <- irt_result
    }
    else {
      irt_result_total$parameters <- rbind(irt_result_total$parameters, irt_result$parameters)
    }
  }
  # Save IRT model
  save_object(irt_result_total, path_irt, "IRT model", mylogger)
  
  return(irt_result_total)
}

extract_difficulty_data <- function(irt_data, irt_num_bins, 
                                    path_difficulty, mylogger = NULL) {
  log_info_message("Extracting difficulty data...", mylogger)  
  # If it's done before, return it
  difficulty_data <- load_object(path_difficulty, "Difficulty data", mylogger)
  if (!is.null(difficulty_data)) {
    return(difficulty_data)
  }
  # Select only difficulty
  difficulty <- dplyr::select(irt_data$parameters, Difficulty, row_id)
  # Filter outliers 
  difficulty <- difficulty %>%
    dplyr::filter(Difficulty > -6) %>%
    dplyr::filter(Difficulty <  6) %>%
    dplyr::mutate(Bin=cut2(Difficulty,
                           g=irt_num_bins))
  #  Add difficulty bins
  difficulty_bins <- sort(unique(difficulty$Bin))
  # Insert results in a list
  difficulty_data <- list(difficulty = difficulty, 
                          bins = difficulty_bins)
  # Save results
  save_object(object_to_save = list(difficulty = difficulty, bins = difficulty_bins),
              save_path = path_difficulty,
              object_name = "Difficulty data")
  
  return(difficulty_data)
}

compute_datasets_IRT <- function(datasets_data, paths, num_models, irt, mylogger = NULL) {
  log_info_message("STAGE: Computing IRT", mylogger)
  log_info_message("--------------------", mylogger)
  # Iterate datasets  
  for (dataset_name in names(datasets_data)) {
    log_info_message(paste("IRT: Processing dataset", 
                           paste("<", dataset_name, ">", sep="")), mylogger)
    # Load response matrix
    path_response_matrix <- 
      file_path_response_matrix[["data"]](paths, dataset_name, num_models)
    response_matrix <- load_object(path_response_matrix, "response matrix", 
                                   mylogger = mylogger)
    # Check if it's already computed    
    path_irt <- file_path_IRT[["data"]](paths, dataset_name, irt)
    if (file.exists(path_irt)) {
      log_info_message("Already processed. Next...", mylogger)
      next
    }
    # Lock process
    path_lock <- file_path_locks(paths, dataset_name, "IRT")
    if (lock_process(path_lock, mylogger)) {
      # Execute IRT
      tryCatch(
        incremental_IRT(response_matrix, irt$n_cycles, 
                        path_irt = path_irt, mylogger = mylogger),
        error = function(e) {
          log_info_message(paste("Error when computing IRT for dataset", 
                                 paste("<", dataset_name, ">:", sep = "")), 
                           mylogger);
          log_info_message(e, mylogger)
        })
      # Extract difficulty data (instances difficulty and bins)
      path_difficulty <- file_path_difficulty[["data"]](paths, dataset_name, irt)
      irt_result <- load_object(path_irt, "IRT data", mylogger = mylogger)
      difficulty_data <- extract_difficulty_data(irt_result, irt$num_bins,
                                                 path_difficulty, 
                                                 mylogger = mylogger)
      # Unlock process
      unlock_process(path_lock, mylogger)
      log_info_message(paste("IRT: end dataset", 
                             paste("<", dataset_name, ">", sep = "")), mylogger)
    }
  }
  return(TRUE)
}