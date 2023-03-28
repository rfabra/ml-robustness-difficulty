compute_feature_stats <- function(data, mylogger=NULL) {
  numcols<-ncol(data)-1
  numrows <- nrow(data)
  # Initializations
  feature_data <- list()
  feature_data$values <- list() 
  feature_data$sd <- list()
  feature_data$freqs <- list()
  feature_data$props <- list()
  # Iterate through features
  for (f in (1:numcols)) {
    feature_name <- colnames(data)[f]
    if (is.factor(data[,f]) || is.integer(data[,f]))
    { # Nominal or ordinal
      feature_data$values[[feature_name]] <- as.factor(unique(data[,f]))
      data_table <- table(data[,f])
      feature_data$freqs[[feature_name]] <- as.list(data_table)
      feature_data$props[[feature_name]] <- as.list(prop.table(data_table))
    } 
    else 
    { # Numeric
      feature_data$sd[[feature_name]] <- sd(data[,f])
    }
  }
  log_info_message("End compute_feature_stats", mylogger)
  return(feature_data)
}

generate_noisy_dataset <- function(data,
                                   noise_level, 
                                   instance_proportion, 
                                   path_noisy_dataset,
                                   feature_stats = NULL,
                                   data_for_stats = NULL,
                                   myseed=123, 
                                   mylogger = NULL)
{
  # Check if computations have been done before
  perturbed_data <- load_object(path_noisy_dataset, "noisy dataset", mylogger)
  if (!is.null(perturbed_data)) {
    return(perturbed_data)
  }
  perturbed_data <- data
  numcols<-ncol(data)-1
  numrows <- nrow(data)
  # Randomly select instances to perturb
  set.seed(myseed)
  instances_to_perturb <- sample(1:numrows, instance_proportion*numrows)
  # Precalculate statistics, if not provided in feature_stats
  if (is.null(feature_stats)) {
    if (is.null(data_for_stats)) {
      error_msg <- paste("If <feature_stats> parameter is NULL",
                         "<data_for_stats> must be provided to compute feature stats.",
                         sep=" ")
      log_error_message(error_msg, mylogger)
      stop(error_msg)
    }
    else {
      feature_stats <- compute_feature_stats(data_for_stats, mylogger)
    }
  }
  set.seed(myseed)
  # Iterate features
  for (f in (1:numcols)) {
    feature_name <- colnames(data)[f]
    # Iterate instances
    for (i in instances_to_perturb) {
      if (is.factor(data[,f]))
      { # Nominal
        f_value <- data[i,f]
        t_array <- rep(0, length(feature_stats$values[[feature_name]]))
        names(t_array) <- feature_stats$values[[feature_name]]
        t_array[feature_stats$values[[feature_name]] == f_value] <- 1
        # Apply generation method
        alpha <- 1 - exp(-noise_level)
        probs_new <- rep(0, length(feature_stats$values[[feature_name]]))
        names(probs_new) <- feature_stats$values[[feature_name]]
        for (v_name in names(probs_new)) {
          probs_new[v_name] <- (alpha * feature_stats$props[[feature_name]][[v_name]]) + (1 - alpha) * t_array[[v_name]]
        }
        new_value <- sample(names(probs_new), 1, prob=probs_new, replace=TRUE)
        perturbed_data[i,f] <- new_value
      } 
      else 
      { # Numeric
        perturbed_data[i,f] <- as.numeric(
          rnorm(1, mean = data[i,f], 
                sd = feature_stats$sd[[feature_name]] * noise_level)
        )
      }  
    } 
  }
  # Save noisy dataset
  save_object(perturbed_data, path_noisy_dataset, "noisy dataset", mylogger)
  log_info_message("END generate_noisy_dataset", mylogger)
  return(perturbed_data)
}

generate_noisy_datasets <- function(datasets_data, paths, noise_magnitude, mylogger = NULL) {
  log_info_message("STAGE: Generating noisy datasets", mylogger)
  log_info_message("--------------------", mylogger)
  # Iterate datasets  
  for (dataset_name in names(datasets_data)) {
    log_info_message(paste("Noisy datasets: Processing dataset", 
                           paste("<", dataset_name, ">", sep="")), mylogger)
    # Load dataset    
    dataset_path <- file_path_dataset[["data"]](paths, dataset_name)
    dataset <- load_dataset(dataset_name, datasets_data[[task_id]],
                            dataset_path = dataset_path)
    # Start 5-fold cross validation
    set.seed(123)
    folds <- create_folds(dataset$Class, k=5)
    i_fold <- 0
    # Iterate folds
    for(raw_train_fold in folds) {
      i_fold <- i_fold + 1
      # Separate folds
      train_fold <- rownames(dataset[raw_train_fold,])
      test_fold <- rownames(dataset[-raw_train_fold,])
      data_train <- dataset[train_fold,]
      data_test <- dataset[test_fold,]
      # Save instances id
      train_row_id <- data_train$row_id
      test_row_id <- data_test$row_id
      # Perform split
      data_train <- dplyr::select(data_train, -row_id)
      data_test <- dplyr::select(data_test, -row_id)
      path_noisy_dataset <- 
        file_path_noisy_dataset[["data"]](paths, dataset_name, i_fold, 
                                          noise_magnitude, 1.0)
      # Compute stats from training
      feature_stats <- compute_feature_stats(data_train, mylogger)
      # Generate noisy test, perturbing all the instances
      data_test_noisy <- generate_noisy_dataset(
        data_test, noise_magnitude, 1.0, 
        path_noisy_dataset, feature_stats = feature_stats, mylogger = mylogger)
      # Lock process
      path_lock <- file_path_locks(paths, dataset_name, 
                                   paste("noisy_datasets", i_fold, sep=""))
      if (lock_process(path_lock, mylogger)) {
        tryCatch(
          data_test_noisy <- generate_noisy_dataset(
            data_test, noise_magnitude, 1.0, 
            path_noisy_dataset, feature_stats = feature_stats, mylogger = mylogger),
          error = function(e) {
            log_info_message(paste("Error when generating noisy datasets for dataset", 
                                   paste("<", dataset_name, ">:", sep = "")), 
                             mylogger);
            log_info_message(e, mylogger)
          })
        unlock_process(path_lock, mylogger)
      }
    }
    log_info_message(paste("Noisy datasets: end dataset", 
                           paste("<", dataset_name, ">", sep = "")), mylogger)
  }
  return(TRUE)
}