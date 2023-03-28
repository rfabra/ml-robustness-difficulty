predict_per_bin_datasets <- function(datasets_data, models, paths, irt, 
                                     noise_data, mylogger = NULL) {
  log_info_message("STAGE: Predictions per bin", mylogger)
  log_info_message("----------------------", mylogger)
  for (dataset_name in names(datasets_data)) {
    log_info_message(paste("Predictions per bin: Processing dataset", 
                           paste("<", dataset_name, ">", sep="")), mylogger)
    # Check if it's already computed    
    path_predictions_per_bin <- 
      file_path_predictions_per_bin[["data"]](paths, dataset_name, noise_data$magnitude)
    
    if (file.exists(path_predictions_per_bin)) {
      log_info_message(paste("Predictions per bin: dataset already processed", 
                             paste("<", dataset_name, ">", sep="")), mylogger)
      next
    }
    # Initialize
    predictions_per_bin <- data.frame(
      Fold = NA,
      row_id = NA,
      Model = NA,
      Real = NA,
      Predicted = NA,
      Predicted.Noisy = NA,
      Noisy.Instances = NA,
      Difficulty = NA,
      Bin = NA,
      Num.Bin = NA
    )
    predictions_per_bin <- predictions_per_bin[0,]
    # If other process is working on this, next
    path_lock <- file_path_locks(paths, dataset_name, 
                                 "predictions_per_bin")
    if (!lock_process(path_lock, mylogger)) {
      log_info_message(paste("LOCKED - Predictions per bin:", 
                             paste("<", dataset_name, ">", sep="")), mylogger)
      next
    }
    # Load dataset
    dataset_path <- file_path_dataset[["data"]](paths, dataset_name)
    dataset <- load_dataset(dataset_name, datasets_data[[dataset_name]],
                            dataset_path = dataset_path)
    # Load difficulty data    
    path_difficulty <- file_path_difficulty[["data"]](paths, dataset_name, irt)
    difficulty_data <- load_object(path_difficulty, "Difficulty", mylogger)
    difficulty <- difficulty_data$difficulty
    difficulty_bins <- difficulty_data$bins
    # Start 5-cross validation
    set.seed(123)
    folds <- create_folds(dataset$Class, k=5)
    i_fold <- 0
    # Iterate folds
    for(raw_train_fold in folds) {
      i_fold <- i_fold + 1
      # Train-test splits
      train_fold <- rownames(dataset[raw_train_fold,])
      test_fold <- rownames(dataset[-raw_train_fold,])
      data_train <- dataset[train_fold,]
      data_test <- dataset[test_fold,]
      # Keep instances id
      train_row_id <- data_train$row_id
      test_row_id <- data_test$row_id
      # Remove 0 variance features
      data_train <- remove_constant_columns(data_train)
      # Iterate models
      for (model_name in names(models)) {
        log_info_message(sprintf("Model %s", model_name), mylogger)
        # Load original predictions
        preds_path <- file_path_predictions[["data"]](paths, dataset_name, 
                                                      model_name, 0, i_fold)
        predictions_original <- load_object(preds_path, "Original predictions",
                                            mylogger = mylogger)
        # Load noisy predictions  
        preds_path <- file_path_predictions[["data"]](paths, dataset_name, 
                                                      model_name, 1.0, i_fold)
        predictions_noisy <- load_object(preds_path, "Noisy predictions",
                                            mylogger = mylogger)
        # Join all data
        predictions <- data.frame(
          Fold = i_fold,
          row_id = test_row_id,
          Model = model_name,
          Real = data_test$Class,
          Predicted = predictions_original,
          Predicted.Noisy = predictions_noisy
        )
        # Add difficulty
        difficulty <- difficulty_data$difficulty
        predictions_metadata <- merge(predictions, difficulty, by="row_id")
        # Iterate noisy instances proportion
        for (noise_instance in noise_data$instance_proportions) {
          log_info_message(sprintf("Noisy instances: %0.2f", noise_instance), mylogger)
          predictions_metadata <- dplyr::mutate(predictions_metadata, Noisy.Instances = noise_instance)
          # Iterate difficulty bins
          for (d_bin in difficulty_bins) {
            i_bin <- which(d_bin == difficulty_bins)
            log_info_message(paste("Processing bin", i_bin), mylogger)
            # Get the test data from the bin
            data_test_bin <- dplyr::filter(predictions_metadata, Bin == d_bin) %>%
              mutate(Num.Bin = i_bin)
            # Perturb the noise_instance % of instances
            # Sample from perturbed predictions
            num_instances_bin <- nrow(data_test_bin)
            set.seed(123)
            perturbed_idx <- sample(1:num_instances_bin, noise_instance*num_instances_bin)
            # Control the case of no perturbation
            if (length(perturbed_idx) == 0) {
              data_test_bin$Predicted.Noisy <- data_test_bin$Predicted
            } else {
              data_test_bin[-perturbed_idx,]$Predicted.Noisy <- 
                data_test_bin[-perturbed_idx,]$Predicted
            }
            data_test_bin$Noisy.Instances <- factor(data_test_bin$Noisy.Instances,
                                                    levels = noise_data$instance_proportions)
            predictions_per_bin <- rbind(predictions_per_bin, data_test_bin)
          }
        }
      }
    }
    # Save results
    save_object(predictions_per_bin, 
                save_path = path_predictions_per_bin,
                object_name = "Predictions per bin",
                mylogger = mylogger)
    unlock_process(path_lock, mylogger)
    log_info_message(paste("PREDICTIONS PER BIN: end dataset", 
                           paste("<", dataset_name, ">", sep = "")), mylogger)
  }
  return(TRUE)
}