process_predictions <- function(datasets_names, model_names, paths, 
                                noise_magnitude, noise_instance_proportions, 
                                mylogger = NULL) {
  # evaluations <- NULL
  
  curves <- NULL
  class_distr_real <- NULL
  class_distr_pred <- NULL
  
  for (dataset_name in datasets_names) {
    log_info_message(paste("Processing predictions of dataset", dataset_name), mylogger)
    # Load predictions per bin
    path_predictions_per_bin <- 
      file_path_predictions_per_bin[["data"]](paths, dataset_name, noise_magnitude)
    predictions_per_bin <- load_object(path_predictions_per_bin, "Predictions per bin", mylogger)
    
    # filter out non-desired noise configurations
    predictions_per_bin <- dplyr::filter(predictions_per_bin, Noisy.Instances %in% noise_instance_proportions)
    # Evaluation without bins, i.e., overall metrics
    #log_info_message("Dataset evaluation...", mylogger)
    # dataset_evaluation <- get_dataset_evaluation(predictions_per_bin)
    # Evaluation per bin, i.e., curves
    log_info_message("Curves...", mylogger)
    dataset_curves <- get_dataset_curves(predictions_per_bin)
    
    # Real and predicted class distributions
    log_info_message("Real class distributions", mylogger)
    class_distr_real_dataset <- get_class_distribution[["Real"]](predictions_per_bin)
    log_info_message("Predicted class distributions", mylogger)
    class_distr_pred_dataset <- get_class_distribution[["Predicted.Noisy"]](predictions_per_bin)
    # Append all the results to their corresponding data.frames 
    # log_info_message("Evaluations...", mylogger)
    # evaluations <- create_or_append_df(evaluations, dataset_evaluation, dataset_name)
    curves <- create_or_append_df(curves, dataset_curves, dataset_name)
    class_distr_real <- create_or_append_df(class_distr_real, 
                                            class_distr_real_dataset, 
                                            dataset_name)
    class_distr_pred <- create_or_append_df(class_distr_pred, 
                                            class_distr_pred_dataset, 
                                            dataset_name)
    # Save dataset result
    log_info_message("Saving results", mylogger)
    # save_processed_predictions_data(dataset_evaluation, paths, dataset_name, 
    #                                 file_path_evaluations[["data"]],
    #                                 c("rds"))
    save_processed_predictions_data(dataset_curves, paths, dataset_name, 
                                    file_path_curves[["data"]][["dataset"]],
                                    c("rds"))
    save_processed_predictions_data(class_distr_real_dataset, paths, dataset_name, 
                                    file_path_class_distributions[["data"]][["real"]],
                                    c("rds"))
    save_processed_predictions_data(class_distr_pred_dataset, paths, dataset_name, 
                                    file_path_class_distributions[["data"]][["predicted"]],
                                    c("rds"))
  }
  # Get model curves
  model_curves <- get_model_curves(curves)
  # Save all data, from all the datasets
  # save_processed_predictions_data(evaluations, paths, "ALL", 
  #                                 file_path_evaluations[["data"]],
  #                                 c("rds", "csv", "tex"))
  save_processed_predictions_data(curves, paths, "ALL", 
                                  file_path_curves[["data"]][["dataset"]],
                                  c("rds"))
  save_processed_predictions_data(model_curves, paths, "ALL", 
                                  file_path_curves[["data"]][["models"]],
                                  c("rds"))
  save_processed_predictions_data(class_distr_real, paths, "ALL", 
                                  file_path_class_distributions[["data"]][["real"]],
                                  c("rds"))
  save_processed_predictions_data(class_distr_pred, paths, "ALL", 
                                  file_path_class_distributions[["data"]][["predicted"]],
                                  c("rds"))
  return(TRUE)
}

get_dataset_evaluation <- function(predictions_per_bin) {
  dataset_evaluation <- predictions_per_bin %>%
    dplyr::group_by(Model, Noisy.Instances) %>%
    dplyr::summarise(
      Dataset.Accuracy.Predicted = eval_metric$Accuracy(Predicted, Predicted.Noisy),
      Dataset.Kappa.Predicted = eval_metric$Kappa(Predicted, Predicted.Noisy),
      Dataset.F1.Predicted = eval_metric$F1(Predicted, Predicted.Noisy),
      Dataset.Accuracy.Real = eval_metric$Accuracy(Real, Predicted.Noisy),
      Dataset.Kappa.Real = eval_metric$Kappa(Real, Predicted.Noisy),
      Dataset.F1.Real = eval_metric$F1(Real, Predicted.Noisy)
    )
  return(dataset_evaluation)
}

get_dataset_curves <- function(predictions_per_bin) {
  dataset_curves <- predictions_per_bin %>%
    dplyr::group_by(Noisy.Instances, Model, Bin, Num.Bin) %>%
    dplyr::summarise(
      Difficulty=mean(Difficulty),
      Accuracy.Predicted = eval_metric$Accuracy(Predicted, Predicted.Noisy),
      Kappa.Predicted = eval_metric$Kappa(Predicted, Predicted.Noisy),
      F1.Predicted = eval_metric$F1(Predicted, Predicted.Noisy),
      Accuracy.Real = eval_metric$Accuracy(Real, Predicted.Noisy),
      Kappa.Real = eval_metric$Kappa(Real, Predicted.Noisy),
      F1.Real = eval_metric$F1(Real, Predicted.Noisy)
    )
  return(dataset_curves)
}

create_or_append_df <- function(df, df_data, dataset_name) {
  if (is.null(df)) {
    df <- cbind(data.frame(Dataset = dataset_name), df_data)
  } else {
    df <- rbind(df, cbind(data.frame(Dataset = dataset_name), 
                          df_data))
  }
  return(df)
}

get_model_curves <- function(curves) {
  model_curves <- curves %>%
    dplyr::group_by(Model, Noisy.Instances, Num.Bin) %>%
    dplyr::summarise(
      Kappa.Predicted.Avg = mean(Kappa.Predicted),
      Kappa.Predicted.SD = sd(Kappa.Predicted)
    )
  return(model_curves)
}

save_processed_predictions_data <- function(to_save, paths, dataset_name, 
                                            path_function,
                                            extensions) {
  if ("rds" %in% extensions) {
    save_path <- path_function(paths, dataset_name, "rds")
    save_object(to_save, save_path)
  }
  if ("csv" %in% extensions) {
    save_path <- path_function(paths, dataset_name, "csv")
    write.csv(to_save, file = save_path, row.names = F)
  }
  if ("tex" %in% extensions) {
    save_path <- path_function(paths, dataset_name, "tex")
    print(xtable(to_save, type = "latex"), 
          file = save_path, include.rownames = F)
  }
  return(TRUE)
}
