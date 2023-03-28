analytics_datasets_evaluation <- function(datasets_names, model_names, paths, 
                            noise_magnitude, mylogger = config$logger) {
  
  all_preds <- data.frame(
    Dataset = NA,
    row_id = NA, 
    Fold = NA, 
    Model = NA, 
    Real = NA, 
    Predicted = NA, 
    Predicted.Noisy = NA, 
    Difficulty = NA, 
    Bin = NA, 
    Noisy.Instances = NA, 
    Num.Bin = NA)
  
  all_preds <- all_preds[0,]
  
  for (dataset_name in datasets_names) {
    path_predictions_per_bin <- 
      file_path_predictions_per_bin[["data"]](paths, dataset_name, noise_magnitude)
    
    predictions_per_bin <- load_object(path_predictions_per_bin)
    predictions_per_bin <- cbind(data.frame(Dataset = dataset_name), 
                                 predictions_per_bin)
    
    all_preds <- rbind(all_preds, predictions_per_bin)
  }
  
  dataset_evaluation <- all_preds %>% 
    dplyr::group_by(Dataset, Model, Noisy.Instances) %>%
    dplyr::summarise(
      Dataset.Accuracy.Real = eval_metric$Accuracy(Real, Predicted.Noisy),
      Dataset.Kappa.Real = eval_metric$Kappa(Real, Predicted.Noisy)#,
    )
  save_path <- file_path_evaluations[["analytics"]](paths, "ALL", "rds")
  save_object(dataset_evaluation, save_path, mylogger)
  save_path <- file_path_evaluations[["analytics"]](paths, "ALL",  "csv")
  write.csv(dataset_evaluation, file = save_path, row.names = F)
  save_path <- file_path_evaluations[["analytics"]](paths, "ALL",  "tex")
  print(xtable(dataset_evaluation, type = "latex"), 
        file = save_path, include.rownames = F)
  
  return(TRUE)
}