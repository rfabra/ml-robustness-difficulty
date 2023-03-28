compute_response_matrix_stats <- function(datasets_names,
                                          datasets_path,
                                          in_path,
                                          out_path = NULL,
                                          mylogger = NULL) {
  datasets_stats <- compute_datasets_stats(datasets_names,
                                           datasets_path,
                                           mylogger = mylogger)
  
  analytics_cols <- c("Dataset", "Instances", "Attributes")
  
  datasets_stats <- data.frame(matrix(ncol=length(analytics_cols), nrow=length(dataset_names)))
  colnames(datasets_stats) <- analytics_cols
  i <- 1
  for (dataset_name in dataset_names) {
    
    dataset <- load_object(file_path_dataset(in_path, dataset_name))
    datasets_stats[i,] <- data.frame(dataset=dataset_name, 
                                     instances = as.integer(nrow(dataset)), 
                                     attributes = as.integer(ncol(dataset) - 1))
    i <- i + 1
  }
  datasets_stats <- 
    datasets_stats %>% 
    dplyr::mutate("Size (instances)" = cut2(Instances, g = 2)) %>%
    dplyr::mutate("Size (attributes)" = cut2(Attributes, g = 2))
  
  small_inst <- unique(datasets_stats[["Size (instances)"]])[1]
  small_attr <- unique(datasets_stats[["Size (attributes)"]])[1]
  
  datasets_stats[["Size (instances)"]] <- apply(datasets_stats, 1, function(x) {
    if (x[["Size (instances)"]] == small_inst)
      x["Size (instances)"] <- "Small"
    else 
      x["Size (instances)"] <- "Big"}
  )
  
  datasets_stats[["Size (attributes)"]] <- apply(datasets_stats, 1, function(x) {
    if (x[["Size (attributes)"]] == small_attr)
      x["Size (attributes)"] <- "Small"
    else 
      x["Size (attributes)"] <- "Big"}
  )
  
  save_path <- path(out_path, "all_datasets.rds")
  save_object(datasets_stats, save_path, mylogger)

  save_path <- path(out_path, "all_datasets.csv")
  write.csv(datasets_stats, file = save_path, row.names = F)
  
  save_path <- path(out_path, "all_datasets.tex")
  print(xtable(datasets_stats, type = "latex"), 
        file = save_path, include.rownames = F)
    
  return(datasets_stats)
}
