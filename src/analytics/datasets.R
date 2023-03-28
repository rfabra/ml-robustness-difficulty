plot_dataset_class_distribution <- function(class_distribution, paths, plot_params) {
  p <- ggplot(class_distribution, aes(x=Class, y=Count)) + 
    geom_bar(stat="identity",fill="steelblue") +
    ggtitle(plot_params$title$text) +
    xlab(plot_params$axis$x$text) + ylab(plot_params$axis$y$text) +
    theme_minimal() +
    theme(plot.title = element_text(size = plot_params$title$size),
          axis.title = element_text(size = plot_params$axis$x$title$size),
          axis.text = element_text(size = plot_params$axis$x$text$size),
          strip.text = element_text(size = plot_params$subplots_title_size)) +
    geom_text(aes(label=Count), vjust=0) +
    facet_wrap(Dataset ~ ., ncol=plot_params$num_cols, scale="free")
  
  return(p)
}
dataset_class_distribution_analytics <- function(dataset_names, paths, plot_params) {
  
  class_cols <- c("Dataset", "Class", "Count")
  class_distr <- data.frame(matrix(ncol=length(class_cols), 
                                   nrow = 0))
  colnames(class_distr) <- class_cols
  # Fill analytics data frame
  i <- 1
  for (dataset_name in dataset_names) {
    dataset <- load_object(file_path_dataset[["data"]](paths, dataset_name))
    if (dataset_name == "GesturePhaseSegmentationProcessed") {
      dataset_name <- "Gest.Phase.Segment.Proc."
    }
    dataset_class_distr <- cbind(data.frame("Dataset" = dataset_name),
                                 get_class_distribution[["Class"]](
                                   data.frame(Class = dataset$Class)))
    
    class_distr <- rbind(class_distr, dataset_class_distr)
    i <- i + 1
  }
  p <- plot_dataset_class_distribution(class_distr, paths, plot_params)
  save_path <- file_path_dataset[["analytics_class_distribution"]](
    paths, extension = "pdf")
  save_plot[["single"]](p, save_path, plot_params$doc)
}

predictions_class_distribution_analytics <- function(dataset_names, paths, plot_params) {
  
  class_cols <- c("Dataset", "Class", "Count")
  class_distr <- data.frame(matrix(ncol=length(class_cols), 
                                   nrow = 0))
  colnames(class_distr) <- class_cols
  # Fill analytics data frame
  i <- 1
  for (dataset_name in dataset_names) {
    dataset <- load_object(file_path_dataset[["data"]](paths, dataset_name))
    dataset_class_distr <- cbind(data.frame("Dataset" = dataset_name),
                                 get_class_distribution[["Class"]](
                                   data.frame(Class = dataset$Class)))
    
    class_distr <- rbind(class_distr, dataset_class_distr)
    i <- i + 1
  }
  p <- plot_dataset_class_distribution(class_distr, paths, plot_params)
  save_path <- file_path_dataset[["analytics_class_distribution"]](
    paths, extension = "pdf")
  
  save_plot[["single"]](p, save_path, plot_params$doc)
}

compute_general_datasets_analytics <- function(dataset_names, paths) {
  # Create analytics data frame
  analytics_cols <- c("Dataset", "Instances", "Attributes", "Classes")
  general_stats <- data.frame(matrix(ncol = length(analytics_cols), 
                                     nrow = length(dataset_names)))
  colnames(general_stats) <- analytics_cols
  
  i <- 1
  for (dataset_name in dataset_names) {
    print(paste("Processing dataset", dataset_name))
    dataset <- load_object(file_path_dataset[["data"]](paths, dataset_name))
    general_stats[i,] <- data.frame(dataset=dataset_name, 
                                    instances = as.integer(nrow(dataset)), 
                                    attributes = as.integer(ncol(dataset) - 1),
                                    num_classes = as.integer(length(unique(dataset$Class))))
    i <- i + 1
  }
  return(general_stats)
}

save_datasets_analytics <- function(paths, analytics_to_save, analytics_type) {
  # Check correct analytics_type
  if (analytics_type != "analytics_general" &&
      analytics_type != "analytics_per_size" ) {
    stop(paste("Wrong analytics type. Options are:", 
               "(1) analytics_general", 
               "(2) analytics_per_size",
               sep = "\n"))
  }
  
  # Save objects in the three table formats
  save_path <- file_path_dataset[[analytics_type]](paths, "rds")
  save_object(analytics_to_save, save_path, mylogger)
  save_path <- file_path_dataset[[analytics_type]](paths, "csv")
  write.csv(analytics_to_save, file = save_path, row.names = F)
  save_path <- file_path_dataset[[analytics_type]](paths, "tex")
  print(xtable(analytics_to_save, type = "latex"), 
        file = save_path, include.rownames = F,
        sanitize.text.function = function(x) {x})
}

compute_stats_per_size <- function(general_stats) {
  stats_per_size <- 
    general_stats %>% 
    dplyr::mutate("Size (instances)" = cut2(Instances, g = 2)) %>%
    dplyr::mutate("Size (attributes)" = cut2(Attributes, g = 2))
  small_inst <- levels(stats_per_size[["Size (instances)"]])[1]
  small_attr <- levels(stats_per_size[["Size (attributes)"]])[1]
  # Assign "Big" or "Small" to instances and attributes
  cut_val_inst <- tail(str_extract_all(small_inst, "\\d+")[[1]], n = 1)
  stats_per_size[["Size (instances)"]] <- apply(stats_per_size, 1, function(x) {
    if (x[["Size (instances)"]] == small_inst) 
      x["Size (instances)"] <- paste("$>", cut_val_inst, "$", sep = " ")
    else 
      x["Size (instances)"] <-  paste("$\\leq", cut_val_inst, "$", sep = " ")
  }
  )
  cut_val_attr <- tail(str_extract_all(small_attr, "\\d+")[[1]], n = 1)
  stats_per_size[["Size (attributes)"]] <- apply(stats_per_size, 1, function(x) {
    if (x[["Size (attributes)"]] == small_attr)
      x["Size (attributes)"] <-  paste("$<", cut_val_attr, "$", sep = " ") #"Small"
    else 
      x["Size (attributes)"] <- paste("$\\geq", cut_val_attr, "$", sep = " ") #"Big"}
  }
  )
  
  return(stats_per_size)
}

compute_datasets_stats <- function(dataset_names, 
                                   paths,
                                   mylogger = NULL) {
  # Initialize datasets anlytics
  general_stats <- compute_general_datasets_analytics(dataset_names, paths)
  # Save general analytics
  save_datasets_analytics(paths, general_stats, "analytics_general")
  # Compute stats per size
  stats_per_size <- compute_stats_per_size(general_stats)
  # Save analytics per size
  save_datasets_analytics(paths, stats_per_size, "analytics_per_size")
  
  return(TRUE)
}