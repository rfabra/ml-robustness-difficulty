aggregate_curves <- function(datasets_names,
                             models_data,
                             paths,
                             datasets_sizes,
                             noise_instance_proportions,
                             aggregation_methods,
                             mylogger = NULL) {
  for (datasets_size in datasets_sizes) {
    for (aggregation_method in aggregation_methods) {
      
      path_aggregated_curves <- file_path_aggregated_curves[["data"]](
        paths, aggregation_method, datasets_size)
      
      if (file.exists(path_aggregated_curves)) {
        log_info_message(paste("File <", path_aggregated_curves, 
                               "> already processed. Skipping...", 
                               sep = ""), mylogger)
        next
      }
      
      aggregated_curves <- aggregate_curves_by[[aggregation_method]](
        datasets_names,
        datasets_size,
        models_data,
        noise_instance_proportions,
        paths,
        mylogger)
      save_path <- file_path_aggregated_curves[["data"]](paths, aggregation_method, 
                                                         datasets_size)
      log_info_message(paste("Saving in", save_path), mylogger)
      saveRDS(aggregated_curves, save_path)
    }
  }
}

aggregate_curves_by <- list(
  average = function(datasets_names,
                     datasets_size,
                     models_data,
                     noise_instance_proportions,
                     paths,
                     mylogger = NULL) {
    datasets_names <- filter_datasets_per_size(datasets_names, datasets_size, 
                                               paths)
    aggregated_data <- NULL
    for (dataset_name in datasets_names) {
      log_info_message(paste("Processing dataset", dataset_name), mylogger)
      path_robustness_curves <- 
        file_path_curves[["data"]][["dataset"]](paths, dataset_name)
      robustness_curves <- readRDS(path_robustness_curves)
      
      difficulty_bins <- sort(unique(robustness_curves$Bin))
      
      column_names <- unique(paste(sprintf("Bin.%i", robustness_curves$Num.Bin),
                                   sprintf(".Noisy.%s", noise_instance_proportions), sep=""))
      if (is.null(aggregated_data)) {
        aggregated_data <- data.frame(matrix(ncol=length(column_names), nrow=length(models_data)))
        colnames(aggregated_data) <- column_names
        rownames(aggregated_data) <- models_data
        aggregated_data[is.na(aggregated_data)] <- 0
      }
      for (model_name in models_data) {
        log_info_message(paste("Processing model", model_name), mylogger)
        for (noise_instance in noise_instance_proportions) {
          log_info_message(paste("Processing noise instance", noise_instance), mylogger)
          for (d_bin in difficulty_bins) {
            i_bin <- which(d_bin == difficulty_bins)
            df_result <-
              robustness_curves %>%
              dplyr::filter(Model == model_name, Num.Bin == i_bin, Noisy.Instances == noise_instance)
            col_idx <- sprintf("Bin.%i.Noisy.%s", i_bin, noise_instance)
            aggregated_data[model_name, col_idx] <-
              aggregated_data[model_name, col_idx] + df_result$Kappa.Predicted
          }
        }
      }
    }
    aggregated_data <- aggregated_data/length(datasets_names)
    return(aggregated_data)
  },
  standard_deviation = function(datasets_names,
                                datasets_size,
                                models_data,
                                noise_instance_proportions,
                                paths,
                                mylogger = NULL) {
    
    datasets_names <- filter_datasets_per_size(datasets_names, datasets_size, 
                                               paths)
    # Average -----------------------------------------------------------------
    
    path_aggregated_curves_average <- file_path_aggregated_curves[["data"]](
      paths, "average", datasets_size)
    
    if (file.exists(path_aggregated_curves_average)) {
      average_data <- readRDS(path_aggregated_curves_average)
    } else {
      average_data <- aggregate_curves_by[["average"]](datasets_names,
                                                       datasets_size,
                                                       models_data,
                                                       noise_instance_proportions,
                                                       paths,
                                                       mylogger)
    }
    sd_data <- NULL
    for (dataset_name in datasets_names) {
      log_info_message(paste("Processing dataset", dataset_name), mylogger)
      path_robustness_curves <- 
        file_path_curves[["data"]][["dataset"]](paths, dataset_name)
      robustness_curves <- readRDS(path_robustness_curves)
      difficulty_bins <- sort(unique(robustness_curves$Bin))
      
      column_names <- unique(paste(sprintf("Bin.%i", robustness_curves$Num.Bin),
                                   sprintf(".Noisy.%s", noise_instance_proportions), sep=""))
      if (is.null(sd_data)) {
        sd_data <- data.frame(matrix(ncol=length(column_names), nrow=length(models_data)))
        colnames(sd_data) <- column_names
        rownames(sd_data) <- models_data
        sd_data[is.na(sd_data)] <- 0
      }
      for (model_name in models_data) {
        log_info_message(paste("Processing model", model_name), mylogger)
        for (noise_instance in noise_instance_proportions) {
          log_info_message(paste("Processing noise instance", noise_instance), mylogger)
          for (d_bin in difficulty_bins) {
            i_bin <- which(d_bin == difficulty_bins)
            df_result <-
              robustness_curves %>%
              dplyr::filter(Model == model_name, Num.Bin == i_bin, 
                            Noisy.Instances == noise_instance)
            col_idx <- sprintf("Bin.%i.Noisy.%s", i_bin, noise_instance)
            sd_data[model_name, col_idx] <-
              sd_data[model_name, col_idx] + 
              (df_result$Kappa.Predicted - average_data[model_name, col_idx]) ^ 2
          }
        }
      }
    }
    sd_data <- sqrt(sd_data / length(datasets_names))
    
    return(sd_data)
  },
  difference_across_noise = function(datasets_names,
                                     datasets_size,
                                     models_data,
                                     noise_instance_proportions,
                                     paths,
                                     mylogger = NULL) {
    
    datasets_names <- filter_datasets_per_size(datasets_names, datasets_size, 
                                               paths)
    aggregated_data <- NULL
    for (dataset_name in datasets_names) {
      log_info_message(paste("Processing dataset", dataset_name), mylogger)
      path_robustness_curves <- 
        file_path_curves[["data"]][["dataset"]](paths, dataset_name)
      
      robustness_curves <- readRDS(path_robustness_curves)
      robustness_curves$Model <-
        sapply(robustness_curves$Model, FUN = function(x) {
          return(models_data[[x]])
        })
      
      difficulty_bins <- sort(unique(robustness_curves$Bin))
      noise_pairs <- c()
      for (i in 1:length(noise_instance_proportions)) {
        if (i + 1 <= length(noise_instance_proportions)) {
          noise_pairs <- c(noise_pairs, paste(c(noise_instance_proportions[i], 
                                                noise_instance_proportions[i+1]), collapse=".To."))
        }
      }
      column_names <- c()
      for (d_bin in difficulty_bins) {
        i_bin <- which(d_bin == difficulty_bins)
        column_names <- c(column_names, 
                          paste(sprintf("Bin.%i.", i_bin),
                                sprintf("Noisy.From.%s", noise_pairs), sep=""))
      }
      
      if (is.null(aggregated_data)) {
        aggregated_data <- data.frame(matrix(ncol=length(column_names), nrow=length(models_data)))
        colnames(aggregated_data) <- column_names
        rownames(aggregated_data) <- models_data
        aggregated_data[is.na(aggregated_data)] <- 0
      }
      for (model_name in models_data) {
        log_info_message(paste("Processing model", model_name), mylogger)
        
        for (noise_instance in noise_pairs) {
          log_info_message(paste("Processing noise instance", noise_instance), mylogger)
          min_noise <- str_extract_all(noise_instance, "[0-9].[0-9]+")[[1]][1]
          max_noise <- str_extract_all(noise_instance, "[0-9].[0-9]+")[[1]][2]
          
          for (d_bin in difficulty_bins) {
            i_bin <- which(d_bin == difficulty_bins)
            df_result_min <-
              robustness_curves %>%
              dplyr::filter(Model == model_name, Num.Bin == i_bin, 
                            Noisy.Instances == min_noise)
            df_result_max <-
              robustness_curves %>%
              dplyr::filter(Model == model_name, Num.Bin == i_bin, 
                            Noisy.Instances == max_noise)
            
            col_idx <- sprintf("Bin.%i.Noisy.From.%s.To.%s", i_bin, min_noise, 
                               max_noise)
            aggregated_data[model_name, col_idx] <-
              aggregated_data[model_name, col_idx] + 
              df_result_min$Kappa.Predicted - df_result_max$Kappa.Predicted
            
          }
        }
      }
    }
    aggregated_data <- aggregated_data/length(datasets_names)
    
    return(aggregated_data)
  },
  difference_across_bins = function(datasets_names,
                                    datasets_size,
                                    models_data,
                                    noise_instance_proportions,
                                    paths,
                                    mylogger = NULL) {
    datasets_names <- filter_datasets_per_size(datasets_names, datasets_size, 
                                               paths)
    aggregated_data <- NULL
    for (dataset_name in datasets_names) {
      log_info_message(paste("Processing dataset", dataset_name), mylogger)
      path_robustness_curves <- 
        file_path_curves[["data"]][["dataset"]](paths, dataset_name)
      
      robustness_curves <- readRDS(path_robustness_curves)
      
      robustness_curves$Model <-
        sapply(robustness_curves$Model, FUN = function(x) {
          return(models_data[[x]])
        })
      
      difficulty_bins <- sort(unique(robustness_curves$Bin))
      
      num_bins <- length(difficulty_bins)
      bin_pairs <- c()
      for (i_bin in 1:num_bins) {
        if (i_bin + 1 <= num_bins) {
          bin_pairs <- c(bin_pairs, paste(c(i_bin,
                                            i_bin + 1), collapse=".To."))
        }
      }
      column_names <- c()
      
      for (i_noise in noise_instance_proportions) {
        column_names <- c(column_names, 
                          paste(sprintf("Noisy.%s.", i_noise),
                                sprintf("Bin.From.%s", bin_pairs), sep=""))
      }
      if (is.null(aggregated_data)) {
        aggregated_data <- data.frame(matrix(ncol=length(column_names), nrow=length(models_data)))
        colnames(aggregated_data) <- column_names
        rownames(aggregated_data) <- models_data
        aggregated_data[is.na(aggregated_data)] <- 0
      }
      for (model_name in models_data) {
        log_info_message(paste("Processing model", model_name), mylogger)
        for (p_bin in bin_pairs) {
          log_info_message(paste("Processing bin pair ", p_bin), mylogger)
          min_bin <- str_extract_all(p_bin, "[0-9]")[[1]][1]
          max_bin <- str_extract_all(p_bin, "[0-9]")[[1]][2]
          
          for (noise_instance in noise_instance_proportions) {
            df_result_min <-
              robustness_curves %>%
              dplyr::filter(Model == model_name, Num.Bin == min_bin, 
                            Noisy.Instances == noise_instance)
            df_result_max <-
              robustness_curves %>%
              dplyr::filter(Model == model_name, Num.Bin == max_bin, 
                            Noisy.Instances == noise_instance)
            
            col_idx <- sprintf("Noisy.%s.Bin.From.%s.To.%s", noise_instance, min_bin, 
                               max_bin)
            
            aggregated_data[model_name, col_idx] <-
              aggregated_data[model_name, col_idx] + 
              df_result_max$Kappa.Predicted - df_result_min$Kappa.Predicted
          }
        }
      }
    }
    aggregated_data <- aggregated_data/length(datasets_names)
    
    return(aggregated_data)
    
  },
  difference_across_noise_and_bins = function(datasets_names,
                                    datasets_size,
                                    models_data,
                                    noise_instance_proportions,
                                    paths,
                                    mylogger = NULL) {
    datasets_names <- filter_datasets_per_size(datasets_names, datasets_size, 
                                               paths)
    path_aggregated_curves_across_noise <- file_path_aggregated_curves[["data"]](
      paths, "difference_across_noise", datasets_size)
    
    if (file.exists(path_aggregated_curves_across_noise)) {
      curves_across_noise <- readRDS(path_aggregated_curves_across_noise)
    } else {
      curves_across_noise <- aggregate_curves_by[["difference_across_noise"]](
        datasets_names, datasets_size, models_data,noise_instance_proportions,
        paths, mylogger)
    }
    
    path_aggregated_curves_across_bins <- file_path_aggregated_curves[["data"]](
      paths, "difference_across_bins", datasets_size)
    
    if (file.exists(path_aggregated_curves_across_bins)) {
      curves_across_bins <- readRDS(path_aggregated_curves_across_bins)
    } else {
      curves_across_bins <- aggregate_curves_by[["difference_across_bins"]](
        datasets_names, datasets_size, models_data,bins_instance_proportions,
        paths, mylogger)
    }
    
    return(cbind(curves_across_noise, curves_across_bins))
  }
)

compute_stats_per_size_dend <- function(general_stats) {
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
      x["Size (instances)"] <- "Small"
    else 
      x["Size (instances)"] <- "Big"
  }
  )
  cut_val_attr <- tail(str_extract_all(small_attr, "\\d+")[[1]], n = 1)
  stats_per_size[["Size (attributes)"]] <- apply(stats_per_size, 1, function(x) {
    if (x[["Size (attributes)"]] == small_attr)
      x["Size (attributes)"] <-  "Small"
    else 
      x["Size (attributes)"] <- "Big"
  }
  )
  
  return(stats_per_size)
}

filter_datasets_per_size <- function(datasets_names, datasets_size, paths) {
  
  if (datasets_size == "ALL") {
    return(datasets_names)
  }
  datasets_stats <- compute_general_datasets_analytics(datasets_names, paths)
  stats_per_size <- compute_stats_per_size_dend(datasets_stats)
  colnames(stats_per_size)[colnames(stats_per_size) == "Size (instances)"] <- "inst.size"
  colnames(stats_per_size)[colnames(stats_per_size) == "Size (attributes)"] <- "attr.size"
  if (datasets_size == "big_instances") {
    return(dplyr::filter(stats_per_size, inst.size == "Big")$Dataset)
  } else if (datasets_size == "small_instances") {
    return(dplyr::filter(stats_per_size, inst.size == "Small")$Dataset)
  } else if (datasets_size == "big_attributes") {
    return(dplyr::filter(stats_per_size, attr.size == "Big")$Dataset)
  } else if (datasets_size == "small_attributes") {
    return(dplyr::filter(stats_per_size, attr.size == "Small")$Dataset)
  }
}