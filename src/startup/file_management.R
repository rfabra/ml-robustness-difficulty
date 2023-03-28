library(fs)

build_path <- function(base_path, data, extension) {
  return(path(base_path,
              sprintf("%s.%s",
                      paste(data, collapse="_"), 
                      extension)))
}

file_path_locks <- function(paths, dataset_name, stage="") {
  if (stage != "") {
    return(build_path(paths$locks, c(dataset_name, stage), "locked"))
  }
  return(build_path(paths$locks, c(dataset_name), "locked"))
}

file_path_dataset <- list(
  data = function(paths, dataset_name, extension = "rds") { 
    return(build_path(path(paths$data$datasets), c(dataset_name), extension)) 
  },
  analytics_general = function(paths, extension = "rds") {
    return(build_path(path(paths$analytics$datasets), c("general"), extension)) 
  },
  analytics_per_size = function(paths, extension = "rds") {
    return(build_path(path(paths$analytics$datasets), c("size"), extension)) 
  },
  analytics_class_distribution = function(paths, extension = "pdf") {
    return(build_path(path(paths$analytics$datasets), c("class_distribution"), extension)) 
  }
)

file_path_runs <- list(
  data = function(paths, dataset_name, extension = "rds") { 
    return(build_path(path(paths$data$runs), c(dataset_name), extension)) 
  },
  analytics = function(paths, dataset_name, extension = "rds") {
    return(build_path(path(paths$analytics$runs), c(dataset_name), extension)) 
  }
)

file_path_response_matrix <- list(
  data = function(paths, dataset_name, num_models, extension = "rds") { 
    return(build_path(path(paths$data$irt), c(dataset_name, "response-matrix", 
                                              num_models), extension)) 
  },
  analytics = function(paths, dataset_name, num_models, extension = "rds") {
    return(build_path(path(paths$analytics$irt), c(dataset_name, "response-matrix", 
                                                   num_models), extension)) 
  }
)

file_path_IRT <- list(
  data = function(paths, dataset_name, irt, extension = "rds") { 
    return(build_path(path(paths$data$irt), c(dataset_name, "IRT", 
                                              irt$num_models, irt$n_cycles), extension)) 
  },
  analytics = function(paths, dataset_name, irt, extension = "rds") {
    return(build_path(path(paths$analytics$irt), c(dataset_name, "IRT", 
                                                   irt$num_models, irt$n_cycles), extension)) 
  }
)

file_path_difficulty <- list(
  data = function(paths, dataset_name, irt, extension = "rds") { 
    return(build_path(path(paths$data$irt), c(dataset_name, "difficulty", 
                                              irt$num_models, irt$n_cycles), extension)) 
  },
  analytics = function(paths, dataset_name, irt, extension = "rds") {
    return(build_path(path(paths$analytics$irt), c(dataset_name, "difficulty", 
                                                   irt$num_models, irt$n_cycles), extension)) 
  },
  table_mean = function(paths, dataset_name, irt, extension = "rds") {
    return(build_path(path(paths$analytics$irt), c(dataset_name, "difficulty-mean", 
                                                   irt$num_models, irt$n_cycles), extension)) 
  }
)

file_path_noisy_dataset <- list(
  data = function(paths, dataset_name, fold, noise_magnitude, 
                  noise_instance_proportion, extension = "rds") {
    return(build_path(
      paths$data$noisy_datasets, 
      c(dataset_name, noise_magnitude, fold, noise_instance_proportion), 
      extension))
  },
  analytics = function(paths, dataset_name, noise_magnitude, 
                       noise_instance_proportion, extension = "rds") {
    return(build_path(
      paths$analytics$noisy_datasets, 
      c(dataset_name, noise_magnitude, fold, noise_instance_proportion), 
      extension))
  }
)

file_path_model <- list(
  data = function(paths, dataset_name, model_name, fold, extension = "rds") {
    return(build_path(
      paths$data$models, 
      c(dataset_name, model_name, fold), 
      extension))
  },
  analytics = function(paths, dataset_name, model_name, fold, extension = "rds") {
    return(build_path(
      paths$analytics$models, 
      c(dataset_name, model_name, fold), 
      extension))
  }
)

file_path_predictions <- list(
  data = function(paths, dataset_name,
                  model_name, noise_instance_proportion, fold, extension = "rds") {
    return(build_path(paths$data$predictions, 
                      c(dataset_name, model_name, fold, 
                        noise_instance_proportion), 
                      extension))
  },
  analytics = function(paths, dataset_name,
                       model_name, noise_instance_proportion, fold, extension = "rds") {
    return(build_path(paths$analytics$predictions, 
                      c(dataset_name, model_name, fold, 
                        noise_instance_proportion), 
                      extension))
  }
)

file_path_predictions_per_bin <- list(
  data = function(paths, dataset_name,
                  noise_magnitude, extension = "rds") {
    return(build_path(paths$data$predictions_per_bin, 
                      c(dataset_name,
                        noise_magnitude), 
                      extension))
  },
  analytics = function(paths, dataset_name,
                       noise_magnitude, extension = "rds") {
    return(build_path(paths$analytics$predictions_per_bin, 
                      c(dataset_name,
                        noise_magnitude), 
                      extension))
  }
)

file_path_evaluations <- list(
  data = function(paths, dataset_name, extension = "rds") {
    return(build_path(paths$data$evaluations, c(dataset_name, "evaluation"), 
                      extension))
  },
  analytics = function(paths, dataset_name, extension = "rds") {
    return(build_path(paths$analytics$evaluations, c(dataset_name, "evaluation"), 
                      extension))
  }
)

file_path_curves <- list(
  data = list(
    dataset = function(paths, dataset_name, extension = "rds") {
      return(build_path(paths$data$curves, 
                        c(dataset_name, "curves"), extension))
    },
    models = function(paths, dataset_name, extension = "rds") {
      return(build_path(paths$data$curves, 
                        c(dataset_name, "models_avg", "curves"), extension))
    }),
  analytics = list(
    CCC_Acc = function(paths, dataset_name, extension = "pdf") {
      return(build_path(paths$analytics$curves, 
                        c(dataset_name, "CCC_Acc"), extension))
    },
    CCC_Kappa = function(paths, dataset_name, extension = "pdf") {
      return(build_path(paths$analytics$curves, 
                        c(dataset_name, "CCC_Kappa"), extension))
    },
    SCC_Acc = function(paths, dataset_name, extension = "pdf") {
      return(build_path(paths$analytics$curves, 
                        c(dataset_name, "SCC_Acc"), extension))
    },
    SCC_Kappa = function(paths, dataset_name, num_cluster = NULL, extension = "pdf") {
      if (!is.null(num_cluster)){
        num_cluster <- paste("cluster", num_cluster, sep = "_")
        return(build_path(paths$analytics$curves_by_cluster, 
                          c(dataset_name, "SCC_Kappa", num_cluster), extension))
      } else {
        return(build_path(paths$analytics$curves, 
                          c(dataset_name, "SCC_Kappa"), extension))  
      }
      
    },
    SCC_avg = function(paths, dataset_name, extension = "pdf") {
      return(build_path(paths$analytics$curves, 
                        c(dataset_name, "AVG_SCC_Kappa"), extension))
    },
    CCC = function(paths, dataset_name, extension = "pdf") {
      return(build_path(paths$analytics$curves, 
                        c(dataset_name, "CCC"), extension))
    }
  )
)

file_path_class_distributions <- list(
  data = list(
    real = function(paths, dataset_name, extension = "rds") {
      return(build_path(paths$data$class_distributions, 
                        c(dataset_name, "real", "class_ditributions"), extension))
    },
    predicted = function(paths, dataset_name, extension = "rds") {
      return(build_path(paths$data$class_distributions, 
                        c(dataset_name, "predicted", "class_ditributions"), extension))
    }
  ),
  analytics = list(
    real = function(paths, dataset_name, extension = "pdf") {
      return(build_path(paths$analytics$class_distributions, 
                        c(dataset_name, "real", "class_ditributions"), extension))
    },
    predicted = function(paths, dataset_name, extension = "pdf") {
      return(build_path(paths$analytics$class_distributions, 
                        c(dataset_name, "predicted", "class_ditributions"), extension))
    }
  )
)

file_path_aggregated_curves <- list(
  data = function(
    paths, aggregation_method, datasets_size, extension = "rds"
  ) {
    check_option(aggregation_method, c("average", "standard_deviation",
                                       "difference_across_noise", 
                                       "difference_across_bins", 
                                       "difference_across_noise_and_bins"))
    
    check_option(datasets_size, c("ALL", "big_instances","small_instances", 
                                  "big_attributes", "small_attributes"))
    
    return(build_path(paths$data$aggregate_curves, 
                      c(datasets_size, aggregation_method), 
                      extension))
  },
  analytics = function(
    paths, aggregation_method, datasets_size, extension = "rds"
  ) {
    check_option(aggregation_method, c("average", "standard_deviation",
                                       "difference_across_noise", 
                                       "difference_across_bins", 
                                       "difference_across_noise_and_bins"))
    
    check_option(datasets_size, c("ALL", "big_instances","small_instances", 
                                  "big_attributes", "small_attributes"))
    
    return(build_path(paths$analytics$aggregate_curves, 
                      c(datasets_size, aggregation_method), 
                      extension))
  })

file_path_taxonomy <- list(
  data = function(paths,
                  aggregation_method,
                  clustering_method,
                  datasets_size,
                  num_clusters,
                  extension = "rds") {
    check_option(aggregation_method, c("average", "standard_deviation",
                                       "difference_across_noise", 
                                       "difference_across_bins", 
                                       "difference_across_noise_and_bins"))
    check_option(clustering_method, c("hierarchical", "agglomerative")) 
    check_option(datasets_size, c("ALL", "big_instances","small_instances", 
                                  "big_attributes", "small_attributes"))
    
    return(build_path(paths$data[[clustering_method]], 
                      c(datasets_size, clustering_method, num_clusters, 
                        aggregation_method), extension))
  },
  analytics = function(paths,
                       aggregation_method,
                       clustering_method,
                       datasets_size,
                       num_clusters,
                       extension = "pdf") {
    check_option(aggregation_method, c("average", "standard_deviation",
                                       "difference_across_noise", 
                                       "difference_across_bins", 
                                       "difference_across_noise_and_bins"))
    check_option(clustering_method, c("hierarchical", "agglomerative")) 
    check_option(datasets_size, c("ALL", "big_instances","small_instances", 
                                  "big_attributes", "small_attributes"))
    
    return(build_path(paths$analytics[[clustering_method]], 
                      c(datasets_size, clustering_method, num_clusters, 
                        aggregation_method), extension))
  }
)

file_path_cluster_quality <- function(paths,
                                      aggregation_method,
                                      clustering_method,
                                      quality_method,
                                      datasets_size,
                                      num_clusters,
                                      extension = "pdf") {
  
  check_option(aggregation_method, c("average", "standard_deviation",
                                     "difference_across_noise", 
                                     "difference_across_bins", 
                                     "difference_across_noise_and_bins"))
  check_option(clustering_method, c("hierarchical", "agglomerative")) 
  check_option(datasets_size, c("ALL", "big_instances","small_instances", 
                                "big_attributes", "small_attributes"))
  check_option(quality_method, c("elbow", "silhouette"))
  
  return(build_path(paths$analytics$cluster_quality, 
                    c(datasets_size, clustering_method, num_clusters, 
                      aggregation_method, quality_method), extension))
}

file_path_cluster_quality_multiple_clusters <- function(paths,
                                      clustering_method,
                                      datasets_size,
                                      num_clusters,
                                      extension = "rds") {
  
  check_option(clustering_method, c("hierarchical", "agglomerative")) 
  check_option(datasets_size, c("ALL", "big_instances","small_instances", 
                                "big_attributes", "small_attributes"))
  
  return(build_path(paths$analytics$cluster_quality, 
                    c(datasets_size, clustering_method, num_clusters, "table_validation"), extension))
}

file_path_dist_matrix <- function(paths,
                                      aggregation_method,
                                      clustering_method,
                                      datasets_size,
                                      num_clusters,
                                      extension = "pdf") {
  
  check_option(aggregation_method, c("average", "standard_deviation",
                                     "difference_across_noise", 
                                     "difference_across_bins", 
                                     "difference_across_noise_and_bins"))
  check_option(clustering_method, c("hierarchical", "agglomerative")) 
  check_option(datasets_size, c("ALL", "big_instances","small_instances", 
                                "big_attributes", "small_attributes"))
  
  return(build_path(paths$analytics$cluster_quality, 
                    c(datasets_size, clustering_method, num_clusters, 
                      aggregation_method, "dist_matrix"), extension))
}

file_path_cluster_validation <- function(paths,
                                  aggregation_method,
                                  clustering_method,
                                  datasets_size,
                                  num_clusters,
                                  extension = "pdf") {
  
  check_option(aggregation_method, c("average", "standard_deviation",
                                     "difference_across_noise", 
                                     "difference_across_bins", 
                                     "difference_across_noise_and_bins"))
  check_option(clustering_method, c("hierarchical", "agglomerative")) 
  check_option(datasets_size, c("ALL", "big_instances","small_instances", 
                                "big_attributes", "small_attributes"))
  
  return(build_path(paths$analytics$cluster_quality, 
                    c(datasets_size, clustering_method, num_clusters, 
                      aggregation_method, "cluster_validation"), extension))
}

file_path_cluster_prototypes   <- list(
  data = function(paths,
                  aggregation_method,
                  clustering_method,
                  datasets_size,
                  num_clusters,
                  extension = "rds") {
    
    
    check_option(aggregation_method, c("average", "standard_deviation",
                                       "difference_across_noise", 
                                       "difference_across_bins", 
                                       "difference_across_noise_and_bins"))
    check_option(clustering_method, c("hierarchical", "agglomerative")) 
    check_option(datasets_size, c("ALL", "big_instances","small_instances", 
                                  "big_attributes", "small_attributes"))
    
    return(build_path(paths$data$cluster_prototypes, 
                      c(datasets_size, clustering_method, num_clusters, 
                        aggregation_method, "prototypes"), extension))
  },
  analytics = function(paths,
                  aggregation_method,
                  clustering_method,
                  datasets_size,
                  num_clusters,
                  extension = "tex") {
    
    
    check_option(aggregation_method, c("average", "standard_deviation",
                                       "difference_across_noise", 
                                       "difference_across_bins", 
                                       "difference_across_noise_and_bins"))
    check_option(clustering_method, c("hierarchical", "agglomerative")) 
    check_option(datasets_size, c("ALL", "big_instances","small_instances", 
                                  "big_attributes", "small_attributes"))
    
    return(build_path(paths$analytics$cluster_prototypes, 
                      c(datasets_size, clustering_method, num_clusters, 
                        aggregation_method, "prototypes"), extension))
  }
)

file_path_curves_prototype <- function(paths,
                     aggregation_method,
                     clustering_method,
                     datasets_size,
                     num_clusters,
                     num_cluster,
                     extension = "pdf") {
  
  
  check_option(aggregation_method, c("average", "standard_deviation",
                                     "difference_across_noise", 
                                     "difference_across_bins", 
                                     "difference_across_noise_and_bins"))
  check_option(clustering_method, c("hierarchical", "agglomerative")) 
  check_option(datasets_size, c("ALL", "big_instances","small_instances", 
                                "big_attributes", "small_attributes"))
  
  return(build_path(paths$analytics$curves_prototypes, 
                    c(datasets_size, clustering_method, num_clusters, 
                      aggregation_method, num_cluster), extension))
}

load_object <- function(path_load, object_name = "object", mylogger=NULL) {
  loaded_object <- NULL
  log_info_message(sprintf("Loading %s from <%s>", object_name, path_load), mylogger)
  if (!is.null(path_load)) {
    loaded_object <- tryCatch(
      {
        readRDS(path_load)
      },
      error = function(e) {
        if (grepl("cannot open the connection", e)) {
          log_info_message(
            paste("The", object_name, "in file", paste("<", path_load, ">", sep=""), 
                  "does not exist."), 
            mylogger)
        }
        else {
          log_error_message(e, mylogger)
        }
        return(NULL)
      }
    )
  }
  return(loaded_object)
}

save_object <- function(object_to_save, save_path = NULL, 
                        object_name="object", mylogger=NULL) {
  if (!is.null(save_path)) {
    log_info_message(paste("Saving", object_name, "in", 
                           paste("<", save_path, ">", sep = "")), 
                     mylogger)
    saveRDS(object_to_save, file=save_path)
  }
}
