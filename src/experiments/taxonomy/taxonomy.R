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



# Clustering -------------------------------------------------

compute_taxonomies <- function(datasets_names, 
                               models,
                               noise_instance_proportions,
                               hierarchical_agglomerative,
                               num_clusters,
                               paths,
                               mylogger = NULL) {
  log_info_message("Computing taxonomy...", mylogger)
  datasets_stats <- compute_general_datasets_analytics(datasets_names, paths)
  stats_per_size <- compute_stats_per_size_dend(datasets_stats)
  colnames(stats_per_size)[colnames(stats_per_size) == "Size (instances)"] <- "inst.size"
  colnames(stats_per_size)[colnames(stats_per_size) == "Size (attributes)"] <- "attr.size"
  perform_clustering(
    stats_per_size$Dataset, names(models), noise_instance_proportions,
    paths, hierarchical_agglomerative, "average", num_clusters, NULL, NULL, TRUE, mylogger = mylogger)
  
  perform_clustering(
    dplyr::filter(stats_per_size, inst.size == "Small")$Dataset,
    names(models), noise_instance_proportions, paths, hierarchical_agglomerative,
    "average", num_clusters, "instances", "small", FALSE,
    mylogger = mylogger)
  
  perform_clustering(
    dplyr::filter(stats_per_size, inst.size == "Big")$Dataset,
    names(models), noise_instance_proportions, paths, hierarchical_agglomerative,
    "average", num_clusters, "instances", "big", FALSE,
    mylogger = mylogger)
  
  perform_clustering(
    dplyr::filter(stats_per_size, attr.size == "Small")$Dataset,
    names(models), noise_instance_proportions, paths, hierarchical_agglomerative,
    "average", num_clusters, "attributes", "small", FALSE,
    mylogger = mylogger)
  
  perform_clustering(
    dplyr::filter(stats_per_size, attr.size == "Big")$Dataset,
    names(models), noise_instance_proportions, paths, hierarchical_agglomerative,
    "average", num_clusters, "attributes", "big", FALSE,
    mylogger = mylogger)
  
  return(TRUE)
}

compute_taxonomies_by_difference <- function(datasets_names, 
                                             models,
                                             noise_instance_proportions,
                                             hierarchical_agglomerative,
                                             num_clusters,
                                             paths,
                                             mylogger = NULL) {
  log_info_message("Computing taxonomy by difference...", mylogger)
  datasets_stats <- compute_general_datasets_analytics(datasets_names, paths)
  stats_per_size <- compute_stats_per_size_dend(datasets_stats)
  colnames(stats_per_size)[colnames(stats_per_size) == "Size (instances)"] <- "inst.size"
  colnames(stats_per_size)[colnames(stats_per_size) == "Size (attributes)"] <- "attr.size"
  
  perform_clustering(
    stats_per_size$Dataset, names(models), noise_instance_proportions,
    paths, hierarchical_agglomerative, "difference", 
    num_clusters, NULL, NULL, TRUE, mylogger = mylogger)
  
  perform_clustering(
    dplyr::filter(stats_per_size, inst.size == "Small")$Dataset,
    names(models), noise_instance_proportions, paths, hierarchical_agglomerative,
    "difference", num_clusters, "instances", "small", FALSE,
    mylogger = mylogger)
  
  perform_clustering(
    dplyr::filter(stats_per_size, inst.size == "Big")$Dataset,
    names(models), noise_instance_proportions, paths, hierarchical_agglomerative,
    "difference", num_clusters, "instances", "big", FALSE,
    mylogger = mylogger)
  
  perform_clustering(
    dplyr::filter(stats_per_size, attr.size == "Small")$Dataset,
    names(models), noise_instance_proportions, paths, hierarchical_agglomerative,
    "difference", num_clusters, "attributes", "small", FALSE,
    mylogger = mylogger)
  
  perform_clustering(
    dplyr::filter(stats_per_size, attr.size == "Big")$Dataset,
    names(models), noise_instance_proportions, paths, hierarchical_agglomerative,
    "difference", num_clusters, "attributes", "big", FALSE,
    mylogger = mylogger)
  
  return(TRUE)
}

compute_taxonomies_by_difference_bin <- function(datasets_names, 
                                                 models,
                                                 noise_instance_proportions,
                                                 hierarchical_agglomerative,
                                                 num_clusters,
                                                 paths,
                                                 mylogger = NULL) {
  log_info_message("Computing taxonomy by difference...", mylogger)
  datasets_stats <- compute_general_datasets_analytics(datasets_names, paths)
  stats_per_size <- compute_stats_per_size_dend(datasets_stats)
  colnames(stats_per_size)[colnames(stats_per_size) == "Size (instances)"] <- "inst.size"
  colnames(stats_per_size)[colnames(stats_per_size) == "Size (attributes)"] <- "attr.size"
  perform_clustering(
    stats_per_size$Dataset, names(models), noise_instance_proportions,
    paths, hierarchical_agglomerative, "bin", 
    num_clusters, NULL, NULL, TRUE, mylogger = mylogger)
  
  perform_clustering(
    dplyr::filter(stats_per_size, inst.size == "Small")$Dataset,
    names(models), noise_instance_proportions, paths, hierarchical_agglomerative,
    "bin", num_clusters, "instances", "small", FALSE,
    mylogger = mylogger)
  
  perform_clustering(
    dplyr::filter(stats_per_size, inst.size == "Big")$Dataset,
    names(models), noise_instance_proportions, paths, hierarchical_agglomerative,
    "bin", num_clusters, "instances", "big", FALSE,
    mylogger = mylogger)
  
  perform_clustering(
    dplyr::filter(stats_per_size, attr.size == "Small")$Dataset,
    names(models), noise_instance_proportions, paths, hierarchical_agglomerative,
    "bin", num_clusters, "attributes", "small", FALSE,
    mylogger = mylogger)
  
  perform_clustering(
    dplyr::filter(stats_per_size, attr.size == "Big")$Dataset,
    names(models), noise_instance_proportions, paths, hierarchical_agglomerative,
    "bin", num_clusters, "attributes", "big", FALSE,
    mylogger = mylogger)
  
  return(TRUE)
}
