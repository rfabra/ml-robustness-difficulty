perform_clustering <- function(datasets_names, 
                               models_data,
                               paths,
                               datasets_sizes,
                               noise_instance_proportions,
                               aggregation_methods,
                               clustering_methods,
                               num_clusters,
                               mylogger = NULL) {
  
  for (datasets_size in datasets_sizes) {
    for (aggregation_method in aggregation_methods) {
      for (clustering_method in clustering_methods) {
        
        path_taxonomy <- file_path_taxonomy[["data"]](
          paths, aggregation_method, clustering_method, datasets_size, 
          num_clusters)
        
        if (file.exists(path_taxonomy)) {
          log_info_message(paste("File <", path_taxonomy, 
                                 "> already processed. Skipping...", 
                                 sep = ""), mylogger)
          next
        }
        
        path_aggregated_curves <- file_path_aggregated_curves[["data"]](
          paths, aggregation_method, datasets_size)
        
        aggregated_curves <- readRDS(path_aggregated_curves)
        taxonomy <- do_clustering[[clustering_method]](
          aggregated_curves,
          num_clusters,
          mylogger)
        
        saveRDS(taxonomy, path_taxonomy)
      }
    }
  }
}

do_clustering <- list(
  hierarchical = function(aggregated_curves, num_clusters, mylogger = NULL) {
    # Hierarchical
    log_info_message("Hierarchical clustering", mylogger)
    clust <- hclust(dist(aggregated_curves))
    clusters <- cutree(clust, k = num_clusters)
    rownames(clusters) <- NULL
    return(list(cluster_object = clust, clusters = clusters))
  },
  agglomerative = function(aggregated_curves, num_clusters, mylogger = NULL) {
    # Agglomerative
    log_info_message("Agglomerative clustering", mylogger)
    clust <- kmeans(aggregated_curves, num_clusters)
    clusters <- clust$cluster
    rownames(clusters) <- NULL
    return(list(cluster_object = clust, clusters = clusters))
  }
)