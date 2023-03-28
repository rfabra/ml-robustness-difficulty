get_cluster_prototypes <- function(datasets_names, 
                                   models_data,
                                   paths,
                                   datasets_sizes,
                                   noise_instance_proportions,
                                   aggregation_methods,
                                   clustering_methods,
                                   num_clusters,
                                   mylogger = NULL) {
  path_curves <- file_path_curves[["data"]][["dataset"]](
    paths, "ALL")
  curves <- readRDS(path_curves)
  
  for (datasets_size in datasets_sizes) {
    for (aggregation_method in aggregation_methods) {
      path_aggregated_curves <- file_path_aggregated_curves[["data"]](
        paths, aggregation_method, datasets_size)
      
      aggregated_curves <- readRDS(path_aggregated_curves)
      
      for (clustering_method in clustering_methods) {
        
        prototypes <- data.frame(datasets_size = NA, aggregation_method = NA,
                                 clustering_method = NA, num_cluster = NA, prototype = NA)
        prototypes <- prototypes[0,]
        i_proto <- 1
        
        path_taxonomy <- file_path_taxonomy[["data"]](
          paths, aggregation_method, clustering_method, datasets_size, 
          num_clusters)
        taxonomy <- readRDS(path_taxonomy)
        
        
        
        for (i_cluster in 1:num_clusters) {
          models_cluster <- names(which(taxonomy$clusters == i_cluster))
          
          aggregated_curves_cluster <- 
            aggregated_curves[rownames(aggregated_curves) %in% models_cluster,]
          
          centroid <- colMeans(aggregated_curves_cluster)
          aggregated_curves_cluster[nrow(aggregated_curves_cluster) + 1,]  <- centroid
          rownames(aggregated_curves_cluster)[nrow(aggregated_curves_cluster)]  <- "centroid"
          
          dists_to_centroid <- as.matrix(dist(aggregated_curves_cluster))
          dists_to_centroid <- dists_to_centroid["centroid",]
          dists_to_centroid <- dists_to_centroid[-length(dists_to_centroid)]
          prototype <- names(which(min(dists_to_centroid) == dists_to_centroid))
          
          print(datasets_size)
          print(aggregation_method)
          print(clustering_method)
          print(i_cluster)
          print(prototype)
          print("---------------")
          
          prototypes[i_proto, "datasets_size"] <- datasets_size
          prototypes[i_proto, "aggregation_method"] <- aggregation_method
          prototypes[i_proto, "clustering_method"] <- clustering_method
          prototypes[i_proto, "num_cluster"] <- i_cluster
          if (length(prototype) > 1) {
            set.seed(123)
            prototype <- sample(prototype, 1)
          }
          prototypes[i_proto, "prototype"] <- prototype
          
          i_proto <- i_proto + 1
        }
        
        path_prototypes <- file_path_cluster_prototypes[["data"]](
          paths, aggregation_method, clustering_method, datasets_size, num_clusters)
        
        saveRDS(prototypes, path_prototypes)
        
        path_prototypes <- file_path_cluster_prototypes[["analytics"]](
          paths, aggregation_method, clustering_method, datasets_size, 
          num_clusters, extension = "tex")
        
        print(xtable::xtable(prototypes), 
              file = path_prototypes, include.rownames = F)
        
        path_prototypes <- file_path_cluster_prototypes[["analytics"]](
          paths, aggregation_method, clustering_method, datasets_size, 
          num_clusters, extension = "csv")
        
        write.csv(prototypes, file = path_prototypes, row.names = F)
        
      }
    }
  }
}