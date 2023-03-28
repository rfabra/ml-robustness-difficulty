plot_cluster_quality_multiple_clusters <- function(paths,
                                                   datasets_sizes,
                                                   aggregation_methods,
                                                   clustering_methods,
                                                   num_clusters,
                                                   plot_params,
                                                   mylogger = NULL) {
  metrics_names <- c("average.between", 
                     "within.cluster.ss", 
                     "avg.silwidth",
                     "DB",
                     "dunn",
                     "dunn2")
  analytics_table <- data.frame(matrix(nrow = length(metrics_names) * 
                                         length(aggregation_methods),
                                       ncol = num_clusters + 1))
  colnames(analytics_table) <- c("Metrics", "Method", 2:num_clusters)
  i_row <- 1
  for (datasets_size in datasets_sizes) {
    for (metric in metrics_names) {
    for (aggregation_method in aggregation_methods) {
      
      for (clustering_method in clustering_methods) {
        
          analytics_table[i_row, "Metrics"] <- metric
          analytics_table[i_row, "Method"] <- aggregation_method
          for (i_cluster in 2:num_clusters) {
            # Load curves
            path_aggregated_curves <- file_path_aggregated_curves[["data"]](
              paths, aggregation_method, datasets_size)
            aggregated_curves <- readRDS(path_aggregated_curves)
            
            # Clustering
            res <- hcut(aggregated_curves, k = i_cluster)
            # Clustering stats
            if (metric == "DB") {
              davies_index <- index.DB(aggregated_curves, res$cluster, centrotypes="centroids")
              analytics_table[i_row, i_cluster + 1] <- davies_index$DB
            } else {
              hc_stats <- cluster.stats(dist(aggregated_curves),  res$cluster)  
              analytics_table[i_row, i_cluster + 1] <- hc_stats[[metric]]
            }
          }
          i_row <- i_row + 1
        }
      }
    }
    save_path <- file_path_cluster_quality_multiple_clusters(
      paths, clustering_method, datasets_size, num_clusters,
      extension = "csv")
    write.csv(analytics_table, file = save_path, row.names = F)
    
    save_path <- file_path_cluster_quality_multiple_clusters(
      paths, clustering_method, datasets_size, num_clusters,
      extension = "tex")
    print(xtable::xtable(analytics_table), 
          file = save_path, include.rownames = F)
  }
}

plot_cluster_quality <- function(paths,
                                 datasets_sizes,
                                 aggregation_methods,
                                 clustering_methods,
                                 num_clusters,
                                 plot_params,
                                 mylogger = NULL) {
  for (datasets_size in datasets_sizes) {
    for (aggregation_method in aggregation_methods) {
      for (clustering_method in clustering_methods) {
        path_aggregated_curves <- file_path_aggregated_curves[["data"]](
          paths, aggregation_method, datasets_size)
        aggregated_curves <- readRDS(path_aggregated_curves)
        
        path_quality <- file_path_cluster_quality(
          paths, aggregation_method, clustering_method, "elbow",
          datasets_size, num_clusters)
        plot_cluster_method(aggregated_curves, path_quality, clustering_method,
                            "elbow", plot_params)
        path_cluster_validation <- file_path_cluster_validation(
          paths, aggregation_method, clustering_method, datasets_size,
          num_clusters)
        
        
        res <- hcut(aggregated_curves, k = num_clusters)
        browser()
        
        hc_stats <- cluster.stats(dist(aggregated_curves),  res$cluster)
        hc_stats
        
        x <- index.DB(aggregated_curves, res$cluster, centrotypes="centroids")
        
        p <- fviz_silhouette(res, palette = "jco", ggtheme = theme_classic())
        print(p)
      }
    }
  }
}

plot_cluster_method <- function(data, save_path, clustering_type, 
                                quality_method, plot_params) {
  check_option(quality_method, c("silhouette", "elbow"))
  if (quality_method == "elbow") quality_method <- "wss"
  check_option(clustering_type, c("hierarchical", "agglomerative"))
  pdf(save_path, width= plot_params$doc$width, height= plot_params$doc$height)
  if (clustering_type == "hierarchical") {
    x <- fviz_nbclust(data, hcut, method = quality_method, 
                      k.max = length(rownames(data))-1) + 
      ggtitle(plot_params$title$text)
    print(x)
  } else {
    x <- fviz_nbclust(data, kmeans, method = quality_method,
                      k.max = length(rownames(data))-1) + 
      ggtitle(plot_params$title$text)  
    print(x)
  }
  
  dev.off()
}