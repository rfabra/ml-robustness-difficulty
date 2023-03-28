# Plot clustering ---------------------------------------------------------
plot_taxonomy <- function(paths,
                          models_data,
                          datasets_sizes,
                          aggregation_methods,
                          clustering_methods,
                          num_clusters,
                          plot_params,
                          mylogger = NULL) {
  for (datasets_size in datasets_sizes) {
    for (aggregation_method in aggregation_methods) {
      for (clustering_method in clustering_methods) {
        path_taxonomy <- file_path_taxonomy[["data"]](
          paths, aggregation_method, clustering_method, datasets_size, 
          num_clusters)
        
        taxonomy <- load_taxonomy(
          paths, aggregation_method, clustering_method, datasets_size, 
          num_clusters)
        
        names(taxonomy$clusters) <- 
          normalize_model_names(names(taxonomy$clusters), models_data)
        path_plot_taxonomy <- file_path_taxonomy[["analytics"]](
          paths, aggregation_method, clustering_method, datasets_size, num_clusters, 
          extension = "png")
        
        print(paste("Computing dendrogram to save in", path_plot_taxonomy))  
        plot_clustering[[clustering_method]](
          taxonomy, path_plot_taxonomy, num_clusters, plot_params[[clustering_method]])
      }
    }
  }
}

load_taxonomy <- function(paths, aggregation_method, clustering_method, 
                          datasets_size, num_clusters) {
  path_taxonomy <- file_path_taxonomy[["data"]](
    paths, aggregation_method, clustering_method, datasets_size, 
    num_clusters)
  taxonomy <- readRDS(path_taxonomy)
  if (clustering_method == "hierarchical") {
    return(taxonomy)
  }
  # Agglomerative
  path_aggregated_curves <- file_path_aggregated_curves[["data"]](
    paths, aggregation_method, datasets_size)
  aggregated_curves <- readRDS(path_aggregated_curves)
  
  return(list(data = aggregated_curves,
              cluster_object = taxonomy$cluster_object,
              clusters = taxonomy$clusters))
}

plot_clustering <- list(
  hierarchical = function(taxonomy, save_path, num_clusters, plot_params) {
    
    png(save_path, width=350, height=700, res = 200)
    nd <- fviz_dend(taxonomy$cluster_object, num_clusters, 
                    cex = plot_params$labels_size,
                    show_labels = TRUE,
                    color_labels_by_k = T, # color labels by groups
                    rect = T, # Add rectangle around groups
                    rect_fill = F,
                    horiz = T,
                    main = "",
                    labels_track_height = 0.01,
                    rect_lty = 2,
                    lwd = 0.3
    )
    
    print(nd)
    dev.off()
  },
  agglomerative = function(clust_data, save_path, num_clusters, plot_params) {
    pdf(save_path, width= plot_params$doc$width, height= plot_params$doc$height)
    clust_data$data <- remove_constant_columns(clust_data$data)
    nd <- fviz_cluster(clust_data$cluster_object, clust_data$data,
                       main = plot_params$title$text,
                       ggtheme = theme_minimal())
    
    print(nd)
    dev.off()
  }
)