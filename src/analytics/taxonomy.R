
# Plot clustering ---------------------------------------------------------


plot_taxonomy_by_config <- function(paths, clustering_type, 
                                    instances_attributes,
                                    big_small, all_datasets, plot_params) {
  if (all_datasets) {
    plot_params$title$text <- paste(plot_params$title$text,
                                    "(all datasets)")  
  } else {
    plot_params$title$text <- paste(plot_params$title$text,
                                    paste("(", big_small, " number of ", 
                                          instances_attributes, ")", sep = ""),
                                    collapse = " ")
  }
  path_clust <- file_path_taxonomy[["data"]](paths, clustering_type,
                                             instances_attributes, big_small, 
                                             all_datasets)
  
  mds_path <- file_path_taxonomy[["analytics"]](paths, clustering_type, "mds",
                                                instances_attributes, big_small, 
                                                all_datasets)
  clust <- load_object(path_clust)
  
  if (clustering_type == "hierarchical") {
    dend_path <- file_path_taxonomy[["analytics"]](paths, 
                                                   clustering_type, 
                                                   "dendrogram",
                                                   instances_attributes, 
                                                   big_small, 
                                                   all_datasets)
    
    plot_clustering[["dendrogram"]](clust, dend_path, plot_params)
    
    path_clust <- file_path_taxonomy[["data"]](paths, "agglomerative",
                                               instances_attributes, big_small, 
                                               all_datasets)
    
    agglo_clustering <- load_object(path_clust)
    agglo_clustering$clusters <- clust$clusters
    clust <- agglo_clustering
  }
  # plot_clustering[["mds"]](clust, mds_path, plot_params)
}

plot_taxonomy_by_config_difference <- function(paths, clustering_type, 
                                    instances_attributes,
                                    big_small, all_datasets, plot_params) {
  if (all_datasets) {
    plot_params$title$text <- paste(plot_params$title$text,
                                    "(all datasets)")  
  } else {
    plot_params$title$text <- paste(plot_params$title$text,
                                    paste("(", big_small, " number of ", 
                                          instances_attributes, ")", sep = ""),
                                    collapse = " ")
  }
  path_clust <- file_path_taxonomy_by_difference[["data"]](paths, clustering_type,
                                             instances_attributes, big_small, 
                                             all_datasets)
  
  mds_path <- file_path_taxonomy_by_difference[["analytics"]](paths, clustering_type, "mds",
                                                instances_attributes, big_small, 
                                                all_datasets)
  clust <- load_object(path_clust)
  
  if (clustering_type == "hierarchical") {
    dend_path <- file_path_taxonomy_by_difference[["analytics"]](paths, 
                                                   clustering_type, 
                                                   "dendrogram",
                                                   instances_attributes, 
                                                   big_small, 
                                                   all_datasets)
    plot_clustering[["dendrogram"]](clust, dend_path, plot_params)
    
    path_clust <- file_path_taxonomy_by_difference[["data"]](paths, "agglomerative",
                                               instances_attributes, big_small, 
                                               all_datasets)
    
    agglo_clustering <- load_object(path_clust)
    agglo_clustering$clusters <- clust$clusters
    clust <- agglo_clustering
  }
  plot_clustering[["mds"]](clust, mds_path, plot_params)
}

plot_taxonomy_by_config_difference_bin <- function(paths, clustering_type, 
                                               instances_attributes,
                                               big_small, all_datasets, plot_params) {
  if (all_datasets) {
    plot_params$title$text <- paste(plot_params$title$text,
                                    "(all datasets)")  
  } else {
    plot_params$title$text <- paste(plot_params$title$text,
                                    paste("(", big_small, " number of ", 
                                          instances_attributes, ")", sep = ""),
                                    collapse = " ")
  }
  path_clust <- file_path_taxonomy_by_difference_bin[["data"]](paths, clustering_type,
                                                           instances_attributes, big_small, 
                                                           all_datasets)
  
  mds_path <- file_path_taxonomy_by_difference_bin[["analytics"]](paths, clustering_type, "mds",
                                                              instances_attributes, big_small, 
                                                              all_datasets)
  clust <- load_object(path_clust)
  
  if (clustering_type == "hierarchical") {
    dend_path <- file_path_taxonomy_by_difference_bin[["analytics"]](paths, 
                                                                 clustering_type, 
                                                                 "dendrogram",
                                                                 instances_attributes, 
                                                                 big_small, 
                                                                 all_datasets)
    plot_clustering[["dendrogram"]](clust, dend_path, plot_params)
    
    path_clust <- file_path_taxonomy_by_difference_bin[["data"]](paths, "agglomerative",
                                                             instances_attributes, big_small, 
                                                             all_datasets)
    
    agglo_clustering <- load_object(path_clust)
    agglo_clustering$clusters <- clust$clusters
    clust <- agglo_clustering
  }
  #plot_clustering[["mds"]](clust, mds_path, plot_params)
}

OLD_plot_taxonomy <- function(paths, plot_params, clustering_type) {
  check_option(clustering_type, c("agglomerative", "hierarchical"))
  plot_taxonomy_by_config(paths, clustering_type, NULL, NULL, TRUE, plot_params)
  plot_taxonomy_by_config(paths, clustering_type, "instances", "big", FALSE, plot_params)
  plot_taxonomy_by_config(paths, clustering_type, "instances", "small", FALSE, plot_params)
  plot_taxonomy_by_config(paths, clustering_type, "attributes", "big", FALSE, plot_params)
  plot_taxonomy_by_config(paths, clustering_type, "attributes", "small", FALSE, plot_params)
}

plot_taxonomy_by_difference <- function(paths, plot_params, clustering_type) {
  check_option(clustering_type, c("agglomerative", "hierarchical"))
  plot_taxonomy_by_config_difference(paths, clustering_type, NULL, NULL, TRUE, plot_params)
  plot_taxonomy_by_config_difference(paths, clustering_type, "instances", "big", FALSE, plot_params)
  plot_taxonomy_by_config_difference(paths, clustering_type, "instances", "small", FALSE, plot_params)
  plot_taxonomy_by_config_difference(paths, clustering_type, "attributes", "big", FALSE, plot_params)
  plot_taxonomy_by_config_difference(paths, clustering_type, "attributes", "small", FALSE, plot_params)
}

plot_taxonomy_by_difference_bin <- function(paths, plot_params, clustering_type) {
  check_option(clustering_type, c("agglomerative", "hierarchical"))
  plot_taxonomy_by_config_difference_bin(paths, clustering_type, NULL, NULL, TRUE, plot_params)
  plot_taxonomy_by_config_difference_bin(paths, clustering_type, "instances", "big", FALSE, plot_params)
  plot_taxonomy_by_config_difference_bin(paths, clustering_type, "instances", "small", FALSE, plot_params)
  plot_taxonomy_by_config_difference_bin(paths, clustering_type, "attributes", "big", FALSE, plot_params)
  plot_taxonomy_by_config_difference_bin(paths, clustering_type, "attributes", "small", FALSE, plot_params)
}

plot_clustering <- list(
  dendrogram = function(clust_data, save_path, plot_params) {
    pdf(save_path, width= plot_params$doc$width, height= plot_params$doc$height)
    nd <- fviz_dend(clust_data$clustered, plot_params$n_clusters,
                    cex = plot_params$labels_size,
                    show_labels = TRUE,
                    color_labels_by_k = T, # color labels by groups
                    rect = T, # Add rectangle around groups
                    rect_fill = F,
                    horiz = T,
                    main = plot_params$title$text,
                    labels_track_height = 0.4,
                    rect_lty = 2,
                    lwd = 0.3
    )
    
    print(nd)
    dev.off()
  },
  mds = function(clust_data, save_path , plot_params) {
    pdf(save_path, width= plot_params$doc$width, height= plot_params$doc$height)
    clust_data$data <- remove_constant_columns(clust_data$data)
    nd <- fviz_cluster(clust_data$clustered, clust_data$data,
                       main = plot_params$title$text,
                       ggtheme = theme_minimal())
    
    print(nd)
    dev.off()
  }
)

# Plot cluster quality (silhouette and elbow) -----------------------------


plot_cluster_quality_by_config <- function(paths, clustering_type, instances_attributes,
                                           big_small, all_datasets, plot_params) {
  if (all_datasets) {
    plot_params$title$text <- paste(plot_params$title$text,
                                    "\n(", clustering_type, " clustering,",
                                    " all datasets)", sep="")  
  } else {
    plot_params$title$text <- paste(plot_params$title$text,
                                    paste("\n(", clustering_type, " clustering, ", 
                                          big_small, " number of ", 
                                          instances_attributes, 
                                          ")", sep = ""),
                                    collapse = " ")
  }
  path_clust <- file_path_taxonomy[["data"]](
    paths, clustering_type, instances_attributes, big_small, all_datasets
  )
  
  path_silhouette <- file_path_cluster_quality(
    paths, clustering_type, "silhouette",instances_attributes, big_small, 
    all_datasets
  )
  
  path_elbow <- file_path_cluster_quality(
    paths, clustering_type, "elbow", instances_attributes, big_small, 
    all_datasets
  )

  clust <- load_object(path_clust)
  if (clustering_type == "hierarchical") {
    path_clust <- file_path_taxonomy[["data"]](
      paths, "agglomerative", instances_attributes, big_small, all_datasets
    )
    agglo_clustering <- load_object(path_clust)
    agglo_clustering$clusters <- clust$clusters
    clust <- agglo_clustering
  }
    
  plot_cluster_method(clust$data, path_elbow, clustering_type, "wss", 
                      plot_params)
  plot_cluster_method(clust$data, path_silhouette, clustering_type, 
                      "silhouette", plot_params)  
}

plot_cluster_quality_by_config_difference <- function(
    paths, clustering_type, instances_attributes,
    big_small, all_datasets, plot_params) {
  
  if (all_datasets) {
    plot_params$title$text <- paste(plot_params$title$text,
                                    "\n(", clustering_type, " clustering,",
                                    " all datasets)", sep="")  
  } else {
    plot_params$title$text <- paste(plot_params$title$text,
                                    paste("\n(", clustering_type, " clustering, ", 
                                          big_small, " number of ", 
                                          instances_attributes, 
                                          ")", sep = ""),
                                    collapse = " ")
  }
  path_clust <- file_path_taxonomy_by_difference[["data"]](
    paths, clustering_type, instances_attributes, big_small, all_datasets
  )
  
  path_silhouette <- file_path_cluster_quality_by_difference(
    paths, clustering_type, "silhouette",instances_attributes, big_small, 
    all_datasets
  )
  
  path_elbow <- file_path_cluster_quality_by_difference(
    paths, clustering_type, "elbow", instances_attributes, big_small, 
    all_datasets
  )
  
  clust <- load_object(path_clust)
  if (clustering_type == "hierarchical") {
    path_clust <- file_path_taxonomy_by_difference[["data"]](
      paths, "agglomerative", instances_attributes, big_small, all_datasets
    )
    agglo_clustering <- load_object(path_clust)
    agglo_clustering$clusters <- clust$clusters
    clust <- agglo_clustering
  }
  
  plot_cluster_method(clust$data, path_elbow, clustering_type, "wss", 
                      plot_params)
  plot_cluster_method(clust$data, path_silhouette, clustering_type, 
                      "silhouette", plot_params)  
}

plot_cluster_quality_by_config_difference_bin <- function(
    paths, clustering_type, instances_attributes,
    big_small, all_datasets, plot_params) {
  
  if (all_datasets) {
    plot_params$title$text <- paste(plot_params$title$text,
                                    "\n(", clustering_type, " clustering,",
                                    " all datasets)", sep="")  
  } else {
    plot_params$title$text <- paste(plot_params$title$text,
                                    paste("\n(", clustering_type, " clustering, ", 
                                          big_small, " number of ", 
                                          instances_attributes, 
                                          ")", sep = ""),
                                    collapse = " ")
  }
  path_clust <- file_path_taxonomy_by_difference_bin[["data"]](
    paths, clustering_type, instances_attributes, big_small, all_datasets
  )
  
  path_silhouette <- file_path_cluster_quality_by_difference_bin(
    paths, clustering_type, "silhouette",instances_attributes, big_small, 
    all_datasets
  )
  
  path_elbow <- file_path_cluster_quality_by_difference_bin(
    paths, clustering_type, "elbow", instances_attributes, big_small, 
    all_datasets
  )
  
  clust <- load_object(path_clust)
  plot_cluster_method(clust$data, path_elbow, clustering_type, "wss", 
                      plot_params)
  plot_cluster_method(clust$data, path_silhouette, clustering_type, 
                      "silhouette", plot_params)  
}

OLD_plot_cluster_quality <- function(paths, plot_params, clustering_type) {
  check_option(clustering_type, c("agglomerative", "hierarchical"))
  plot_cluster_quality_by_config(paths, clustering_type, NULL, NULL, TRUE, 
                                 plot_params)
  plot_cluster_quality_by_config(paths, clustering_type, "instances", "big", FALSE, 
                                 plot_params)
  plot_cluster_quality_by_config(paths, clustering_type, "instances", "small", FALSE, 
                                 plot_params)
  plot_cluster_quality_by_config(paths, clustering_type, "attributes", "big", FALSE, 
                                 plot_params)
  plot_cluster_quality_by_config(paths, clustering_type, "attributes", "small", FALSE, 
                                 plot_params)
}

plot_cluster_quality_by_difference <- function(paths, plot_params, clustering_type) {
  check_option(clustering_type, c("agglomerative", "hierarchical"))
  plot_cluster_quality_by_config_difference(paths, clustering_type, NULL, NULL, TRUE, 
                                 plot_params)
  plot_cluster_quality_by_config_difference(paths, clustering_type, "instances", "big", FALSE, 
                                 plot_params)
  plot_cluster_quality_by_config_difference(paths, clustering_type, "instances", "small", FALSE, 
                                 plot_params)
  plot_cluster_quality_by_config_difference(paths, clustering_type, "attributes", "big", FALSE, 
                                 plot_params)
  plot_cluster_quality_by_config_difference(paths, clustering_type, "attributes", "small", FALSE, 
                                 plot_params)
}

plot_cluster_quality_by_difference_bin <- function(paths, plot_params, clustering_type) {
  check_option(clustering_type, c("agglomerative", "hierarchical"))
  plot_cluster_quality_by_config_difference_bin(paths, clustering_type, NULL, NULL, TRUE, 
                                            plot_params)
  plot_cluster_quality_by_config_difference_bin(paths, clustering_type, "instances", "big", FALSE, 
                                            plot_params)
  plot_cluster_quality_by_config_difference_bin(paths, clustering_type, "instances", "small", FALSE, 
                                            plot_params)
  plot_cluster_quality_by_config_difference_bin(paths, clustering_type, "attributes", "big", FALSE, 
                                            plot_params)
  plot_cluster_quality_by_config_difference_bin(paths, clustering_type, "attributes", "small", FALSE, 
                                            plot_params)
}

plot_cluster_method <- function(data, save_path, clustering_type, 
                                quality_method, plot_params) {
  check_option(quality_method, c("silhouette", "wss"))
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
