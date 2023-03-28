plot_robustness_curves_by_prototype <- function(datasets_names,
                                              models_data,
                                              paths,
                                              curve_type,
                                              plot_params,
                                              datasets_sizes,
                                              aggregation_methods,
                                              clustering_methods,
                                              num_clusters,
                                              mylogger = NULL) {
  y_axis_name <- get_y_axis[[curve_type]]
  
  path_curves <- file_path_curves[["data"]][["dataset"]](
    paths, "ALL")
  
  all_curves <- readRDS(path_curves)
  all_curves$Model <-
    sapply(all_curves$Model, FUN = function(x) {
      return(models_data[[x]])
    })
  all_curves[all_curves$Dataset == "GesturePhaseSegmentationProcessed",]$Dataset <- 
    "Gest.Phase.Segment.Proc."
  for (datasets_size in datasets_sizes) {
    datasets_per_size <- filter_datasets_per_size(datasets_names, datasets_size, 
                                                  paths)
    
    for (aggregation_method in aggregation_methods) {
      
      for (clustering_method in clustering_methods) {
        path_taxonomy <- file_path_taxonomy[["data"]](
          paths, aggregation_method, clustering_method, datasets_size, 
          num_clusters)
        taxonomy <- readRDS(path_taxonomy)
        
        
        for (i_cluster in 1:num_clusters) {
          plots <- list()
          i_plot <- 1  
          
          path_prototypes <- file_path_cluster_prototypes[["data"]](
            paths, aggregation_method, clustering_method, datasets_size, 
            num_clusters, extension = "rds")
          
          prototypes <- readRDS(path_prototypes)
          model_prototype <- dplyr::filter(prototypes, num_cluster == i_cluster)$prototype
          prototype_curves <- dplyr::filter(all_curves, Model == model_prototype)
          p <- ggplot(data=prototype_curves, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) + 
            geom_line(data=prototype_curves, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) +
            geom_point(data=prototype_curves, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) +
            ggtitle(model_prototype) + 
            labs(col=plot_params$legend$title$text) +
            xlab(plot_params$axis$x$title$text) +
            ylab(plot_params$axis$y$title$text) +
            facet_wrap(vars(Dataset), ncol=plot_params$num_col, scales = "free_x") +
            theme_minimal() + 
            set_plot_theme(plot_params) 
          
          save_path <- file_path_curves_prototype(
            paths, aggregation_method, clustering_method, datasets_size, 
            num_clusters, i_cluster, extension = "pdf")
          
          pdf(save_path, width = plot_params$doc$width, height = plot_params$doc$height)
          print(p)
          dev.off()
        }
      }
    }
  }
}

plot_robustness_curves_by_cluster <- function(datasets_names,
                                   model_names,
                                   paths,
                                   curve_type,
                                   plot_params,
                                   datasets_sizes,
                                   aggregation_methods,
                                   clustering_methods,
                                   num_clusters,
                                   mylogger = NULL) {
  y_axis_name <- get_y_axis[[curve_type]]
  for (datasets_size in datasets_sizes) {
    datasets_per_size <- filter_datasets_per_size(datasets_names, datasets_size, 
                                                  paths)
    
    for (aggregation_method in aggregation_methods) {
      
      for (clustering_method in clustering_methods) {
        path_taxonomy <- file_path_taxonomy[["data"]](
          paths, aggregation_method, clustering_method, datasets_size, 
          num_clusters)
        taxonomy <- readRDS(path_taxonomy)
        
        
        for (i_cluster in 1:num_clusters) {
          plots <- list()
          i_plot <- 1  
          for (dataset_name in datasets_per_size) {
            models_cluster <- names(which(taxonomy$clusters == i_cluster))
            
            path_curves <- file_path_curves[["data"]][["dataset"]](
              paths, dataset_name)
            
            cluster_curves <- readRDS(path_curves)
            
            cluster_curves <- dplyr::filter(cluster_curves, Model %in% models_cluster)
            
            
            
            
            p <- ggplot(data=cluster_curves, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) + 
              geom_line(data=cluster_curves, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) +
              geom_point(data=cluster_curves, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) +
              ggtitle(dataset_name) + 
              labs(col=plot_params$legend$title$text) +
              xlab(plot_params$axis$x$title$text) +
              ylab(plot_params$axis$y$title$text) +
              facet_wrap(vars(Model), ncol=plot_params$num_cols, scales = "fixed") +
              theme_minimal() + 
              set_plot_theme(plot_params)
            # p
            plots[[i_plot]] <- p
            i_plot <- i_plot + 1
            
            
            saveRDS(taxonomy, path_taxonomy)
          }
          
          save_path <- file_path_curves[["analytics"]][[curve_type]](
            paths, paste(datasets_size, aggregation_method, clustering_method, 
                         sep="-"), 
            num_cluster = i_cluster, extension = "pdf")
          plot_params$title$text <- paste(plot_params$title$text, 
                                          " (cluster", i_cluster, ")", sep = "")
          pdf(save_path, width = plot_params$doc$width, height = plot_params$doc$height)
          
          final_plot <- ggarrange(plotlist = plots, ncol = 1) + ggtitle(plot_params$title$text)
          print(final_plot)
          dev.off()
        }
        
      }
    }
  }
}

plot_robustness_curves <- function(datasets_names,
                                   models_data,
                                   paths,
                                   curve_type,
                                   datasets_sizes,
                                   plot_params,
                                   mylogger = NULL) {

  log_info_message("Plotting robustness curves...", mylogger)
  y_axis_name <- get_y_axis[[curve_type]]
  model_names <- names(models_data)
  for (datasets_size in datasets_sizes) {
    datasets_per_size <- filter_datasets_per_size(datasets_names, datasets_size, 
                                                  paths)
    plots <- list()
    i_plot <- 1
    for (dataset_name in datasets_per_size) {
      path_robustness_curves_ds <-
        file_path_curves[["data"]][["dataset"]](paths, dataset_name,
                                                "rds")
      robustness_curves_ds <- load_object(path_robustness_curves_ds)
      robustness_curves_ds <- dplyr::filter(robustness_curves_ds, Model %in% model_names)
      
      robustness_curves_ds$Model <-
        sapply(robustness_curves_ds$Model, FUN = function(x) {
          return(models_data[[x]])
        })
      p <- ggplot(data=robustness_curves_ds, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) +
        geom_line(data=robustness_curves_ds, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) +
        geom_point(data=robustness_curves_ds, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) +
        ggtitle(dataset_name) +
        labs(col=plot_params$legend$title$text) +
        xlab(plot_params$axis$x$title$text) +
        ylab(plot_params$axis$y$title$text) +
        facet_wrap(vars(Model), ncol=plot_params$num_cols, scales = "fixed") +
        theme_minimal() +
        set_plot_theme(plot_params)
      plots[[i_plot]] <- p
      i_plot <- i_plot + 1
      
    }
    n_plots <- i_plot - 1
    plot_page <- data.frame(plot=1:n_plots, page=ceil(1:n_plots / 
                                                         (plot_params$plots_per_page)))
    pages <- sort(unique(plot_page$page))
    for (page in pages) {
      print(paste("Processing page", page))
      plots_idx <- plot_page[plot_page$page == page,]
      x <- plots[plots_idx$plot]
      n_plots_this_page <- length(x)
      height_ratio <- (plot_params$doc$height * n_plots_this_page) / 
        plot_params$plots_per_page 
      
      save_path <- file_path_curves[["analytics"]][[curve_type]](
        paths, paste(datasets_size, page, sep="-"), extension = "pdf")
      pdf(save_path, width = plot_params$doc$width, height = height_ratio)
      final_plot <- ggarrange(plotlist = x, ncol = 1) + ggtitle(plot_params$title$text)
      print(final_plot)
      dev.off()
    }
  }
  log_info_message("END plot robustness curves", mylogger)
}

plot_single_scc <- function(dataset_name, models_data, paths, curve_type, 
                            plot_params, mylogger = NULL) {
  y_axis_name <- get_y_axis[[curve_type]]
  model_names <- names(models_data)
  path_robustness_curves_ds <-
    file_path_curves[["data"]][["dataset"]](paths, dataset_name,
                                            "rds")
  robustness_curves_ds <- load_object(path_robustness_curves_ds)
  robustness_curves_ds <- dplyr::filter(robustness_curves_ds, Model %in% model_names)
  
  robustness_curves_ds$Model <-
    sapply(robustness_curves_ds$Model, FUN = function(x) {
      return(models_data[[x]])
    })
  p <- ggplot(data=robustness_curves_ds, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) +
    geom_line(data=robustness_curves_ds, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) +
    geom_point(data=robustness_curves_ds, aes_string(x="Difficulty", y=y_axis_name, colour="Noisy.Instances")) +
    ggtitle(dataset_name) +
    labs(col=plot_params$legend$title$text) +
    xlab(plot_params$axis$x$title$text) +
    ylab(plot_params$axis$y$title$text) +
    facet_wrap(vars(Model), ncol=plot_params$num_cols, scales = "fixed") +
    theme_minimal() +
    set_plot_theme(plot_params) + 
  
  save_path <- file_path_curves[["analytics"]][[curve_type]](
    paths, paste(dataset_name, model_names[[1]], sep="-"), extension = "pdf")
  pdf(save_path, width = plot_params$doc$width, height = plot_params$doc$height)
  print(p)
  dev.off()
}

plot_CCC_per_model <- function(datasets_names,
                               model_names,
                               paths,
                               plot_params,
                               mylogger = NULL) {
  
  path_robustness_curves <- 
    file_path_curves[["data"]][["dataset"]](paths, "ALL", 
                                            "rds")
  robustness_curves <- load_object(path_robustness_curves)
  
  robustness_curves <- dplyr::filter(robustness_curves, Model %in% model_names &
                                       Noisy.Instances == 0)
  
  p <- ggplot(data=robustness_curves, aes_string(x="Difficulty", y="Accuracy.Real", colour="Model")) + 
    geom_line(data=robustness_curves, aes_string(x="Difficulty", y="Accuracy.Real", colour="Model")) +
    geom_point(data=robustness_curves, aes_string(x="Difficulty", y="Accuracy.Real", colour="Model")) +
    ggtitle(plot_params$title$text) + 
    labs(col=plot_params$legend$title$text) +
    xlab(plot_params$axis$x$title$text) +
    ylab(plot_params$axis$y$title$text) +
    facet_wrap(vars(Dataset), ncol=plot_params$num_cols) +
    theme_minimal() + 
    set_plot_theme(plot_params)
  
  save_path <- file_path_curves[["analytics"]][["CCC"]](paths, "ALL", "pdf")
  save_plot[["single"]](p, save_path, plot_params$doc)
  
  return(TRUE)
}

get_y_axis <- list(
  "SCC_Kappa" = "Kappa.Predicted",
  "SCC_Acc" = "Accuracy.Predicted",
  "CCC_Kappa" = "Kappa.Real",
  "CCC_Acc" = "Accuracy.Real"
)

plot_model_curves <- function(datasets_names,
                              model_names,
                              paths,
                              plot_params,
                              mylogger = NULL) {
  path_robustness_curves <- file_path_curves[["data"]][["models"]](paths, "ALL", "rds")
  all_robustness_curves <- load_object(path_robustness_curves)
  all_robustness_curves <- dplyr::filter(all_robustness_curves, Model %in% model_names)
  all_robustness_curves <- all_robustness_curves[complete.cases(all_robustness_curves),]
  num_noisy_params <- length(unique(all_robustness_curves$Noisy.Instances))
  p <- ggplot(all_robustness_curves, aes(x=Num.Bin, y=Kappa.Predicted.Avg, colour=Noisy.Instances)) + 
    geom_line(data=all_robustness_curves, aes(x=Num.Bin, y=Kappa.Predicted.Avg, colour=Noisy.Instances)) +
    geom_point(data=all_robustness_curves, aes(x=Num.Bin, y=Kappa.Predicted.Avg, colour=Noisy.Instances)) +
    geom_errorbar(aes(ymin=Kappa.Predicted.Avg-Kappa.Predicted.SD, ymax=Kappa.Predicted.Avg+Kappa.Predicted.SD)) + #, width=.2, position=position_dodge(.9)) +
    ggtitle(plot_params$title$text) +
    labs(col=plot_params$legend$title$text) +
    xlab(plot_params$axis$x$title$text) +
    ylab(plot_params$axis$y$title$text) +
    facet_wrap(vars(Noisy.Instances, Model), ncol=plot_params$num_cols) +
    theme_minimal() +
    set_plot_theme(plot_params)

  path_plot <- file_path_curves[["analytics"]][["SCC_avg"]](paths, "ALL", "pdf")
  log_info_message(sprintf("Saving plots in file <%s>", path_plot), mylogger)
  save_plot[["single"]](p, path_plot, plot_params$doc)
  
  log_info_message("END plot_analytics", mylogger)
  return(p)
}