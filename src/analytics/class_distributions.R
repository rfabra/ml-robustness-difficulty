class_distribution_analytics <- function(dataset_names, paths, model_names, 
                                         mode, plot_params) {
  
  if (!(mode %in% c("real", "predicted"))) {
    stop("mode parameter must be either <real> or <predicted>")
  }
  
  if (mode == "real") {
    if (plot_params$num_pages > 1) {
      plots <- list()
      i_plot <- 1
      for (dataset_name in dataset_names) {
        path_class_distr <- file_path_class_distributions[["data"]][["real"]](
          paths, dataset_name, "rds")
        class_distr <- load_object(path_class_distr)
        
        first_model <- model_names[[1]]
        class_distr <- class_distr %>%
          dplyr::filter(Model == first_model, Noisy.Instances == 0.0)
        class_distr <- cbind(class_distr, data.frame(Dataset=dataset_name))
        p <- ggplot(class_distr, aes(x=Real, y=Real.Count)) + 
          geom_bar(stat="identity",fill="steelblue") +
          ggtitle(plot_params$title$text) +
          xlab(plot_params$axis$x$text) + ylab(plot_params$axis$y$text) +
          theme_minimal() +
          set_plot_theme(plot_params) +
          geom_text(aes(label=Real.Count), vjust=0) +
          facet_wrap(vars(Dataset, Num.Bin), ncol=plot_params$num_cols, scale="free")
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
        
        save_path <- file_path_class_distributions[["analytics"]][["real"]](
              paths, paste("ALL", page, sep="-"), "pdf")
          
        pdf(save_path, width = plot_params$doc$width, height = height_ratio)
        final_plot <- ggarrange(plotlist = x, ncol = 1) + ggtitle(plot_params$title$text)
        print(final_plot)
        dev.off()
      }
    }
  }
    # } else {
    
    
    
    # n_plots <- i_plot - 1
    # plot_page <- data.frame(plot=1:n_plots, page=cut2(1:n_plots, g=plot_params$num_pages))
    # i <- 1
    # i_page <- 1
    # 
    # for (page in levels(plot_page$page)) {
    #   print(paste("Processing page", i_page))
    #   plots_idx <- plot_page[plot_page$page == page,]
    #   x <- plots[plots_idx$plot]
    #   
    #   save_path <- file_path_class_distributions[["analytics"]][["real"]](
    #     paths, paste("ALL", i_page, sep="-"), "pdf")
    #   pdf(save_path, width = plot_params$doc$width, height = plot_params$doc$height)
    #   final_plot <- ggarrange(plotlist = x, ncol = 1) + ggtitle(plot_params$title$text)
    #   print(final_plot)
    #   dev.off()
    #   
    #   i_page <- i_page + 1
    # }
    
    #     first_model <- model_names[[1]]
    #     path_class_distr <- file_path_class_distributions[["data"]][[mode]](
    #       paths, "ALL", "rds")
    #     class_distr <- load_object(path_class_distr)
    #     class_distr <- class_distr %>%
    #       dplyr::filter(Model == first_model, Noisy.Instances == 0.0)
    #     p <- ggplot(class_distr, aes(x=Real, y=Real.Count)) + 
    #       geom_bar(stat="identity",fill="steelblue") +
    #       ggtitle(plot_params$title$text) +
    #       xlab(plot_params$axis$x$text) + ylab(plot_params$axis$y$text) +
    #       theme_minimal() +
    #       set_plot_theme(plot_params) +
    #       geom_text(aes(label=Real.Count), vjust=0) +
    #       facet_wrap(vars(Dataset, Noisy.Instances, Num.Bin), ncol=plot_params$num_cols, scale="free")
    #     
    #     save_path <- file_path_class_distributions[["analytics"]][[mode]](
    #       paths, "ALL", "pdf")
    #     save_plot[["single"]](p, save_path, plot_params$doc)
    #   }
    # } else {
    #   for (dataset_name in dataset_names) {
    #     path_class_distr <- file_path_class_distributions[["data"]][["predicted"]](
    #       paths, dataset_name, "rds")
    #     class_distr <- load_object(path_class_distr)
    #     browser()
    #     p <- ggplot(class_distr, aes(x=Predicted.Noisy, y=Predicted.Noisy.Count, fill=Num.Bin)) + 
    #       geom_bar(stat="identity") +
    #       ggtitle(dataset_name) +
    #       xlab(plot_params$axis$x$text) + ylab(plot_params$axis$y$text) +
    #       theme_minimal() +
    #       set_plot_theme(plot_params) +
    #       geom_text(aes(label=Predicted.Noisy.Count), vjust=0) +
    #       facet_wrap(vars(Model, Noisy.Instances, Num.Bin), ncol=plot_params$num_cols, scale="free")
    #     
    #     save_path <- file_path_class_distributions[["analytics"]][[mode]](
    #       paths, dataset_name, "pdf")
    #     
    #     pdf(save_path, width = plot_params$doc$width, height = plot_params$doc$height)
    #     print(p)
    #     dev.off()
    #   }
    # }
  }