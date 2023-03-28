library(ggplot2)
library(gridExtra)

set_plot_theme <- function(plot_params) {
  return(theme(
    plot.title = element_text(size = plot_params$title$size),
    axis.title.x = element_text(size = plot_params$axis$x$title$size),
    axis.title.y = element_text(size = plot_params$axis$y$title$size),
    axis.text.x = element_text(size = plot_params$axis$x$text$size),
    axis.text.y = element_text(size = plot_params$axis$y$text$size),
    legend.title = element_text(size = plot_params$legend$title$size),
    legend.text = element_text(size = plot_params$legend$text_size),
    strip.text = element_text(size = plot_params$subplots_title_size)
  ))
}

set_plot_theme_scc <- function(plot_params, ticks_size) {
  
  return(theme(
    plot.title = element_text(size = plot_params$title$size),
    axis.title.x = element_text(size = plot_params$axis$x$title$size),
    axis.title.y = element_text(size = plot_params$axis$y$title$size),
    axis.text.x = element_text(size = plot_params$axis$x$text$size),
    axis.text.y = element_text(size = plot_params$axis$y$text$size),
    legend.title = element_text(size = plot_params$legend$title$size),
    legend.text = element_text(size = plot_params$legend$text_size),
    strip.text = element_text(size = plot_params$subplots_title_size),
    axis.
  ))
}

save_plot <- list(
  "single" = function(to_save, save_path, doc_params) {
    if (doc_params$format == "pdf") {
      pdf(save_path, width = doc_params$width, height = doc_params$height)
    } else if (doc_params$format == "png") {
      png(save_path, width = doc_params$width, height = doc_params$height, 
          res = doc_params$res, units = "in")
    }
    else {
      stop(paste("Non recognitzed format:", doc_params$format))
    }
    print(to_save)
    dev.off()
  },
  "multiple" = function(to_save, save_path, doc_params, num_cols) {
    if (doc_params$format == "pdf") {
      pdf(save_path, width = doc_params$width, height = doc_params$height)
      do.call("grid.arrange", c(to_save, ncol = num_cols))
      dev.off()
    } else if (doc_params$format == "png") {
      stop("Not implemented yet: png format")
    }
    else {
      stop(paste("Non recognitzed format:", doc_params$format))
    }
  }
)

plot_real_class_distribution <- function(class_distribution, path_plot, mylogger = NULL) {
  
  class_distribution$Noisy.Instances <- paste("Noisy\nProp.\n", class_distribution$Noisy.Instances)
  class_distribution$Num.Bin <- paste("Bin", class_distribution$Num.Bin)
  
  class_distribution <- dplyr::select(class_distribution, Num.Bin, 
                                      Real, Real.Count) %>%
    dplyr::distinct()
  p <- ggplot(class_distribution, aes(x=Real, y=Real.Count, fill=Num.Bin)) + 
    geom_bar(stat="identity") +
    xlab("Class") + ylab("Real Class Freq.") +
    theme_minimal() +
    geom_text(aes(label=Real.Count), vjust=0) +
    facet_grid(. ~ Num.Bin)

  pdf_width <- ceil(1.5 * (length(unique(class_distribution$Real)) +
                        length(unique(class_distribution$Num.Bin)) +
                        length(unique(class_distribution$Noisy.Instances))))
  
  pdf_height <- ceil(0.6 * pdf_width)
  
  log_info_message(sprintf("Saving plots in file <%s>", path_plot), mylogger)
  pdf(path_plot, width = pdf_width, height = pdf_height)
  print(p)
  dev.off()
  
  log_info_message("END plot_analytics", mylogger)
  
  return(p)
}

plot_predicted_class_distribution <- function(class_distribution,  path_plot, mylogger = NULL) {
  
  class_distribution$Noisy.Instances <- paste("Noisy\nProp.\n", class_distribution$Noisy.Instances)
  class_distribution$Num.Bin <- paste("Bin", class_distribution$Num.Bin)
  p <- ggplot(class_distribution, aes(x=Predicted.Noisy, y=Predicted.Noisy.Count, fill=Num.Bin)) + 
    geom_bar(stat="identity") +
    xlab("Class") + ylab("Predictions Class Freq.") +
    theme_minimal() +
    geom_text(aes(label=Predicted.Noisy.Count), vjust=0) +
    facet_grid(Model ~ Noisy.Instances + Num.Bin)
  log_info_message(sprintf("Saving plots in file <%s>", path_plot), mylogger)
  pdf_width <- ceil(1.5 * (length(unique(class_distribution$Predicted.Noisy)) +
    length(unique(class_distribution$Num.Bin)) +
    length(unique(class_distribution$Noisy.Instances))))
  
  pdf_height <- ceil(7 * length(unique(class_distribution$Model)))
  
pdf(path_plot, width = pdf_width, height = pdf_height)
  print(p)
  dev.off()
  
  log_info_message("END plot_analytics", mylogger)
  
  return(p)
}