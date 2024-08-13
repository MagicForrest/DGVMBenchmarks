#' Plots all comparison objects from fullSpatialComparison as refined difference plots
#'
#' @param Benchmark Benchmark holds the settings for the benchmark currently being performed used to set range parameters to the plotting.
#' @param all_comparisons List holding all comparison objects
#' @param type A character specifying what type of plot to make. Can be "difference" (default, for a difference plot), "percentage.difference", "values" (actual values, side-by-side).
#' 
#' @return Returns difference for every possible comparison in your comparison list.
#' @export
#'
#' @examples plotAllSpatialComparisons(Benchmark = this_benchmark, all_comparisons = all_comparisons)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
plotAllSpatialComparisons <- function(Benchmark, all_comparisons, type = "difference") {

  if (type == "difference"){
  if (length(Benchmark@guess_layers) < 2){
  for (i in seq_along(all_comparisons[["Values"]])) {
    this_comparison <- all_comparisons[["Values"]][[i]]

    
  if (this_comparison@source2@id == "Obs"){
    #Quick plot of comparison object
    DGVMTools::plotSpatialComparison(this_comparison, type = type)
  
    #Get the Max, Min, Mean difference of Model - Observed data
    max_dif<-round(max(this_comparison@data[[5]]),2)
    mean_dif<-round(mean(this_comparison@data[[5]]),2)
    min_dif<-round(min(this_comparison@data[[5]]),2)
    mean_obs<-round(mean(this_comparison@data[[4]]),2)
    # Plotting difference maps
    limits = range(this_comparison@data[[5]]) 
    breaks = pretty(range(this_comparison@data[[5]]))
    if (!is.null(Benchmark@difference_limits) && length(Benchmark@difference_limits) != 0){
      limits = unlist(Benchmark@difference_limits)
    }
    if (!is.null(Benchmark@difference_breaks) && length(Benchmark@difference_breaks) != 0){
      breaks = unlist(Benchmark@difference_breaks)
    }
    
    p1 <- DGVMTools::plotSpatialComparison(this_comparison,
                                           type = type,
                                           map.overlay = "world",
                                           panel.bg.col = "gray")+
      scale_fill_gradient2(low = "red",
                           high = "blue",
                           mid = "white",
                           midpoint = 0,
                           na.value = "black",
                           limits = limits,
                           breaks = breaks)+
      labs(title = paste(this_comparison@name),
           subtitle = paste("Max:", max_dif,
                            "Mean:", mean_dif,
                            "Min:", min_dif,
                            "Data Mean:", mean_obs))+
      theme(plot.title = element_text(size = 30),
            plot.subtitle = element_text(size = 20),
            axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 25),
            legend.text = element_text(size = 17),
            legend.title = element_text(size = 19),
            panel.border = element_rect(color = "black",
                                        fill = NA,
                                        size = 1))
    plot(p1)
  }
    else{
      DGVMTools::plotSpatialComparison(this_comparison,
                                       type = type)
      
      #Get the Max, Min, Mean difference of Model - Observed data
      max_dif<-round(max(this_comparison@data[[5]]),2)
      mean_dif<-round(mean(this_comparison@data[[5]]),2)
      min_dif<-round(min(this_comparison@data[[5]]),2)
      
      limits = range(this_comparison@data[[5]]) 
      breaks = pretty(range(this_comparison@data[[5]]))
      if (!is.null(Benchmark@difference_limits) && length(Benchmark@difference_limits) != 0){
        limits = unlist(Benchmark@difference_limits)
      }
      if (!is.null(Benchmark@difference_breaks) && length(Benchmark@difference_breaks) != 0){
        breaks = unlist(Benchmark@difference_breaks)
      }
      # Plotting difference maps
      p1 <- DGVMTools::plotSpatialComparison(this_comparison,
                                             type = type,
                                             map.overlay = "world",
                                             panel.bg.col = "gray")+
        scale_fill_gradient2(low = "red",
                             high = "blue",
                             mid = "white",
                             midpoint = 0,
                             na.value = "black",
                             limits = limits,
                             breaks = breaks)+
        labs(title = paste(this_comparison@name),
             subtitle = paste("Max:", max_dif,
                              "Mean:", mean_dif,
                              "Min:", min_dif))+
        theme(plot.title = element_text(size = 30),
              plot.subtitle = element_text(size = 20),
              axis.title.x = element_text(size = 25),
              axis.title.y = element_text(size = 25),
              legend.text = element_text(size = 17),
              legend.title = element_text(size = 19),
              panel.border = element_rect(color = "black",
                                          fill = NA,
                                          size = 1))
      plot(p1)
    }
  }
  }

  if(length(Benchmark@guess_layers) > 1){
    p1 <- DGVMTools::plotSpatialComparison(all_comparisons[["Values"]],
                                           type = type,
                                           text.multiplier = 1.1)+ #, map.overlay = "world", panel.bg.col = "gray")+
       scale_fill_gradient2(low = "red",
                            high = "blue",
                            mid = "white",
                            midpoint = 0,
                            na.value = "black")+
      theme(plot.title = element_text(size = 30),
            plot.subtitle = element_text(size = 20),
            axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 25),
            legend.text = element_text(size = 17),
            legend.title = element_text(size = 19),
            panel.border = element_rect(colour = "black",
            linewidth = 0.5),
        strip.background = element_rect(fill = "white",
                                        color = "black",
                                        size = 0.5))

      plot(p1)
    }
  }
  if (type == "percentage.difference"){
    if (length(Benchmark@guess_layers) < 2){
      for (i in seq_along(all_comparisons[["Values"]])) {
        this_comparison <- all_comparisons[["Values"]][[i]]
        
        
        if (this_comparison@source2@id == "Obs"){
          
          reference_value <- this_comparison@data[[4]]
          raw_difference <- this_comparison@data[[3]] - this_comparison@data[[4]]
          percentage_difference <- (raw_difference / reference_value) * 100
          
          # Plotting difference maps
          limits = range(percentage_difference) 
          breaks = pretty(range(percentage_difference))
          if (!is.null(Benchmark@percentage.difference_limits[[1]]) && length(Benchmark@percentage.difference_limits[[1]]) != 0){
            limits = unlist(Benchmark@percentage.difference_limits)
          }
          if (!is.null(Benchmark@percentage.difference_breaks[[1]]) && length(Benchmark@percentage.difference_breaks[[1]]) != 0){
            breaks = unlist(Benchmark@percentage.difference_breaks)
          }
          
          DGVMTools::plotSpatialComparison(this_comparison,
                                           type = type,
                                           map.overlay = "world",
                                           panel.bg.col = "gray")
          
          this_comparison@data <- na.omit(this_comparison@data)
          
          p1 <- DGVMTools::plotSpatialComparison(this_comparison,
                                                 type = type,
                                                 map.overlay = "world",
                                                 panel.bg.col = "gray")+
            scale_fill_gradient2(low = "red",
                                 high = "blue",
                                 mid = "white",
                                 midpoint = 0,
                                 na.value = "black",
                                 limits = limits,
                                 breaks = breaks)+
            labs(title = paste(this_comparison@name),
            subtitle = paste("Max:", round(max(this_comparison@data[[5]]),2),
                             "Mean:", round(mean(this_comparison@data[[5]]),2),
                             "Min:", round(min(this_comparison@data[[5]]),2)))+
            theme(plot.title = element_text(size = 30),
                  plot.subtitle = element_text(size = 20),
                  axis.title.x = element_text(size = 25),
                  axis.title.y = element_text(size = 25),
                  legend.text = element_text(size = 17),
                  legend.title = element_text(size = 19),
                  panel.border = element_rect(color = "black",
                                              fill = NA,
                                              size = 1))
          plot(p1)
        }
        else{
          
          reference_value <- this_comparison@data[[4]]
          raw_difference <- this_comparison@data[[3]] - this_comparison@data[[4]]
          percentage_difference <- (raw_difference / reference_value) * 100
          
          # Plotting difference maps
          limits = range(percentage_difference, na.rm = T) 
          breaks = pretty(range(percentage_difference, na.rm = T))
          if (!is.null(Benchmark@percentage.difference_limits[[1]]) && length(Benchmark@percentage.difference_limits[[1]]) != 0){
            limits = unlist(Benchmark@percentage.difference_limits)
          }
          if (!is.null(Benchmark@percentage.difference_breaks[[1]]) && length(Benchmark@percentage.difference_breaks[[1]]) != 0){
            breaks = unlist(Benchmark@percentage.difference_breaks)
          }
          
          DGVMTools::plotSpatialComparison(this_comparison,
                                           type = type,
                                           map.overlay = "world",
                                           panel.bg.col = "gray")
          
         this_comparison@data <- na.omit(this_comparison@data)
          # Plotting difference maps
          p1 <- DGVMTools::plotSpatialComparison(this_comparison,
                                                 type = type,
                                                 map.overlay = "world",
                                                 panel.bg.col = "gray")+
            scale_fill_gradient2(low = "red",
                                 high = "blue",
                                 mid = "white",
                                 midpoint = 0,
                                 na.value = "black",
                                 limits = limits,
                                 breaks = breaks)+
            labs(title = paste(this_comparison@name),
             subtitle = paste("Max:", round(max(this_comparison@data[[5]]),2),
                              "Mean:", round(mean(this_comparison@data[[5]]),2),
                              "Min:", round(min(this_comparison@data[[5]]),2)))+
            theme(plot.title = element_text(size = 30),
                  plot.subtitle = element_text(size = 20),
                  axis.title.x = element_text(size = 25),
                  axis.title.y = element_text(size = 25),
                  legend.text = element_text(size = 17),
                  legend.title = element_text(size = 19),
                  panel.border = element_rect(color = "black",
                                              fill = NA,
                                              size = 1))
          plot(p1)
        }
      }
    }
    
    if(length(Benchmark@guess_layers) > 1){
      p1 <- DGVMTools::plotSpatialComparison(all_comparisons[["Values"]],
                                             type = type,
                                             text.multiplier = 1.1)+ #, map.overlay = "world", panel.bg.col = "gray")+
        scale_fill_gradient2(low = "red", 
                             high = "blue", 
                             mid = "white",
                             midpoint = 0,
                             na.value = "black")+
        theme(plot.title = element_text(size = 30),
              plot.subtitle = element_text(size = 20),
              axis.title.x = element_text(size = 25),
              axis.title.y = element_text(size = 25),
              legend.text = element_text(size = 17),
              legend.title = element_text(size = 19),
              panel.border = element_rect(colour = "black",
              linewidth = 0.5),
              strip.background = element_rect(fill = "white",
                                              color = "black",
                                              size = 0.5))
      
      
      
      
      
      plot(p1)
    }
  }
  if (type == "values"){
    if (length(Benchmark@guess_layers) < 2){
      for (i in seq_along(all_comparisons[["Values"]])) {
        this_comparison <- all_comparisons[["Values"]][[i]]
        
        
        if (this_comparison@source2@id == "Obs"){
          
          # Plotting difference maps
          combined_range <- range(this_comparison@data[[3]], this_comparison@data[[4]])  # Get the combined range of both columns
          limits <- combined_range             # Limits are simply the combined range
          
          breaks <- pretty(combined_range) 
          if (!is.null(Benchmark@values_limits[[1]]) && length(Benchmark@values_limits[[1]]) != 0){
            limits = unlist(Benchmark@values_limits)
          }
          if (!is.null(Benchmark@values_breaks[[1]]) && length(Benchmark@values_breaks[[1]]) != 0){
            breaks = unlist(Benchmark@values_breaks)
          }
          
          p1 <- DGVMTools::plotSpatialComparison(this_comparison,
                                                 type = type,
                                                 map.overlay = "world",
                                                 panel.bg.col = "gray")+
            scale_fill_gradient2(low = "red",
                                 high = "blue",
                                 mid = "white",
                                 midpoint = 0,
                                 na.value = "black",
                                 limits = limits,
                                 breaks = breaks)+
            labs(title = paste(this_comparison@name))+
            theme(plot.title = element_text(size = 30),
                  plot.subtitle = element_text(size = 20),
                  axis.title.x = element_text(size = 25),
                  axis.title.y = element_text(size = 25),
                  legend.text = element_text(size = 17),
                  legend.title = element_text(size = 19),
                  panel.border = element_rect(color = "black",
                                              fill = NA,
                                              size = 1))
          plot(p1)
        }
        else{
          
          combined_range <- range(this_comparison@data[[3]], this_comparison@data[[4]])  # Get the combined range of both columns
          limits <- combined_range             # Limits are simply the combined range
          
          breaks <- pretty(combined_range) 
          if (!is.null(Benchmark@values_limits[[1]]) && length(Benchmark@values_limits[[1]]) != 0){
            limits = unlist(Benchmark@values_limits)
          }
          if (!is.null(Benchmark@values_breaks[[1]]) && length(Benchmark@values_breaks[[1]]) != 0){
            breaks = unlist(Benchmark@values_breaks)
          }
          # Plotting difference maps
          p1 <- DGVMTools::plotSpatialComparison(this_comparison,
                                                 type = type,
                                                 map.overlay = "world",
                                                 panel.bg.col = "gray")+
            scale_fill_gradient2(low = "red",
                                 high = "blue",
                                 mid = "white",
                                 midpoint = 0,
                                 na.value = "black",
                                 limits = limits,
                                 breaks = breaks)+
            labs(title = paste(this_comparison@name))+
            theme(plot.title = element_text(size = 30),
                  plot.subtitle = element_text(size = 20),
                  axis.title.x = element_text(size = 25),
                  axis.title.y = element_text(size = 25),
                  legend.text = element_text(size = 17),
                  legend.title = element_text(size = 19),
                  panel.border = element_rect(color = "black",
                                              fill = NA,
                                              size = 1))
          plot(p1)
        }
      }
    }
    
    if(length(Benchmark@guess_layers) > 1){
      p1 <- DGVMTools::plotSpatialComparison(all_comparisons[["Values"]],
                                             type = type,
                                             text.multiplier = 1.1)+ #, map.overlay = "world", panel.bg.col = "gray")+
        scale_fill_gradient2(low = "red", 
                             high = "blue", 
                             mid = "white",
                             midpoint = 0,
                             na.value = "black")+
        theme(plot.title = element_text(size = 30),
              plot.subtitle = element_text(size = 20),
              axis.title.x = element_text(size = 25),
              axis.title.y = element_text(size = 25),
              legend.text = element_text(size = 17),
              legend.title = element_text(size = 19),
              panel.border = element_rect(colour = "black",
                                          linewidth = 0.5),
              strip.background = element_rect(fill = "white",
                                              color = "black",
                                              size = 0.5))
      
      
      
      
      
      plot(p1)
    }
  }
}
