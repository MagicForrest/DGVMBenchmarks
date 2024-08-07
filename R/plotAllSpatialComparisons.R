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

  if(length(Benchmark@guess_layers) < 2){
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
    if (!is.null(Benchmark@limits) && length(Benchmark@limits) != 0){
      limits = unlist(Benchmark@limits)
    }
    if (!is.null(Benchmark@breaks) && length(Benchmark@breaks) != 0){
      breaks = unlist(Benchmark@breaks)
    }
    
    p1 <- DGVMTools::plotSpatialComparison(this_comparison, type = type, map.overlay = "world", panel.bg.col = "gray")+
      scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                           midpoint = 0,
                           na.value = "black",
                           limits = limits,
                           breaks = breaks)+
      labs(title = paste(this_comparison@name), subtitle = paste("Max:", max_dif,
                                                                 "Mean:", mean_dif,
                                                                 "Min:", min_dif,
                                                                 "Data Mean:", mean_obs))+
      theme(plot.title = element_text(size = 30), plot.subtitle = element_text(size = 20), axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 25),legend.text = element_text(size = 17),legend.title = element_text(size = 19), panel.border = element_rect(color = "black", fill = NA, size = 1))
    plot(p1)
  }
    else{
      DGVMTools::plotSpatialComparison(this_comparison, type = type)
      
      #Get the Max, Min, Mean difference of Model - Observed data
      max_dif<-round(max(this_comparison@data[[5]]),2)
      mean_dif<-round(mean(this_comparison@data[[5]]),2)
      min_dif<-round(min(this_comparison@data[[5]]),2)
      
      limits = range(this_comparison@data[[5]]) 
      breaks = pretty(range(this_comparison@data[[5]]))
      if (!is.null(Benchmark@limits) && length(Benchmark@limits) != 0){
        limits = unlist(Benchmark@limits)
      }
      if (!is.null(Benchmark@breaks) && length(Benchmark@breaks) != 0){
        breaks = unlist(Benchmark@breaks)
      }
      # Plotting difference maps
      p1 <- DGVMTools::plotSpatialComparison(this_comparison, type = type, map.overlay = "world", panel.bg.col = "gray")+
        scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                             midpoint = 0,
                             na.value = "black",
                             limits = limits,
                             breaks = breaks)+
        labs(title = paste(this_comparison@name), subtitle = paste("Max:", max_dif,
                                                                   "Mean:", mean_dif,
                                                                   "Min:", min_dif
                                                                   ))+
        theme(plot.title = element_text(size = 30), plot.subtitle = element_text(size = 20), axis.title.x = element_text(size = 25),
              axis.title.y = element_text(size = 25),legend.text = element_text(size = 17),legend.title = element_text(size = 19), panel.border = element_rect(color = "black", fill = NA, size = 1))
      plot(p1)
    }
  }
  }

  if(length(Benchmark@guess_layers) > 1){
    p1 <- DGVMTools::plotSpatialComparison(all_comparisons[["Values"]], type = type,text.multiplier = 1.1)+ #, map.overlay = "world", panel.bg.col = "gray")+
       scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                            midpoint = 0,
                            na.value = "black",
                           )+
      theme(plot.title = element_text(size = 30), plot.subtitle = element_text(size = 20), axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 25),legend.text = element_text(size = 17),legend.title = element_text(size = 19),panel.border = element_rect(colour = "black", linewidth = 0.5),
        strip.background = element_rect(fill = "white", color = "black", size = 0.5)
        )




      
    plot(p1)
  }
}
