
#' Plot Field or list of fields
#'
#' @param Benchmark Holds the current benchmark settings
#' @param all_Fields_list list containing all your fields both data and simulations
#'
#' @return
#' @export
#'
#' @examples
plotSpatialField <- function(Benchmark, all_Fields_list = all_Fields_list) {
  
    for (i in seq_along(all_Fields_list)) {
      this_field <- all_Fields_list[[i]]
      
      # Plotting difference maps
      p1 <- DGVMTools::plotSpatial(this_field, layer = Benchmark@guess_layers, map.overlay = "world", panel.bg.col = "gray")+
        scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                             midpoint = 0,
                             na.value = "black")+
                             #limits = unlist(Benchmark@limits),
                             #breaks = unlist(Benchmark@breaks))+
        labs(title = paste(this_field@source@name))+
        theme(plot.title = element_text(size = 30), plot.subtitle = element_text(size = 20), axis.title.x = element_text(size = 25),
              axis.title.y = element_text(size = 25),legend.text = element_text(size = 17),legend.title = element_text(size = 19), panel.border = element_rect(color = "black", fill = NA, size = 1))
        
      plot(p1)
    }
}