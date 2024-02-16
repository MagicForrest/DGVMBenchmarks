#' Make plots from temporal comparison objects from the difference between simulation and data.
#'
#' @param Benchmark Holding the current benchmark information.
#' @param all_comparisons List holding all comparison objects.
#'
#' @return Temporal difference plots
#' @export
#'
#' @examples plotAllTemporalComparisons(Benchmark = this_benchmark, all_comparisons = all_comparisons)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
plotAllTemporalComparisons <- function(Benchmark = this_benchmark, all_comparisons = all_comparisons){
 
  if (Benchmark@datasets[[1]]@source@format@id == "ICOS"){
  grid.names <- unique(Benchmark@datasets[[1]]@spatial.extent$Name)
  names(grid.names) <- paste0("(", unique(Benchmark@datasets[[1]]@spatial.extent$Lon), ",", unique(Benchmark@datasets[[1]]@spatial.extent$Lat), ")")
  
  
if(length(Benchmark@guess_layers) < 2){
for (layer in all_comparisons[[1]]){
  plot1 <- DGVMTools::plotTemporalComparison(layer, type = c("difference"), labeller= as_labeller(grid.names))+
    geom_line( size = 0.6, linetype = 1 )+
    geom_point( size = 0.4, colour = "black", alpha = 0.4)+
    scale_color_manual(values = color_palette)+
    #coord_fixed(ratio = 550)+
    labs(title = paste0("Difference"," ", layer@name),
      subtitle = paste0("Total"," ", "Daily", " ", Benchmark@guess_layers),
      x = "Year",
      y.label = paste0("Difference", toupper(Benchmark@id),"(", Benchmark@unit,")"))+
    theme(axis.title = element_text(size = 18),  # Adjust axis title text size
          axis.text = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1),# Adjust axis text size
          legend.text = element_text(size = 18),
          plot.title = element_text(size = 20))
  print(plot1)
}
}

if(length(Benchmark@guess_layers) > 1){
  p1 <- DGVMTools::plotTemporalComparison(all_comparisons[["Values"]], type = "difference",text.multiplier = 1.1, labeller= as_labeller(grid.names))+ #, map.overlay = "world", panel.bg.col = "gray")+
    geom_line( size = 0.6, linetype = 1 )+
    geom_point( size = 0.4, colour = "black", alpha = 0.4)+
    scale_color_manual(values = color_palette)+
    #coord_fixed(ratio = 550)+
    labs(#title = NULL,
      #subtitle = NULL,
      x = "Year",
      y = paste("Difference", toupper(Benchmark@id),"(", Benchmark@unit,")"))+
    theme(axis.title = element_text(size = 18),  # Adjust axis title text size
          axis.text = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1),# Adjust axis text size
          legend.text = element_text(size = 18),
          plot.title = element_text(size = 20))
  print(p1)
}
  }
  else {
    if(length(Benchmark@guess_layers) < 2){
    for (layer in all_comparisons[[1]]){
      plot1 <- DGVMTools::plotTemporalComparison(layer, type = c("difference"))+
        geom_line( size = 0.6, linetype = 1 )+
        geom_point( size = 0.4, colour = "black", alpha = 0.4)+
        scale_color_manual(values = color_palette)+
        #coord_fixed(ratio = 550)+
        labs(title = paste0("Difference"," ", layer@name),
             subtitle = paste0("Total"," ", "Daily", " ", Benchmark@guess_layers),
             x = "Year",
             y.label = paste0("Difference", toupper(Benchmark@id),"(", Benchmark@unit,")"))+
        theme(axis.title = element_text(size = 18),  # Adjust axis title text size
              axis.text = element_text(size = 18),
              axis.text.x = element_text(angle = 45, hjust = 1),# Adjust axis text size
              legend.text = element_text(size = 18),
              plot.title = element_text(size = 20))
      print(plot1)
    }
  }
    
    if(length(Benchmark@guess_layers) > 1){
      p1 <- DGVMTools::plotTemporalComparison(all_comparisons[["Values"]], type = "difference",text.multiplier = 1.1)+ #, map.overlay = "world", panel.bg.col = "gray")+
        geom_line( size = 0.6, linetype = 1 )+
        geom_point( size = 0.4, colour = "black", alpha = 0.4)+
        scale_color_manual(values = color_palette)+
        #coord_fixed(ratio = 550)+
        labs(#title = NULL,
          #subtitle = NULL,
          x = "Year",
          y = paste("Difference", toupper(Benchmark@id),"(", Benchmark@unit,")"))+
        theme(axis.title = element_text(size = 18),  # Adjust axis title text size
              axis.text = element_text(size = 18),
              axis.text.x = element_text(angle = 45, hjust = 1),# Adjust axis text size
              legend.text = element_text(size = 18),
              plot.title = element_text(size = 20))
      print(p1)}
  }
}