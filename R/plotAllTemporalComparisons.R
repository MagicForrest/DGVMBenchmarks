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
 
  comparisons = all_comparisons[[1]]
  
  color_palette <- c("#457b9d", "#a8dadc", "#f4a261", "#2a9d8f", "#e76f51")
  
  grid.names <- NULL
  if (Benchmark@datasets[[1]]@source@format@id == "ICOS"){
  grid.names <- unique(Benchmark@datasets[[1]]@spatial.extent$Name)
  names(grid.names) <- paste0("(", unique(Benchmark@datasets[[1]]@spatial.extent$Lon), ",", unique(Benchmark@datasets[[1]]@spatial.extent$Lat), ")")}
  
  if (Benchmark@datasets[[1]]@source@format@id == "FLUXNET"){
    grid.names <- unique(Benchmark@datasets[[1]]@spatial.extent$Name)
    names(grid.names) <- paste0("(", unique(Benchmark@datasets[[1]]@spatial.extent$Lon), ",", unique(Benchmark@datasets[[1]]@spatial.extent$Lat), ")")}
  
  
  if (all_comparisons[["Values"]][[1]]@source2@format@id == "SITE"){
    PROFOUND <- read.csv(file.path(system.file("extdata/PROFOUND/PROFOUND_Grid_List.csv", package = "DGVMBenchmarks")), header = T,sep = ",")
    setDT(PROFOUND)
    comparisons <- list()
    for (i in seq_along(all_comparisons)){
      this_comparison <- copy(all_comparisons[[1]][[i]])
      this_comparison@data[, Site := character()]
      
      # Add the "Site" column to this_comparison@data
this_comparison@data[
        PROFOUND,
        Site := i.Site,
        on = c("Lat", "Lon")
      ]      
    
    comparisons[[this_comparison@name]] <- this_comparison
      grid.names <- unique(PROFOUND$Site)
      names(grid.names) <- paste0("(", unique(PROFOUND$Lon), ",", unique(PROFOUND$Lat), ")")
      }
    
    
    }
  
if(length(Benchmark@guess_layers) < 2 && !is.null(grid.names) ){
for (layer in comparisons){
  plot1 <- DGVMTools::plotTemporalComparison(layer, type = c("difference"), labeller= as_labeller(grid.names))+
    geom_line( size = 0.6, linetype = 1 )+
    geom_point( size = 0.4, colour = "black", alpha = 0.4)+
    scale_color_manual(values = color_palette)+
    
    #coord_fixed(ratio = 550)+
    labs(title = paste0("Difference"," ", layer@name),
      subtitle = paste0("Total"," ", layer@sta.info1@subannual.resolution, " ", Benchmark@guess_layers),
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

if(length(Benchmark@guess_layers) > 1 && !is.null(grid.names)){
  for (layer in comparisons){
  p1 <- DGVMTools::plotTemporalComparison(layer, type = "difference",text.multiplier = 1.1, labeller= as_labeller(grid.names))+ #, map.overlay = "world", panel.bg.col = "gray")+
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
    if(length(Benchmark@guess_layers) < 2 && is.null(grid.names)){
    for (layer in all_comparisons[[1]]){
      plot1 <- DGVMTools::plotTemporalComparison(layer, type = c("difference"))+
        geom_line( size = 0.9, linetype = 1 )+
        geom_point( size = 0.8, colour = "black", alpha = 0.4)+
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
    
    if(length(Benchmark@guess_layers) > 1 && is.null(grid.names)){
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