#' Make ICOS scatter plots. Creates scatter plots and use fill to display per Land use or Climate etc.
#'
#' @param Benchmark Holds current becnhmark information.
#' @param all_comparisons List holding the comparison objects
#' @param fill fill is used to classify points by different characteristics. Use "climate" for koppen climate zones and "landuse" for land cover information.
#'
#' @return Scatter plots of ICOS comparisons.
#' @export
#'
#' @examples plotICOSScatter(Benchmark = this_benchmark, all_comparisons = all_comparisons, fill = "climate")
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
plotICOSScatter <- function(Benchmark = this_benchmark, all_comparisons = all_comparisons, fill = NULL){


if (fill == "climate"){
for (i in seq_along(all_comparisons)) {
  this_comparison <- all_comparisons[[1]][[i]]
  stations <- read.table(file.path(system.file("extdata/ICOS/ICOS_stations_info.txt", package = "DGVMBenchmarks")), header = T,sep = "\t")
  stations$Lon <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 1))
  stations$Lat <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 2))
  
  # Remove Position column
  stations <- stations[, -which(names(stations) == "Position")]
  # Merge based on matching latitude and longitude coordinates
  merged_data <- merge(this_comparison@data, stations[, c("Lon", "Lat", "Site.type", "Name", "Climate.zone")], by = c("Lon", "Lat"), all.x = TRUE)
  
  # Add the Site.type column to this_comparison@data
  this_comparison@data$Climate.zone <- merged_data$Climate.zone
  this_comparison@data$Name <- merged_data$Name
  
  
  densplot1<-ggplot(data = this_comparison@data, aes(x  = this_comparison@data[[6]], y = this_comparison@data[[5]],color=Climate.zone)) +
    #geom_bin2d(bins = 30) +
    scale_fill_continuous(type = "viridis") +
    theme_bw()+
    geom_point(size = 2, alpha = 0.6)+
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size=1)+
    
    labs(title = paste(this_comparison@source1@name, "vs", Benchmark@datasets[[1]]@source@name),
         x = paste(Benchmark@datasets[[1]]@source@name, Benchmark@id, Benchmark@unit),
         y = paste("Model", Benchmark@id, Benchmark@unit),
         color = "Climate Zone") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          axis.text = element_text(size = 15))+
    coord_fixed(ratio = 1)+
    guides(color = guide_legend(override.aes = list(size = 5)))
    plot(densplot1)
}
}else if (fill == "landuse"){
  for (i in seq_along(all_comparisons)) {
    this_comparison <- all_comparisons[[1]][[i]]
    stations <- read.table(file.path(system.file("extdata/ICOS/ICOS_stations_info.txt", package = "DGVMBenchmarks")), header = T,sep = "\t")
    stations$Lon <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 1))
    stations$Lat <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 2))
    
    # Remove Position column
    stations <- stations[, -which(names(stations) == "Position")]
    # Merge based on matching latitude and longitude coordinates
    merged_data <- merge(this_comparison@data, stations[, c("Lon", "Lat", "Site.type", "Name", "Climate.zone")], by = c("Lon", "Lat"), all.x = TRUE)
    
    # Add the Site.type column to this_comparison@data
    this_comparison@data$Site.type <- merged_data$Site.type
    this_comparison@data$Name <- merged_data$Name
    
    
    
    densplot1<-ggplot(data = this_comparison@data, aes(x  = this_comparison@data[[6]], y = this_comparison@data[[5]],color=Site.type)) +
      #geom_bin2d(bins = 30) +
      scale_fill_continuous(type = "viridis") +
      theme_bw()+
      geom_point(size = 2, alpha = 0.6)+
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size=1)+
      
      labs(title = paste(this_comparison@source1@name, "vs", Benchmark@datasets[[1]]@source@name),
           x = paste(Benchmark@datasets[[1]]@source@name, Benchmark@id, Benchmark@unit),
           y = paste("Model", Benchmark@id, Benchmark@unit),
           color = "Land Use") +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
            axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 25),
            axis.text = element_text(size = 15))+
      coord_fixed(ratio = 1)+
      guides(color = guide_legend(override.aes = list(size = 5)))
    plot(densplot1)
  }
} else if (fill == "station"){
  for (i in seq_along(all_comparisons)) {
    this_comparison <- all_comparisons[[1]][[i]]
    stations <- read.table(file.path(system.file("extdata/ICOS/ICOS_stations_info.txt", package = "DGVMBenchmarks")), header = T,sep = "\t")
    stations$Lon <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 1))
    stations$Lat <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 2))
    
    # Remove Position column
    stations <- stations[, -which(names(stations) == "Position")]
    # Merge based on matching latitude and longitude coordinates
    merged_data <- merge(this_comparison@data, stations[, c("Lon", "Lat", "Site.type", "Name", "Climate.zone", "Country")], by = c("Lon", "Lat"), all.x = TRUE)
    
    # Add the Site.type column to this_comparison@data
    this_comparison@data$Name <- merged_data$Name
    this_comparison@data$Country <- merged_data$Country
    
    
    
    densplot1<-ggplot(data = this_comparison@data, aes(x  = this_comparison@data[[6]], y = this_comparison@data[[5]],color=Country, shape = Name)) +
      #geom_bin2d(bins = 30) +
      scale_fill_continuous(type = "viridis") +
      theme_bw()+
      geom_point(size = 2, alpha = 0.6)+
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size=1)+
      
      labs(title = paste(this_comparison@source1@name, "vs", Benchmark@datasets[[1]]@source@name),
           x = paste(Benchmark@datasets[[1]]@source@name, Benchmark@id, Benchmark@unit),
           y = paste("Model", Benchmark@id, Benchmark@unit),
           color = "Country") +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
            axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 25),
            axis.text = element_text(size = 15))+
      coord_fixed(ratio = 1)+
      guides(color = guide_legend(override.aes = list(size = 5)))
    plot(densplot1)
  }
}else {
  for (i in seq_along(all_comparisons)) {
  this_comparison <- all_comparisons[[1]][[i]]
  
  densplot1<-ggplot(data = this_comparison@data, aes(x  = this_comparison@data[[6]], y = this_comparison@data[[5]])) +
    #geom_bin2d(bins = 30) +
    scale_fill_continuous(type = "viridis") +
    theme_bw()+
    geom_point(size = 2,color = "purple", alpha = 0.6)+
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size=1)+
    
    labs(title = paste(this_comparison@source1@name, "vs", Benchmark@datasets[[1]]@source@name),
         x = paste(Benchmark@datasets[[1]]@source@name, Benchmark@id, Benchmark@unit),
         y = paste("Model", Benchmark@id, Benchmark@unit)) +
    theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          axis.text = element_text(size = 15))+
    coord_fixed(ratio = 1)
    
  plot(densplot1)
  
  }
}
}