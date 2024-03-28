#' Plotting timeseries with uncertainty metrics.
#'
#' @param Benchmark The current benchmark information
#' @param all_Fields_list Used to plot per source
#' @param all_comparisons Used to plut combined timeseries
#' @param plot.option Option for pre layer or combined plots use "per_source" or "joined". 
#' @param uncertainty Default = "none" . Set "SD" to use standard deviation, Looks for ancilliary files.
#'
#' @return plots
#' @export
#'
#' @examples plotTemporalFields(Benchmark = this_benchmark, all_Fields_list = all_Fields_list, all_comparisons = all_comparisons, plot.option , uncertainty = "SD")
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
plotTemporalFields <- function(Benchmark = this_benchmark, all_Fields_list = all_Fields_list, all_comparisons = all_comparisons, plot.option , uncertainty = "none") {
  source1 <- list()
  source2 <- list()
  field_STD <- list()
  if (uncertainty == "SD"){
    for (i in seq_along(all_comparisons[[1]])){
    
      source1[[all_comparisons[[1]][[i]]@source1@name]] <- list.files(path = paste(all_comparisons[["Values"]][[i]]@source1@dir), pattern = paste0(Benchmark@id, "_","std"),full.names = TRUE, recursive = TRUE)
      source2[[all_comparisons[[1]][[i]]@source2@name]] <- list.files(path = paste(all_comparisons[["Values"]][[i]]@source2@dir), pattern = paste0(Benchmark@id, "_","std"),full.names = TRUE, recursive = TRUE)
      
      if (length(source1[[1]]) > 0){
        # Read the standard deviation files as data.tables
        source1_std <- lapply(source1, fread)
        
        # Merge with the main data table by 'Lat', 'Lon', and 'Year'
        all_comparisons[[1]][[i]]@data <- merge(all_comparisons[[1]][[i]]@data, source1_std[[1]], by = c("Lat", "Lon", "Year"), all.x = TRUE, suffixes = c("", ".1"))
        setnames(all_comparisons[[1]][[i]]@data, "STD", "STD1")
      }
      if (length(source2[[1]]) > 0){
        source2_std <- lapply(source2, fread)
      all_comparisons[[1]][[i]]@data <- merge(all_comparisons[[1]][[i]]@data, source2_std[[1]], by = c("Lat", "Lon", "Year"), all.x = TRUE, suffixes = c("", ".2"))
      setnames(all_comparisons[[1]][[i]]@data, "STD", "STD2")
      
    }
    source1 <- list()
    source2 <- list()
    source1_std <- list()
    source2_std <- list()
    }
    for (i in seq_along(all_Fields_list)){
      field_STD[[all_Fields_list[[i]]@source@name]] <- list.files(path = paste(all_Fields_list[[i]]@source@dir), pattern = paste0(Benchmark@id, "_","std"),full.names = TRUE, recursive = TRUE)
    if (length(field_STD[[1]]) > 0){
      
      source_std <- lapply(field_STD, fread)
      
      # Merge with the main data table by 'Lat', 'Lon', and 'Year'
      all_Fields_list[[i]]@data <- merge(all_Fields_list[[i]]@data, source_std[[1]], by = c("Lat", "Lon", "Year"), all.x = TRUE, suffixes = c("", ".1"))
    }
      field_STD <- list()
  }
}
  
  if (plot.option == "per_source") {
    for (i in seq_along(all_Fields_list)) {
      this_Field <- all_Fields_list[[i]]
      
      
      # Add the Site Names column to this_comparison@data
      if ("Year" %in% names(this_Field@data) && "Day" %in% (this_Field@data)){
      this_Field@data$Date <- as.Date(paste0(this_Field@data$Year, "-", this_Field@data$Day), format = "%Y-%j")
      
      # Group data by Lon, Lat, and Year
      grouped_data <- this_Field@data %>%
        group_by(Lon, Lat, Year)
      
      densplot <- ggplot(grouped_data, aes(x = Date))
      }
    if ("Year" %in% names(this_Field@data) && !"Day" %in% names(this_Field@data)){
      densplot <- ggplot(this_Field@data, aes(x = Year))
    }
      if (uncertainty == "SD" && "STD" %in% names(this_Field@data)){
        
        densplot <- densplot +
          geom_ribbon(                     # Shaded area for uncertainty
            aes(ymin =this_Field@data[[Benchmark@guess_layers]] - (this_Field@data[["STD"]] * 1.96), ymax = this_Field@data[[Benchmark@guess_layers]] + (this_Field@data[["STD"]] * 1.96), fill = Site), 
            alpha = 0.2                    # Set transparency of the shaded area
          )
      }
    
     densplot <- densplot +
        geom_line(aes(y = this_Field@data[[Benchmark@guess_layers]], color = Benchmark@guess_layers), color = "#f4a261") +
      facet_wrap(~Site) +  # Facet by Lon and Lat
        labs(title = paste0("Total", " ", Benchmark@guess_layers, " ", this_Field@source@name), y = paste0(toupper(Benchmark@id), " (", Benchmark@unit, ")"),
             color = "") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 18),
              legend.position = "bottom",
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 18),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.text = element_text(size = 18),
              legend.box = "horizontal") +
        guides(color = guide_legend(ncol = 1))
      
      plot(densplot)
    }
  }
   if (plot.option == "joined") {
    
    collection <- list()
      if (all_comparisons[["Values"]][[1]]@source2@format@id == "SITE"){
        PROFOUND <- read.csv(file.path(system.file("extdata/PROFOUND/PROFOUND_Grid_List.csv", package = "DGVMBenchmarks")), header = T,sep = ",")
        setDT(PROFOUND)
        for (i in seq_along(all_comparisons[[1]])) {
          this_comparison <- copy(all_comparisons[[1]][[i]])
          # Merge and keep only the 'Site' column from the PROFOUND dataset
          this_comparison@data <- merge(
            x = this_comparison@data,
            y = PROFOUND[, .(Lat, Lon, Site)],
            by = c("Lon", "Lat"),
            all.x = TRUE
          )
          collection[[this_comparison@name]] <- this_comparison
        
      }
      
        
      for (layer in collection){
        if ("Year" %in% names(layer@data) && "Day" %in% names(layer@data)){
          layer@data$Date <- as.Date(paste0(layer@data$Year, "-", layer@data$Day), format = "%Y-%j")
          
          # Group data by Lon, Lat, and Year
          grouped_data <- this_Field@data %>%
            group_by(Lon, Lat, Year)
          
          densplot <- ggplot(grouped_data, aes(x = Date))
        }
        if ("Year" %in% names(layer@data) && !"Day" %in% names(layer@data)){
          densplot <- ggplot(layer@data, aes(x = Year))}
        if ("STD1" %in% names(layer@data)){
      densplot <- densplot +
        geom_ribbon(                     # Shaded area for uncertainty
          aes(ymin =layer@data[[4]] - (layer@data[["STD1"]] * 1.96), ymax = layer@data[[4]] + (layer@data[["STD1"]] * 1.96), fill = "#a8dadc"), 
          alpha = 0.2                    # Set transparency of the shaded area
        )}
        
        if ("STD2" %in% names(layer@data)){
        densplot <- densplot +
        geom_ribbon(                     # Shaded area for uncertainty
          aes(ymin =layer@data[[5]] - (layer@data[["STD2"]] * 1.96), ymax = layer@data[[5]] + (layer@data[["STD2"]] * 1.96), fill = "#f4a261"), 
          alpha = 0.2                    # Set transparency of the shaded area
        )} 
        
        densplot <- densplot +
        geom_line(aes(y = layer@data[[4]], color = layer@source1@name)) +
        geom_line(aes(y = layer@data[[5]], color = layer@source2@name), alpha = 0.8) +
        facet_wrap(~toupper(Site)) +  # Facet by Lon and Lat
        labs(title = paste0(layer@name), y = paste0(toupper(Benchmark@id), " (", Benchmark@unit, ")"),
             color = "") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 18),
              legend.position = "bottom",
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 18),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.text = element_text(size = 18),
              legend.box = "horizontal") +
        guides(color = guide_legend(ncol = 1), fill = FALSE) 
      
      plot(densplot)
      }
    }
  }
}
