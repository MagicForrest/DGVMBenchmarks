#' Plot FLUXNET dataset and runs either per run or by comparison Model/Data.
#'
#' @param Benchmark Object holding current benchmark information
#' @param all_Fields_list The list holding all exclusive fields 
#' @param all_comparisons The list holding comparison objects for joined plots
#' @param plot.option Plotting options. Either "per_source" to make a plot for each source exclusively, or "joined" to make plots of data together with simulations.
#'
#' @return plots of FLUXNET time.series
#' @export
#'
#' @examples plotFLUXNETFields(Benchmark = this_benchmark, all_comparisons = all_comparisons, plot.option = "joined") plotFLUXNETFields(Benchmark = this_benchmark, all_Fields_list = all_Fields_list, plot.option = "per_source")
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
plotFLUXNETFields <- function(Benchmark = this_benchmark, all_Fields_list = all_Fields_list, all_comparisons = all_comparisons, plot.option) {
  if (plot.option == "per_source") {
    for (i in seq_along(all_Fields_list)) {
      this_Field <- all_Fields_list[[i]]
      FLUXNET_SITES <- read.csv(file.path(system.file("extdata/FLUXNET/FLUXNET_SITES.csv", package = "DGVMBenchmarks")), header = T,sep = ";")
      setDT(FLUXNET_SITES)
      
     
      # Merge based on matching latitude and longitude coordinates
      merged_data <- merge(this_Field@data, FLUXNET_SITES[, c("Lon", "Lat", "Name")], by = c("Lon", "Lat"), all.x = TRUE)
      
      # Rename "Name.y" to "Name" if it exists in merged_data
      if ("Name.y" %in% names(merged_data)) {
        names(merged_data)[names(merged_data) == "Name.y"] <- "Name"
      }
      # Add the Site Names column to this_comparison@data
      this_Field@data$Name <- merged_data$Name
      this_Field@data$Date <- as.Date(paste0(this_Field@data$Year, "-", this_Field@data$Day), format = "%Y-%j")
      
      # Group data by Lon, Lat, and Year
      grouped_data <- this_Field@data %>%
        group_by(Lon, Lat, Year)
      
      densplot <- ggplot(grouped_data, aes(x = Date)) +
        geom_line(aes(y = this_Field@data[[Benchmark@guess_layers]], color = Benchmark@guess_layers), color = "#f4a261") +
        facet_wrap(~Name) +  # Facet by Lon and Lat
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
  } else if (plot.option == "joined") {
    for (i in seq_along(all_comparisons)) {
      this_comparison <- all_comparisons[[1]][[i]]
      
      FLUXNET_SITES <- read.csv(file.path(system.file("extdata/FLUXNET/FLUXNET_SITES.csv", package = "DGVMBenchmarks")), header = T,sep = ";")
      setDT(FLUXNET_SITES)
      
      
      # Merge based on matching latitude and longitude coordinates
      merged_data <- merge(this_comparison@data, FLUXNET_SITES[, c("Lon", "Lat", "Name")], by = c("Lon", "Lat"), all.x = TRUE)
      
      # Add the Site.type column to this_comparison@data
      
      this_comparison@data$Name <- merged_data$Name
      this_comparison@data$Date <- as.Date(paste0(this_comparison@data$Year, "-", this_comparison@data$Day), format = "%Y-%j")
      
      # Group data by Lon, Lat, and Year
      grouped_data <- this_comparison@data %>%
        group_by(Lon, Lat, Year)
      
      densplot <- ggplot(this_comparison@data, aes(x = Date)) +
        geom_line(aes(y = this_comparison@data[[5]], color = this_comparison@source1@name)) +
        geom_line(aes(y = this_comparison@data[[6]], color = this_comparison@source2@name), alpha = 0.8) +
        facet_wrap(~Name) +  # Facet by Lon and Lat
        labs(title = paste0(this_comparison@name), y = paste0(toupper(Benchmark@id), " (", Benchmark@unit, ")"),
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
}
