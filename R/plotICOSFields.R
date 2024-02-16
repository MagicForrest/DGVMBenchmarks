
#' Plot ICOS dataset and runs either per run or by comparison Model/Data.
#'
#' @param Benchmark Object holding current benchmark information
#' @param all_Fields_list The list holding all exclusive fields 
#' @param all_comparisons The list holding comparison objects for joined plots
#' @param plot.option Plotting options. Either "per_source" to make a plot for each source exclusively, or "joined" to make plots of data together with simulations.
#'
#' @return plots of ICOS time.series
#' @export
#'
#' @examples plotICOSFields(Benchmark = this_benchmark, all_comparisons = all_comparisons, plot.option = "joined") plotICOSFields(Benchmark = this_benchmark, all_Fields_list = all_Fields_list, plot.option = "per_source")
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
plotICOSFields <- function(Benchmark = this_benchmark, all_Fields_list = all_Fields_list, all_comparisons = all_comparisons, plot.option){
 
if (!missing(all_Fields_list) && plot.option == "per_source"){
  for (i in seq_along(all_Fields_list)) {
  this_Field <- all_Fields_list[[i]]
  stations <- read.table(file.path(system.file("extdata/ICOS/ICOS_stations_info.txt", package = "DGVMBenchmarks")), header = T,sep = "\t")
  stations$Lon <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 1))
  stations$Lat <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 2))
  
  # Remove Position column
  stations <- stations[, -which(names(stations) == "Position")]
  # Merge based on matching latitude and longitude coordinates
  merged_data <- merge(this_Field@data, stations[, c("Lon", "Lat", "Site.type", "Name")], by = c("Lon", "Lat"), all.x = TRUE)
  
  # Add the Site Names column to this_comparison@data
  this_Field@data$Name <- merged_data$Name
  this_Field@data$Date <- as.Date(paste0(this_Field@data$Year, "-", this_Field@data$Day), format = "%Y-%j")

# Group data by Lon, Lat, and Year
grouped_data <- this_Field@data %>%
  group_by(Lon, Lat, Year)

densplot <- ggplot(grouped_data, aes(x = Date)) +
  geom_line(aes(y = this_Field@data[[Benchmark@guess_layers]],color = Benchmark@guess_layers), color = "#f4a261") +
  #geom_smooth(method = "loess", aes(y = this_Field@data[[Benchmark@guess_layers]]))+
  facet_wrap(~Name) +  # Facet by Lon and Lat
  labs(title = paste0("Total"," ", Benchmark@guess_layers," ", this_Field@source@name),y = paste0(toupper(Benchmark@id), " (", Benchmark@unit, ")"),
       color = "") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "bottom",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 18),
        legend.box = "horizontal",
  ) +
  guides(color = guide_legend(ncol = 1))
plot(densplot)
}}

if (!missing(all_comparisons) && plot.option == "joined"){
for (i in seq_along(all_comparisons)) {
  this_comparison <- all_comparisons[[1]][[i]]
  stations <- read.table(file.path(system.file("extdata/ICOS/ICOS_stations_info.txt", package = "DGVMBenchmarks")), header = T,sep = "\t")
  stations$Lon <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 1))
  stations$Lat <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 2))
  
  # Remove Position column
  stations <- stations[, -which(names(stations) == "Position")]
  # Merge based on matching latitude and longitude coordinates
  merged_data <- merge(this_comparison@data, stations[, c("Lon", "Lat", "Site.type", "Name")], by = c("Lon", "Lat"), all.x = TRUE)
  
  # Add the Site.type column to this_comparison@data
  this_comparison@data$Site.type <- merged_data$Site.type
  this_comparison@data$Name <- merged_data$Name

  this_comparison@data$Date <- as.Date(paste0(this_comparison@data$Year, "-", this_comparison@data$Day), format = "%Y-%j")
  
  # Group data by Lon, Lat, and Year
  grouped_data <- this_comparison@data %>%
    group_by(Lon, Lat, Year)
  
# Plotting
densplot <- ggplot(this_comparison@data, aes(x = Date)) +
  geom_line(aes(y = this_comparison@data[[5]], color = this_comparison@source1@name)) +
  geom_line(aes(y = this_comparison@data[[6]], color = this_comparison@source2@name), alpha = 0.8)+
  facet_wrap(~Name) +  # Facet by Lon and Lat
  labs(title = paste0(this_comparison@name) ,y = paste0(toupper(Benchmark@id), " (", Benchmark@unit, ")"),
       color = "") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "bottom",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 18),
        legend.box = "horizontal",
  ) +
  guides(color = guide_legend(ncol = 1))

# Print the plot
print(densplot)
}
  }
}