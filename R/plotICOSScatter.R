#' Make ICOS scatter plots. Creates scatter plots and use fill to display per Land use or Climate etc.
#'
#' @param Benchmark Holds current becnhmark information.
#' @param all_comparisons List holding the comparison objects
#' @param fill fill is used to classify points by different characteristics. Use "climate" for koppen climate zones and "landuse" for land cover information, "country" or "station".
#'
#' @return Scatter plots of ICOS comparisons.
#' @export
#'
#' @examples plotICOSScatter(Benchmark = this_benchmark, all_comparisons = all_comparisons, fill = "climate")
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
plotICOSScatter <- function(Benchmark = this_benchmark, all_comparisons = all_comparisons, fill = NULL){
  
  
if (fill == "climate"){
for (i in seq_along(all_comparisons[[1]])) {
  this_comparison <- all_comparisons[[1]][[i]]
  # this_comparison@data$Code <- sapply(this_comparison@data$Lon, function(lon) {
  #   matching_row <- spatial_extent[spatial_extent$Lon %in% lon & spatial_extent$Lat %in% this_comparison@data$Lat]
  #   if (nrow(matching_row) > 0) {
  #     return(matching_row$Code)
  #   } else {
  #     return(NA)
  #   }
  # })
  stations <- read.csv(file.path(system.file("extdata/ICOS/ICOS_stations_info.csv", package = "DGVMBenchmarks")), header = T,sep = ";")
  stations$Lon <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 1))
  stations$Lat <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 2))
  decimals <- 4  # Specify the number of decimal places you want to round to
  
  # Round Lon and Lat columns in stations
  stations$Lon <- round(as.numeric(stations$Lon), decimals)
  stations$Lat <- round(as.numeric(stations$Lat), decimals)
  
  # Round Lon and Lat columns in this_Field@data
  this_comparison@data$Lon <- round(this_comparison@data$Lon, decimals)
  this_comparison@data$Lat <- round(this_comparison@data$Lat, decimals)
  # Remove Position column
  stations <- stations[, -which(names(stations) == "Position")]
  #stations$Station_ID <- sapply(strsplit(stations$Id, "/"), function(x) tail(x, 1))
  # Merge based on matching latitude and longitude coordinates
  merged_data <- merge(this_comparison@data, stations[, c("Lon", "Lat", "Site.type", "Name", "Climate.zone")], by = c("Lon", "Lat"), all.x = TRUE)
  
  # Add the Site.type column to this_comparison@data
  this_comparison@data$Climate.zone <- merged_data$Climate.zone
  this_comparison@data$Name <- merged_data$Name
  field_data <- this_comparison@data
  
  # Add the Season column using case_when
  field_data <- field_data %>%
    mutate(Season = case_when(
      between(Day, 80, 171) ~ "Spring",
      between(Day, 172, 263) ~ "Summer",
      between(Day, 264, 355) ~ "Autumn",
      TRUE ~ "Winter"
    ))
  
  # Assign the modified data back to the list
  this_comparison@data <- field_data

  unique_seasons <- unique(this_comparison@data$Season)
  unique_sites <- unique(this_comparison@data$Name)
  
  meanSeasonal <- data.frame(Name = character(), 
                        Season = character(), 
                        Mean1 = numeric(),
                        Mean2 = numeric(),
                        SD1 = numeric(),
                        SD2 = numeric(),
                        Min1 = numeric(),
                        Min2 = numeric(),
                        Max1 = numeric(),
                        Max2 = numeric(),
                        Climate.zone = character())
  
  for (season in unique_seasons) {
    for (site in unique_sites) {
      # Filter data for specific site and season
        temp_data <- filter(this_comparison@data, Season == season, Name == site)
        
        climate_zone <- unique(temp_data$Climate.zone)
        # Calculate mean 
        mean_value1 <- mean(temp_data[[5]], na.rm = TRUE)
        mean_value2 <- mean(temp_data[[6]], na.rm = TRUE)
        std_val1 <- sd(temp_data[[5]], na.rm = TRUE)
        std_val2 <-  sd(temp_data[[6]], na.rm = TRUE)
        Min_val1 <- min(temp_data[[5]], na.rm = TRUE)
        Min_val2 <-  min(temp_data[[6]], na.rm = TRUE)
        Max_val1 <- max(temp_data[[5]], na.rm = TRUE)
        Max_val2 <-  max(temp_data[[6]], na.rm = TRUE)
        # Append results
        meanSeasonal <- rbind(meanSeasonal, data.frame(Name = site, Season = season, Climate.zone = climate_zone, Mean1 = mean_value1, Mean2 = mean_value2, SD1 = std_val1, SD2 = std_val2, Min1 = Min_val1, Min2 = Min_val2, Max1 = Max_val1, Max2 = Max_val2))
    }
  }
  
  
  shape_mapping <- c("Winter" = 21, "Spring" = 22, "Summer" = 23, "Autumn" = 24)
  
  plot1 <- ggplot(data = meanSeasonal, aes(x = Mean2, y = Mean1, color = Climate.zone, shape = Season)) +
    geom_point(aes(fill = Climate.zone), size = 3, alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
    geom_errorbar(aes(ymin = Mean1 - SD1, ymax = Mean1 + SD1, color = Climate.zone), width = 0.001, alpha = 0.5) +
    geom_errorbarh(aes(xmin = Mean2 - SD2, xmax = Mean2 + SD2, color = Climate.zone), height = 0.001, alpha = 0.5) +
    scale_shape_manual(values = shape_mapping) +
    labs(title = paste(this_comparison@source1@name, "vs", this_comparison@source2@name),
         x = paste(this_comparison@source2@name, Benchmark@id, Benchmark@unit),
         y = paste("Model", Benchmark@id, Benchmark@unit),
         color = "Climate Zone") +
    guides(fill = FALSE) +  # This removes the fill legend
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          axis.text = element_text(size = 15),
          legend.key = element_blank(),
          legend.box.background = element_rect(color = "transparent", fill = "transparent"))
  
  print(plot1)
  
  
  # densplot1<-ggplot(data = this_comparison@data, aes(x  = this_comparison@data[[6]], y = this_comparison@data[[5]],color=Climate.zone, shape = Season)) +
  #   #geom_bin2d(bins = 30) +
  #   scale_fill_continuous(type = "viridis") +
  #   theme_bw()+
  #   geom_point(size = 2, alpha = 0.6)+
  #   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size=1)+
  #   
  #   labs(title = paste(this_comparison@source1@name, "vs", Benchmark@datasets[[1]]@source@name),
  #        x = paste(Benchmark@datasets[[1]]@source@name, Benchmark@id, Benchmark@unit),
  #        y = paste("Model", Benchmark@id, Benchmark@unit),
  #        color = "Climate Zone") +
  #   theme(legend.position = "bottom",
  #         legend.direction = "vertical",
  #         legend.title = element_text(size = 12, face = "bold"),
  #         legend.text = element_text(size = 12),
  #         plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
  #         axis.title.x = element_text(size = 25),
  #         axis.title.y = element_text(size = 25),
  #         axis.text = element_text(size = 15))+
  #   coord_fixed(ratio = 1)+
  #   guides(color = guide_legend(override.aes = list(size = 5)))
  #   plot(densplot1)
}
}else if (fill == "landcover"){
  for (i in seq_along(all_comparisons[[1]])) {
    this_comparison <- all_comparisons[[1]][[i]]
    stations <- read.csv(file.path(system.file("extdata/ICOS/ICOS_stations_info.csv", package = "DGVMBenchmarks")), header = T,sep = ";")
    stations$Lon <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 1))
    stations$Lat <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 2))
    decimals <- 4  # Specify the number of decimal places you want to round to
    
    # Round Lon and Lat columns in stations
    stations$Lon <- round(as.numeric(stations$Lon), decimals)
    stations$Lat <- round(as.numeric(stations$Lat), decimals)
    
    # Round Lon and Lat columns in this_Field@data
    this_comparison@data$Lon <- round(this_comparison@data$Lon, decimals)
    this_comparison@data$Lat <- round(this_comparison@data$Lat, decimals)
    # Remove Position column
    stations <- stations[, -which(names(stations) == "Position")]
    # Merge based on matching latitude and longitude coordinates
    merged_data <- merge(this_comparison@data, stations[, c("Lon", "Lat", "Site.type", "Name", "Climate.zone")], by = c("Lon", "Lat"), all.x = TRUE)
    
    # Add the Site.type column to this_comparison@data
    this_comparison@data$Site.type <- merged_data$Site.type
    this_comparison@data$Name <- merged_data$Name
    
    field_data <- this_comparison@data
    
    # Add the Season column using case_when
    field_data <- field_data %>%
      mutate(Season = case_when(
        between(Day, 80, 171) ~ "Spring",
        between(Day, 172, 263) ~ "Summer",
        between(Day, 264, 355) ~ "Autumn",
        TRUE ~ "Winter"
      ))
    
    # Assign the modified data back to the list
    this_comparison@data <- field_data
    
    unique_seasons <- unique(this_comparison@data$Season)
    unique_sites <- unique(this_comparison@data$Name)
    
    meanSeasonal <- data.frame(Name = character(), 
                               Season = character(), 
                               Mean1 = numeric(),
                               Mean2 = numeric(),
                               SD1 = numeric(),
                               SD2 = numeric(),
                               Min1 = numeric(),
                               Min2 = numeric(),
                               Max1 = numeric(),
                               Max2 = numeric(),
                               Land.use = character())
    
    for (season in unique_seasons) {
      for (site in unique_sites) {
        # Filter data for specific site and season
        temp_data <- filter(this_comparison@data, Season == season, Name == site)
        
        land_use <- unique(temp_data$Site.type)
        # Calculate mean 
        mean_value1 <- mean(temp_data[[5]], na.rm = TRUE)
        mean_value2 <- mean(temp_data[[6]], na.rm = TRUE)
        std_val1 <- sd(temp_data[[5]], na.rm = TRUE)
        std_val2 <-  sd(temp_data[[6]], na.rm = TRUE)
        Min_val1 <- min(temp_data[[5]], na.rm = TRUE)
        Min_val2 <-  min(temp_data[[6]], na.rm = TRUE)
        Max_val1 <- max(temp_data[[5]], na.rm = TRUE)
        Max_val2 <-  max(temp_data[[6]], na.rm = TRUE)
        # Append results
        meanSeasonal <- rbind(meanSeasonal, data.frame(Name = site, Season = season, Land.use = land_use, Mean1 = mean_value1, Mean2 = mean_value2, SD1 = std_val1, SD2 = std_val2, Min1 = Min_val1, Min2 = Min_val2, Max1 = Max_val1, Max2 = Max_val2))
      }
    }
    
    
    shape_mapping <- c("Winter" = 21, "Spring" = 22, "Summer" = 23, "Autumn" = 24)
    
    plot1 <- ggplot(data = meanSeasonal, aes(x = Mean2, y = Mean1, color = Land.use, shape = Season)) +
      geom_point(aes(fill = Land.use), size = 3, alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
      geom_errorbar(aes(ymin = Mean1 - SD1, ymax = Mean1 + SD1, color = Land.use), width = 0.001, alpha = 0.5) +
      geom_errorbarh(aes(xmin = Mean2 - SD2, xmax = Mean2 + SD2, color = Land.use), height = 0.001, alpha = 0.5) +
      scale_shape_manual(values = shape_mapping) +
      labs(title = paste(this_comparison@source1@name, "vs", this_comparison@source2@name),
           x = paste(this_comparison@source2@name, Benchmark@id, Benchmark@unit),
           y = paste("Model", Benchmark@id, Benchmark@unit),
           color = "Land Cover") +
      guides(fill = FALSE) +  # This removes the fill legend
      theme_bw() +
      theme(legend.position = "bottom",
            legend.direction = "vertical",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
            axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 25),
            axis.text = element_text(size = 15),
            legend.key = element_blank(),
            legend.box.background = element_rect(color = "transparent", fill = "transparent"))
    
    print(plot1)
    
    # densplot1<-ggplot(data = this_comparison@data, aes(x  = this_comparison@data[[6]], y = this_comparison@data[[5]],color=Site.type, shape = Season)) +
    #   #geom_bin2d(bins = 30) +
    #   scale_fill_continuous(type = "viridis") +
    #   theme_bw()+
    #   geom_point(size = 2, alpha = 0.6)+
    #   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size=1)+
    #   
    #   labs(title = paste(this_comparison@source1@name, "vs", Benchmark@datasets[[1]]@source@name),
    #        x = paste(Benchmark@datasets[[1]]@source@name, Benchmark@id, Benchmark@unit),
    #        y = paste("Model", Benchmark@id, Benchmark@unit),
    #        color = "Land Use") +
    #   theme(legend.position = "bottom",
    #         legend.direction = "horizontal",
    #         legend.title = element_text(size = 12, face = "bold"),
    #         legend.text = element_text(size = 12),
    #         plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
    #         axis.title.x = element_text(size = 25),
    #         axis.title.y = element_text(size = 25),
    #         axis.text = element_text(size = 15))+
    #   coord_fixed(ratio = 1)+
    #   guides(color = guide_legend(override.aes = list(size = 5)))
    # plot(densplot1)
  }
} else if (fill == "country"){
  for (i in seq_along(all_comparisons[[1]])) {
    this_comparison <- all_comparisons[[1]][[i]]
    stations <- read.csv(file.path(system.file("extdata/ICOS/ICOS_stations_info.csv", package = "DGVMBenchmarks")), header = T,sep = ";")
    stations$Lon <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 1))
    stations$Lat <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 2))
    decimals <- 4  # Specify the number of decimal places you want to round to
    
    # Round Lon and Lat columns in stations
    stations$Lon <- round(as.numeric(stations$Lon), decimals)
    stations$Lat <- round(as.numeric(stations$Lat), decimals)
    
    # Round Lon and Lat columns in this_Field@data
    this_comparison@data$Lon <- round(this_comparison@data$Lon, decimals)
    this_comparison@data$Lat <- round(this_comparison@data$Lat, decimals)
    # Remove Position column
    stations <- stations[, -which(names(stations) == "Position")]
    # Merge based on matching latitude and longitude coordinates
    merged_data <- merge(this_comparison@data, stations[, c("Lon", "Lat", "Site.type", "Name", "Climate.zone", "Country")], by = c("Lon", "Lat"), all.x = TRUE)
    
    # Add the Site.type column to this_comparison@data
    this_comparison@data$Site.type <- merged_data$Country
    this_comparison@data$Name <- merged_data$Name
    
    field_data <- this_comparison@data
    
    # Add the Season column using case_when
    field_data <- field_data %>%
      mutate(Season = case_when(
        between(Day, 80, 171) ~ "Spring",
        between(Day, 172, 263) ~ "Summer",
        between(Day, 264, 355) ~ "Autumn",
        TRUE ~ "Winter"
      ))
    
    # Assign the modified data back to the list
    this_comparison@data <- field_data
    
    unique_seasons <- unique(this_comparison@data$Season)
    unique_sites <- unique(this_comparison@data$Name)
    
    meanSeasonal <- data.frame(Name = character(), 
                               Season = character(), 
                               Mean1 = numeric(),
                               Mean2 = numeric(),
                               SD1 = numeric(),
                               SD2 = numeric(),
                               Min1 = numeric(),
                               Min2 = numeric(),
                               Max1 = numeric(),
                               Max2 = numeric(),
                               Country = character())
    
    for (season in unique_seasons) {
      for (site in unique_sites) {
        # Filter data for specific site and season
        temp_data <- filter(this_comparison@data, Season == season, Name == site)
        
        Country <- unique(temp_data$Site.type)
        # Calculate mean 
        mean_value1 <- mean(temp_data[[5]], na.rm = TRUE)
        mean_value2 <- mean(temp_data[[6]], na.rm = TRUE)
        std_val1 <- sd(temp_data[[5]], na.rm = TRUE)
        std_val2 <-  sd(temp_data[[6]], na.rm = TRUE)
        Min_val1 <- min(temp_data[[5]], na.rm = TRUE)
        Min_val2 <-  min(temp_data[[6]], na.rm = TRUE)
        Max_val1 <- max(temp_data[[5]], na.rm = TRUE)
        Max_val2 <-  max(temp_data[[6]], na.rm = TRUE)
        # Append results
        meanSeasonal <- rbind(meanSeasonal, data.frame(Name = site, Season = season, Country = Country, Mean1 = mean_value1, Mean2 = mean_value2, SD1 = std_val1, SD2 = std_val2, Min1 = Min_val1, Min2 = Min_val2, Max1 = Max_val1, Max2 = Max_val2))
      }
    }
    
    
    shape_mapping <- c("Winter" = 21, "Spring" = 22, "Summer" = 23, "Autumn" = 24)
    
    plot1 <- ggplot(data = meanSeasonal, aes(x = Mean2, y = Mean1, color = Country, shape = Season)) +
      geom_point(aes(fill = Country), size = 3, alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
      geom_errorbar(aes(ymin = Mean1 - SD1, ymax = Mean1 + SD1, color = Country), width = 0.001, alpha = 0.5) +
      geom_errorbarh(aes(xmin = Mean2 - SD2, xmax = Mean2 + SD2, color = Country), height = 0.001, alpha = 0.5) +
      scale_shape_manual(values = shape_mapping) +
      labs(title = paste(this_comparison@source1@name, "vs", this_comparison@source2@name),
           x = paste(this_comparison@source2@name, Benchmark@id, Benchmark@unit),
           y = paste("Model", Benchmark@id, Benchmark@unit),
           color = "Country") +
      guides(fill = FALSE) +  # This removes the fill legend
      theme_bw() +
      theme(legend.position = "bottom",
            legend.direction = "vertical",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
            axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 25),
            axis.text = element_text(size = 15),
            legend.key = element_blank(),
            legend.box.background = element_rect(color = "transparent", fill = "transparent"))
    
    print(plot1)
    
    # densplot1<-ggplot(data = this_comparison@data, aes(x  = this_comparison@data[[6]], y = this_comparison@data[[5]],color=Country, shape = Season)) +
    #   #geom_bin2d(bins = 30) +
    #   scale_fill_continuous(type = "viridis") +
    #   theme_bw()+
    #   geom_point(size = 2, alpha = 0.6)+
    #   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size=1)+
    #   
    #   labs(title = paste(this_comparison@source1@name, "vs", Benchmark@datasets[[1]]@source@name),
    #        x = paste(Benchmark@datasets[[1]]@source@name, Benchmark@id, Benchmark@unit),
    #        y = paste("Model", Benchmark@id, Benchmark@unit),
    #        color = "Stations") +
    #   theme(legend.position = "bottom",
    #         legend.direction = "horizontal",
    #         legend.title = element_text(size = 12, face = "bold"),
    #         legend.text = element_text(size = 12),
    #         plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
    #         axis.title.x = element_text(size = 25),
    #         axis.title.y = element_text(size = 25),
    #         axis.text = element_text(size = 15))+
    #   coord_fixed(ratio = 1)+
    #   guides(color = guide_legend(override.aes = list(size = 5)))
    # plot(densplot1)
  }
}else if (fill == "station"){
  for (i in seq_along(all_comparisons[[1]])) {
    this_comparison <- all_comparisons[[1]][[i]]
    stations <- read.csv(file.path(system.file("extdata/ICOS/ICOS_stations_info.csv", package = "DGVMBenchmarks")), header = T,sep = ";")
    stations$Lon <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 1))
    stations$Lat <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 2))
    decimals <- 4  # Specify the number of decimal places you want to round to
    
    # Round Lon and Lat columns in stations
    stations$Lon <- round(as.numeric(stations$Lon), decimals)
    stations$Lat <- round(as.numeric(stations$Lat), decimals)
    
    # Round Lon and Lat columns in this_Field@data
    this_comparison@data$Lon <- round(this_comparison@data$Lon, decimals)
    this_comparison@data$Lat <- round(this_comparison@data$Lat, decimals)
    # Remove Position column
    stations <- stations[, -which(names(stations) == "Position")]
    # Merge based on matching latitude and longitude coordinates
    merged_data <- merge(this_comparison@data, stations[, c("Lon", "Lat", "Site.type", "Name", "Climate.zone")], by = c("Lon", "Lat"), all.x = TRUE)
    
    # Add the Site.type column to this_comparison@data
    this_comparison@data$Site.type <- merged_data$Site.type
    this_comparison@data$Name <- merged_data$Name
    
    field_data <- this_comparison@data
    
    # Add the Season column using case_when
    field_data <- field_data %>%
      mutate(Season = case_when(
        between(Day, 80, 171) ~ "Spring",
        between(Day, 172, 263) ~ "Summer",
        between(Day, 264, 355) ~ "Autumn",
        TRUE ~ "Winter"
      ))
    
    # Assign the modified data back to the list
    this_comparison@data <- field_data
    
    unique_seasons <- unique(this_comparison@data$Season)
    unique_sites <- unique(this_comparison@data$Name)
    
    meanSeasonal <- data.frame(Name = character(), 
                               Season = character(), 
                               Mean1 = numeric(),
                               Mean2 = numeric(),
                               SD1 = numeric(),
                               SD2 = numeric(),
                               Min1 = numeric(),
                               Min2 = numeric(),
                               Max1 = numeric(),
                               Max2 = numeric(),
                               Land.use = character())
    
    for (season in unique_seasons) {
      for (site in unique_sites) {
        # Filter data for specific site and season
        temp_data <- filter(this_comparison@data, Season == season, Name == site)
        
        land_use <- unique(temp_data$Site.type)
        # Calculate mean 
        mean_value1 <- mean(temp_data[[5]], na.rm = TRUE)
        mean_value2 <- mean(temp_data[[6]], na.rm = TRUE)
        std_val1 <- sd(temp_data[[5]], na.rm = TRUE)
        std_val2 <-  sd(temp_data[[6]], na.rm = TRUE)
        Min_val1 <- min(temp_data[[5]], na.rm = TRUE)
        Min_val2 <-  min(temp_data[[6]], na.rm = TRUE)
        Max_val1 <- max(temp_data[[5]], na.rm = TRUE)
        Max_val2 <-  max(temp_data[[6]], na.rm = TRUE)
        # Append results
        meanSeasonal <- rbind(meanSeasonal, data.frame(Name = site, Season = season, Land.use = land_use, Mean1 = mean_value1, Mean2 = mean_value2, SD1 = std_val1, SD2 = std_val2, Min1 = Min_val1, Min2 = Min_val2, Max1 = Max_val1, Max2 = Max_val2))
      }
    }
    
    
    shape_mapping <- c("Winter" = 21, "Spring" = 22, "Summer" = 23, "Autumn" = 24)
    
    plot1 <- ggplot(data = meanSeasonal, aes(x = Mean2, y = Mean1, color = Name, shape = Season)) +
      geom_point(aes(fill = Name), size = 3, alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
      geom_errorbar(aes(ymin = Mean1 - SD1, ymax = Mean1 + SD1, color = Name), width = 0.001, alpha = 0.5) +
      geom_errorbarh(aes(xmin = Mean2 - SD2, xmax = Mean2 + SD2, color = Name), height = 0.001, alpha = 0.5) +
      scale_shape_manual(values = shape_mapping) +
      labs(title = paste(this_comparison@source1@name, "vs", this_comparison@source2@name),
           x = paste(this_comparison@source2@name, Benchmark@id, Benchmark@unit),
           y = paste("Model", Benchmark@id, Benchmark@unit),
           color = "Station") +
      guides(fill = FALSE) +  # This removes the fill legend
      theme_bw() +
      theme(legend.position = "bottom",
            legend.direction = "vertical",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
            axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 25),
            axis.text = element_text(size = 15),
            legend.key = element_blank(),
            legend.box.background = element_rect(color = "transparent", fill = "transparent"))
    
    print(plot1)
}
  }else {
  for (i in seq_along(all_comparisons[[1]])) {
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