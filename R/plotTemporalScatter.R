#' Plot temporal scatterplots. Plotting the mean per site with possebility of uncertainty metrics.
#'
#' @param Benchmark The current benchmark
#' @param all_comparisons the list holding comparisons
#' @param uncertainty What method of uncertainty should be used "SD" or "Range" default = "none"
#'
#' @return Scatterplot of mean site measurments
#' @export
#'
#' @examples plotTemporalScatter(Benchmark = this_benchmark, all_comparisons = all_comparisons, uncertainty = "SD")
#' @author Karl piltz (karl.piltz@@nateko.lu.se)
plotTemporalScatter <- function(Benchmark = this_benchmark, all_comparisons = all_comparisons, uncertainty = "none"){
    
    
if (all_comparisons[["Values"]][[1]]@source2@format@id == "SITE"){
  PROFOUND <- read.csv(file.path(system.file("extdata/PROFOUND/PROFOUND_Grid_List.csv", package = "DGVMBenchmarks")), header = T,sep = ",")
    setDT(PROFOUND)
    for (i in seq_along(all_comparisons)) {
      # Merge and keep only the 'Site' column from the PROFOUND dataset
      all_comparisons[[1]][[i]]@data <- merge(
        x = all_comparisons[[1]][[i]]@data,
        y = PROFOUND[, .(Lat, Lon, Site)],
        by = c("Lon", "Lat"),
        all.x = TRUE
      )
    }
          }
    
    for (this_comparison in all_comparisons[[1]]){
      unique_sites <- unique(this_comparison@data$Site)
      
      meanSeasonal <- data.frame(Site = character(), 
                                 Mean1 = numeric(),
                                 Mean2 = numeric(),
                                 SD1 = numeric(),
                                 SD2 = numeric(),
                                 Min1 = numeric(),
                                 Min2 = numeric(),
                                 Max1 = numeric(),
                                 Max2 = numeric())
      
      
        for (site in unique_sites) {
          # Filter data for specific site and season
          temp_data <- filter(this_comparison@data, Site == site)
          
          
          # Calculate mean 
          mean_value1 <- mean(temp_data[[4]], na.rm = TRUE)
          mean_value2 <- mean(temp_data[[5]], na.rm = TRUE)
          std_val1 <- sd(temp_data[[4]], na.rm = TRUE)
          std_val2 <-  sd(temp_data[[5]], na.rm = TRUE)
          Min_val1 <- min(temp_data[[4]], na.rm = TRUE)
          Min_val2 <-  min(temp_data[[5]], na.rm = TRUE)
          Max_val1 <- max(temp_data[[4]], na.rm = TRUE)
          Max_val2 <-  max(temp_data[[5]], na.rm = TRUE)
          # Append results
          meanSeasonal <- rbind(meanSeasonal, data.frame(Site = site, Mean1 = mean_value1, Mean2 = mean_value2, SD1 = std_val1, SD2 = std_val2, Min1 = Min_val1, Min2 = Min_val2, Max1 = Max_val1, Max2 = Max_val2))
        }
      
  
      plot1 <- ggplot(data = meanSeasonal, aes(x = Mean2, y = Mean1, color = Site)) +
        geom_point(aes(fill = Site), size = 3, alpha = 0.6) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
        labs(title = paste(this_comparison@source1@name, "vs", this_comparison@source2@name),
             x = paste(this_comparison@source2@name, Benchmark@id, Benchmark@unit),
             y = paste("Model", Benchmark@id, Benchmark@unit),
             color = "Site") +
        guides(fill = FALSE) + # This removes the fill legend
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
              legend.box.background = element_rect(color = "transparent", fill = "transparent")) +
        coord_cartesian(xlim = c(min(meanSeasonal$Min2, meanSeasonal$Min1), max(meanSeasonal$Max2, meanSeasonal$Max1)), 
                        ylim = c(min(meanSeasonal$Min1, meanSeasonal$Min2), max(meanSeasonal$Max1, meanSeasonal$Max2)))
      
      # Conditional addition of error bars based on the uncertainty type
      if (uncertainty == "SD") {
        plot1 <- plot1 + 
          geom_errorbar(aes(ymin = Mean1 - SD1, ymax = Mean1 + SD1), width = 0.2, alpha = 0.5) +
          geom_errorbarh(aes(xmin = Mean2 - SD2, xmax = Mean2 + SD2), height = 0.2, alpha = 0.5)
      }
      
      if (uncertainty == "Range") {
        plot1 <- plot1 +
          geom_errorbar(aes(ymin = Min1, ymax = Max1), width = 0.2, alpha = 0.5) +
          geom_errorbarh(aes(xmin = Min2, xmax = Max2), height = 0.2, alpha = 0.5)
      }
      
      print(plot1)
    }
    }
   
    