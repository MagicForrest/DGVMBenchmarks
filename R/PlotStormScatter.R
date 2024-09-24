#' Storm disturbance scatter
#'
#' @param benchmark The current benchmark.
#' @param all_sim_full List holding simulations.
#' @param do_plots External argument TRUE or FALSE if plot is to be rendered or not.
#' @param anno.hjust Horizontal adjustment of equation annotation. 
#' @param anno.vjust Vertical adjustment of equation annotation. 
#' @return Scatterplot of modelled and observed damage
#' @export
#'
#' @examples
#' @author Karl Piltz (karl.piltz@@nateko.lu.se), Fredrik Lagergren (fredrik.lagergren@@nateko.lu.se)
plotStormScatter <- function(benchmark = this_benchmark, all_sim_full, do_plots = do_plots, anno.hjust = 0, anno.vjust = 0){
  # Define paths
  areapath <- file.path(system.file("extdata", "Storm", "GridcellFractionsEMEP", package = "DGVMBenchmarks"))
  
  input_dir <- file.path(system.file("extdata", "Storm", package = "DGVMBenchmarks"))
  LC_file <- file.path(input_dir, "LC_europe_nat_for_1801_2010_Pucher_noNatural.txt")
  
  
  lulist <- fread(LC_file)  # Assuming lumap.csv contains long, lat, and land use data
  lulist$Year <- lulist$year 
  # Assuming lulist is already loaded as a data.table and Lon, Lat adjusted
  lulist[, `:=`(Lon = Lon - 0.25, Lat = Lat - 0.25)]
  
  # Define constants
  years <- 210
  gridcells <- nrow(lulist) / years
  ages <- seq(210, 1, by = -1)
  
  # Calculate grid indices
  lulist[, `:=`(lonnr = (Lon - min(Lon)) * 2 + 1, latnr = (Lat - min(Lat)) * 2 + 1)]
  
  # Calculate forest age within the data.table
  lulist[, agetemp := c(
    FOREST[1] / (1 - BARREN[1]), # For the first year
    (FOREST[2:.N] - FOREST[1:(.N-1)]) / (1 - BARREN[1]) # For subsequent years
  ), by = .(Lon, Lat)]
  
  # Sum weighted forest ages
  lulist[, FOREST_AGE := sum(agetemp * ages), by = .(Lon, Lat)]
  
  # Filter for year 2010 to create the final lumap data.table
  lulist_2010 <- lulist[year == 2010, .(Lon, Lat, NATURAL, FOREST, BARREN, FOREST_AGE)]
  
  # Reading wind damage probability data
  # Load the data
  input_dir <- file.path(system.file("extdata", "Storm", package = "DGVMBenchmarks"))
  wind_dist_file <- file.path(input_dir, "wdp_05deg_from01deg_colinear.txt")
  wdplist <- fread(wind_dist_file, sep = "\t", header = TRUE)
  
  # Adjust coordinates (shift from center to SW corner)
  wdplist[, `:=`(Lon = Lon - 0.25, Lat = Lat - 0.25)]
  iteration_tables <- list()
  for(this_sim in all_sim_full){
    
    # Load the data
    stormlist <- this_sim@data
    
    # Define years and grid cells
    stormlist[, `:=`(Lon = Lon - 0.25, Lat = Lat - 0.25)]
    
    # Define the range of years for which you want to calculate the means
    year_range <- benchmark@first.year : benchmark@last.year
    
    # Filter for the years 1986-2020 and calculate the mean for each Lon/Lat
    storm_means <- stormlist[Year %in% year_range, 
                             .(DamWoodC_mean = mean(get(benchmark@guess_layers), na.rm = TRUE)), 
                             by = .(Lon, Lat)]
    
    
    stormWDP <- merge(storm_means,wdplist, by = c("Lon", "Lat"), all = F)
    
    final_merge <- merge(stormWDP, lulist_2010, by = c("Lon", "Lat"), all = F)
    # Create the uncalibrated damage map
    final_merge$damagemap_uncall <- final_merge$DamWoodC_mean * final_merge$WDP
    
    # Calibration
    # Define conversion factors
    kgtoton <- 0.001 # Ton per kg
    stemfrac <- 0.65 # Fraction of biomass that is stem
    cfrac <- 0.5 # Carbon fraction of biomass
    dens <- 0.4 # Density of wood
    m2toha <- 0.0001 # Hectar per m2
    cmtovol <- kgtoton * stemfrac / cfrac / dens / m2toha # Conversion factor
    
    
    # Define countries and initialize results
    countrycode <- c('ES', 'FR', 'BE', 'NL', 'CH', 'SE', 'CZ', 'DE', 'PL', 'FI')
    results <- data.table(country = countrycode, mod_totdam_uncall = NA_real_, forestarea = NA_real_)
    
    # Define a function to calculate area of a 0.5-degree cell by latitude
    calc_cell_area <- function(lat) {
      latC <- lat + 0.25
      -28.562 * latC^2 - 1229.7 * latC + 331456
    }
    
    # Process each country
    for (i in seq_along(countrycode)) {
      country <- countrycode[i]
      
      # Read the fraction data for each country
      thefile <- file.path(areapath,  paste0(country, ".csv"))
      aflist <- fread(thefile, sep = ";", header = TRUE)
      aflist <- aflist[, .(Lon = floor(longitude * 2) / 2, Lat = floor(latitude * 2) / 2, Fraction = fraction)]
      
      # Merge with the main data table
      country_data <- merge(final_merge, aflist, by = c("Lon", "Lat"), all.x = TRUE)
      
      # Calculate the area of each grid cell
      country_data[, Area := calc_cell_area(Lat) * Fraction / 25]
      
      # Calculate total damage using direct column operations
      country_data[, totdam := DamWoodC_mean * cmtovol * 35 * WDP * Area * FOREST]
      
      # Summarize results
      mod_totdam <- sum(country_data$totdam, na.rm = TRUE)
      forest_area <- sum(country_data$Area * country_data$FOREST, na.rm = TRUE)
      # Store results in the results data.table
      results[i, `:=`(mod_totdam_uncall = mod_totdam, forestarea = forest_area)]
    }
    
    # Define the data
    DFDE1986_2019 <- c(
      0.311712469,  # Spain
      15.92983022,  # France
      8.624930994,  # Belgium
      1.83575288,   # Netherlands
      18.95101885,  # Switzerland
      5.811679096,  # Sweden
      38.41414753,  # Czech Republic
      17.37980347,  # Germany
      6.684530283,  # Poland
      1.54078413    # Finland
    )
    
    DFDE2020 <- c(
      0.0000, # Spain
      0.0000, # France
      0.0000, # Belgium
      0.0000, # Netherlands
      0.5906, # Switzerland
      0.0000, # Sweden
      0.0000, # Czechia
      1.7410, # Germany
      0.0000, # Poland
      0.0000  # Finland
    )
    
    # Calculate total DFDE damage 1986-2020 by country (m3)
    DFDEdam <- DFDE1986_2019 * results$forestarea + DFDE2020 * 1000000
    
    # Perform linear regression through the origin
    xdata <- results$mod_totdam_uncall / 1000000  # Modelled total damage (milj m3)
    ydata <- DFDEdam / 1000000            # Reported total damage (milj m3)
    
    # Fit linear model through the origin
    fit <- lm(ydata ~ 0 + xdata)
    slope <- coef(fit)[1]                 # Slope of the line
    R2 <- summary(fit)$adj.r.squared      # Adjusted R^2
    
    # Calibration of the maps
    final_merge$damagemap_call <- final_merge$damagemap_uncall * slope  # Calibrated map of modelled damage
    final_merge$stormmap_call <- final_merge$DamWoodC_mean * slope           # Calibrated stormmap
    
    # Create a table of total modelled and reported damage 1986-2020 for export
    country_table <- data.table(
      Country = countrycode,
      Reported_Damage = ydata,
      Modelled_Damage_Before_Calibration = xdata,
      Modelled_Damage_After_Calibration = xdata * slope
    )
    
    iteration_tables[[this_sim@source@name]] <- country_table
    # SCATTERPLOT OF MODELLED AND REPORTED DAMAGE
    # Plot the modelled total damage 1986-2020 by country against DFDE reported values
    
    # Common range for x and y axes
    x_range <- range(country_table$Reported_Damage, na.rm = TRUE)
    y_range <- range(country_table$Modelled_Damage_Before_Calibration, na.rm = TRUE)
    common_range <- c(min(c(x_range[1], y_range[1])), max(c(x_range[2], y_range[2])))
    
    # Define the plot
    storm_scatter <- ggplot(country_table,
                            aes(x = Reported_Damage,
                                y = Modelled_Damage_Before_Calibration)) +
      geom_point(size = 3.5,
                 shape = 21,
                 fill = "#56B4E9",
                 color = "#0072B2",
                 stroke = 0.7) +  
      geom_text_repel(aes(label = Country),
                      size = 6, max.overlaps = 10,
                      box.padding = 0.4,
                      segment.color = "grey50") +  # Repel labels
      geom_abline(slope = slope,
                  intercept = 0,
                  linetype = "dotted",
                  color = "darkred",
                  size = 1.2) +  # Prominent regression line
      labs(
        x = expression("Total Reported Damage (Milj m"^3*")"),
        y = expression("Total Modelled Damage (Milj m"^3*")"),
        title = paste("Reported vs Modelled Damage:", benchmark@first.year, "-", benchmark@last.year),
        subtitle = this_sim@source@id  
      ) +
      annotate(
        "text",
        x = common_range[2] - anno.hjust,
        y = common_range[2] - anno.vjust,
        label = sprintf("y = %.3fx\nR² = %.2f", slope, R2),
        hjust = 1,
        vjust = 1,
        size = 7,
        color = "black",
        fontface = "italic"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 18, margin = margin(t = 5, b = 15)),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(size = 15),
        panel.grid.major = element_line(size = 0.3, color = "grey85"),  # Very light grid lines
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 10, 10, 10),  # Adequate spacing around the plot
        legend.position = "none"  # Remove legend if not needed
      ) +
      coord_fixed(ratio = 1) +  # Ensure the plot is square
      scale_x_continuous(limits = common_range) +  # Set x axis limits
      scale_y_continuous(limits = common_range)  # Set y axis limits
    
    # Plot scatter plot
    if (do_plots){plot(storm_scatter)}
  }
  
  renamed_tables <- list()
  
  # Iterate over each table in iteration_tables and rename the columns
  for (iteration_name in names(iteration_tables)) {
    
    # Extract the table for the current iteration
    current_table <- iteration_tables[[iteration_name]]
    
    # Rename the columns for this iteration
    setnames(current_table,
             old = c("Modelled_Damage_Before_Calibration", "Modelled_Damage_After_Calibration"),
             new = c(paste0(iteration_name, " Before Calibration"), paste0(iteration_name, " After Calibration")))
    
    # Store the renamed table in the list
    renamed_tables[[iteration_name]] <- current_table
  }
  
  # Now combine all renamed tables by merging on 'Country' and 'Reported_Damage'
  if(length(renamed_tables) > 1){
  final_table <- merge(renamed_tables[[1]], renamed_tables[[2]], by = c("Country", "Reported_Damage"))}
  
  return(country_table)
}