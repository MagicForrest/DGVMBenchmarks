#' plotStormMap
#'
#' @param benchmark The current benchmark.
#' @param all_sim_full List holding simulations.
#'
#' @return
#' @export
#'
#' @examples
#' @author Karl Piltz (karl.piltz@@nateko.lu.se), Fredrik Lagergren (fredrik.lagergren@@nateko.lu.se)
plotStormMap <- function(benchmark = this_benchmark, all_sim_full){


# Define paths
path <- 'C:\\Users\\Admin\\Documents\\tellus\\Storm_disturbance\\run_240809_sim4_age_probharv_satdist07\\'
areapath <- 'C:\\Users\\Admin\\Documents\\tellus\\Storm_disturbance\\GridcellFractionsEMEP\\'


lulist <- fread("C:\\Users\\Admin\\Documents\\tellus\\Storm_disturbance\\LC_europe_nat_for_1801_2010_Pucher_noNatural.txt")  # Assuming lumap.csv contains long, lat, and land use data
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

# View the resulting data.table
head(lulist_2010)




# Reading wind damage probability data
# Load the data
wdplist <- fread('C:/Users/Admin/Documents/tellus/Storm_disturbance/wdp_05deg_from01deg_colinear.txt', sep = "\t", header = TRUE)

# Adjust coordinates (shift from center to SW corner)
wdplist[, `:=`(Lon = Lon - 0.25, Lat = Lat - 0.25)]

for(this_sim in all_sim_full){
  
  # Load the data
  stormlist <- this_sim@data
  
  # Define years and grid cells
  stormlist[, `:=`(Lon = Lon - 0.25, Lat = Lat - 0.25)]
  
  # Define the range of years for which you want to calculate the means
  year_range <- benchmark@first.year : benchmark@last.year
  
  # Filter for the years 1986-2020 and calculate the mean for each Lon/Lat
  storm_means <- stormlist[Year %in% year_range, 
                           .(DamWoodC_mean = mean(benchmark@guess_layers, na.rm = TRUE)), 
                           by = .(Lon, Lat)]
  
  
  stormWDP <- merge(storm_means,wdplist, by = c("Lon", "Lat"), all = F)
  
  final_merge <- merge(stormWDP, lulist_2010, by = c("Lon", "Lat"), all = F)
  # Create the uncalibrated damage map
  final_merge$expected_uncall <- final_merge$DamWoodC_mean * final_merge$WDP
  
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
    thefile <- sprintf('%s%s.csv', areapath, country)
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
  final_merge$expected_call <- final_merge$expected_uncall * slope  # Calibrated map of modelled damage
  final_merge$stormmap_call <- final_merge$DamWoodC_mean * slope           # Calibrated stormmap
  
 this_sim@data <- final_merge
 
 p1 <- DGVMTools::plotSpatial(this_sim,
                              layers = c("stormmap_call",
                                         "expected_call"),
                              map.overlay = "world",
                              panel.bg.col = "gray")+
   scale_fill_gradient2(low = "red",
                        high = "blue",
                        mid = "white",
                        midpoint = 0,
                        na.value = "black")+
   labs(title = paste(this_field@source@name))+
   theme(plot.title = element_text(size = 30),
         plot.subtitle = element_text(size = 20),
         axis.title.x = element_text(size = 25),
         axis.title.y = element_text(size = 25),
         legend.text = element_text(size = 17),
         legend.title = element_text(size = 19),
         panel.border = element_rect(color = "black",
                                     fill = NA,
                                     size = 1))
 
 plot(p1)

}
}