
#' CpoolTable
#'
#' @param param Not used
#' @param benchmark the current benchmark object
#' @param all_sim_full List of simulations
#'
#' @return Table with cpool Litter, Soil & AGB per country
#' @export
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
#' @examples
CpoolTable <- function(param = NULL, benchmark = this_benchmark, all_sim_full = all_sim_full){
  

create_grid <- function(data, coords, cellsize = 0.5, crs = "EPSG:4326") {
  
  map_data_wgs <- sf::st_as_sf(data, 
                               coords=coords,
                               crs=crs,
                               remove=FALSE) 
  
  grid_geometries <- sf::st_make_grid(
    sf::st_bbox(map_data_wgs) + 
      c(-cellsize/2, -cellsize/2,
        cellsize/2, cellsize/2),
    what="polygons", cellsize=cellsize)
  
  grid_out <- data.frame(id = 1:length(grid_geometries)) %>%
    mutate(geometry = grid_geometries) %>%
    sf::st_as_sf(crs=crs) %>%
    sf::st_join(map_data_wgs)
  
  return(grid_out)
}

grid_table <- read.csv(file.path(system.file("extdata/Harvest/gridcell_country_cluster.csv", package = "DGVMBenchmarks")), row.names = 1)
grid_countries <- create_grid(grid_table, coords = c("lon05", "lat05"), cellsize = 0.5)

grid_countries <- grid_countries %>%
  st_transform(crs = "EPSG:3035")

grid_countries$area_m2 <- as.numeric(st_area(grid_countries))

LC2010 <- read.table(file.path(system.file("extdata/Harvest/LC_europe_nat_for_1801_2010_Pucher_noNatural_twin_2010-2010.tab", package = "DGVMBenchmarks")), header = T)

# Read and clean Forest Europe 2015 data
Forest_europe_2015 <- read_excel("C:/Users/Admin/Documents/tellus/Data_240809/harvest/Forest_Europe_2015.xlsx")
Forest_europe_2015$ABG <- Forest_europe_2015$Forest_agb + Forest_europe_2015$abg
Forest_europe_2015$BGB <- Forest_europe_2015$Forest_bgb + Forest_europe_2015$bgb
Forest_europe_2015$Litter <- Forest_europe_2015$Forest_litter + Forest_europe_2015$litter
Forest_europe_2015$Deadwood <- Forest_europe_2015$Forest_deadwood + Forest_europe_2015$deadwood
Forest_europe_2015$Litter <- Forest_europe_2015$Litter + Forest_europe_2015$Deadwood
Forest_europe_2015$Soil <- Forest_europe_2015$Forest_soil + Forest_europe_2015$soil

Forest_europe <- Forest_europe_2015 %>%
  mutate(Source = "ForestEurope") %>%
  mutate(
    country = Country,
    ABG_milj_ton = Forest_agb + abg,                  # Total above-ground biomass (million metric tonnes)
    Litter_million_tonnes = (Forest_litter + litter + Forest_deadwood + deadwood) ,  # Total litter and deadwood (million metric tonnes)
    Soil_million_tonnes = (Forest_soil + soil) ,               # Total soil (million metric tonnes)
    Forest_ha = Forest_1000_ha * 1000,
    Source = "ForestEurope") 

Forest_europe <- Forest_europe %>%
  select(country, Source , Litter_million_tonnes, Soil_million_tonnes)

# Read and clean data for AGB
AGB <- read_excel("C:/Users/Admin/Documents/tellus/Data_240809/harvest/Biomass_and_Increment_STATISTICS.xlsx", sheet = 2, skip = 2)
colnames(AGB) <- c("Country", "ISO", "NUTS", "Forest_Area_ha", "FAWS_ha", 
                   "FNAWS_ha", "AGB_Tons", "AGB_Tons_ha", "BAWS_Tons", "BNAWS_Tons", "Extra1", "Legend")
AGB <- AGB %>% 
  select(-Extra1, -Legend) %>% ## Rid data of extra columns
  mutate(AGB_million_tonnes = AGB_Tons / 1e6) %>% ## Convert AGB to million tonnes
  filter(nchar(NUTS) < 3) ## Remove regional, only keep country level total.
AGB <- AGB %>%
  mutate(country = Country,
         Source = "ForestEurope") %>%
  select(country, Source, AGB_million_tonnes )

results_cpool <- list()
results_AGB <- list()
for (this_sim in all_sim_full){
    # Total harvest per country -----------------------------------------------
    # (1) multiplying the harvested cmass with the expansion factors, 
    # (2) multiplying with the area of the grid cell, and 
    # (3) summing up the grid cells in a country
    
    ### for 2013-2017 (range represented by 2015 in SoEF) -----
    # SoilC and LitterC
  if (param == "Cpool"){
  cstock_2013_2017 <- this_sim@data %>%
    filter(Year %in% 2013:2017) %>%
    left_join(grid_countries, by = c(Lat = "lat05", Lon = "lon05")) %>%
    left_join(LC2010) %>%
    group_by(country, Year) %>%
    mutate(
      area_m2_FOREST = area_m2 * FOREST, ## Cell area by Forest land cover
      tot_cell_Litter_kg = (LitterC * FOREST) * area_m2_FOREST,   # Total litter carbon in kg per Forest grid cell area
      tot_cell_Soil_kg = (SoilC * FOREST) * area_m2_FOREST, # Total soil carbon in kg per Forest grid cell area
      Source = this_sim@source@id # Simulation source
    ) 
  
  # Summarize by country, source, and year
  cstock_2013_2017 <- cstock_2013_2017 %>%
    group_by(country, Source, Year) %>%
    summarise(
      # Total litter and soil carbon (kg) across all forest areas in million tonnes
      tot_cell_Litter_million_tonnes = sum(tot_cell_Litter_kg, na.rm = TRUE) / 1e9,  # Sum to country level and Convert kg to million tonnes
      tot_cell_Soil_million_tonnes = sum(tot_cell_Soil_kg, na.rm = TRUE) / 1e9,      # Sum to country level and Convert kg to million tonnes
      sim_forest_area_1000ha = sum(area_m2_FOREST) * 1e-07  # Convert m² to 1000 hectares
    )
  
  # Take the mean values for each country
  cstock_mean <- cstock_2013_2017 %>%
    group_by(country) %>%
    summarise(
      Litter_million_tonnes = mean(tot_cell_Litter_million_tonnes), # Country mean 
      Soil_million_tonnes = mean(tot_cell_Soil_million_tonnes),
      Source = this_sim@source@id
    )
  
  results_cpool[[this_sim@source@id]] <- cstock_mean
  }
  
  ## Above-ground biomass
  if (param == "AGB"){
    
  if(this_sim@source@id == "EFISCEN"){
    AGB_2018_2022 <- this_sim@data %>%
      left_join(grid_countries, by = c(Lat = "lat05", Lon = "lon05")) %>%
      left_join(LC2010) %>%
      group_by(country) %>%
      mutate(
        area_m2_FOREST = area_m2 * FOREST,
        AGB = !!sym(benchmark@guess_var) * 0.75, # Vegetation biomass to Above-ground biomass 
        tot_cell_AGB_kg = AGB * area_m2  ,    #(AGB / FOREST) * area_m2_FOREST,   # Total AGB carbon in kg per Forested grid cell area
        Source = this_sim@source@id # Simulation source
      ) 
    
    AGB_2018_2022 <- AGB_2018_2022 %>%
      filter(!country %in% c("Norway")) %>%
      group_by(country, Source) %>%
      summarise(
        tot_AGB_million_tonnes = sum(tot_cell_AGB_kg, na.rm = TRUE) / 1e9,  # Convert kg to million tonnes
      )
    
    AGB_mean <- AGB_2018_2022 %>%
      group_by(country) %>%
      summarise(
        AGB_million_tonnes = mean(tot_AGB_million_tonnes),
        Source = this_sim@source@id
      )
    results_AGB[[this_sim@source@id]] <- AGB_mean
    
    
  }
    else{
  AGB_2018_2022 <- this_sim@data %>%
    filter(Year %in% 2018:2022) %>%
    left_join(grid_countries, by = c(Lat = "lat05", Lon = "lon05")) %>%
    left_join(LC2010) %>%
    group_by(country, Year) %>%
    mutate(
      area_m2_FOREST = area_m2 * FOREST,
      AGB = !!sym(benchmark@guess_var) * 0.75, # Vegetation biomass to Above-ground biomass 
      tot_cell_AGB_kg =  AGB * area_m2 ,         #    (AGB / FOREST) * area_m2_FOREST,   # Total AGB carbon in kg per Forested grid cell area
      Source = this_sim@source@id # Simulation source
    ) 
  
  AGB_2018_2022 <- AGB_2018_2022 %>%
    filter(!country %in% c("Norway")) %>%
    group_by(country, Source, Year) %>%
    summarise(
      tot_AGB_million_tonnes = sum(tot_cell_AGB_kg, na.rm = TRUE) / 1e9,  # Convert kg to million tonnes
    )
  
  AGB_mean <- AGB_2018_2022 %>%
    group_by(country) %>%
    summarise(
      AGB_million_tonnes = mean(tot_AGB_million_tonnes),
      Source = this_sim@source@id
    )
  
  
  results_AGB[[this_sim@source@id]] <- AGB_mean
    }
  }
}
if (param == "cpool"){
final_results_cpool <- bind_rows(results_cpool)




Forest_europe_cpool <- Forest_europe %>%
  filter(country %in% final_results_cpool$country)


Cpool_country_average <- final_results_cpool %>%
  select(country, Source, Litter_million_tonnes, Soil_million_tonnes) %>%
  rbind(Forest_europe_cpool)
}
if (param == "AGB"){

  final_results_AGB <- bind_rows(results_AGB)
  
AGB <- AGB %>%
  filter(country %in% final_results_AGB$country)
AGB_country_average <- final_results_AGB %>%
  select(country, Source, AGB_million_tonnes) %>%
  rbind(AGB)
AGB_country_average <- AGB_country_average %>%
  filter(!is.na(country))}

cols_bar <- c("black",  brewer.pal(n = 4, name = "Paired"))

if (param == "cpool"){
  gg_bar_Litter <- ggplot(Cpool_country_average , 
                         aes(country, Litter_million_tonnes, fill = Source )) +
    geom_bar(stat = "identity", position = "dodge") +
    # scale_fill_discrete(name = "") +
    scale_fill_manual(values=cols_bar, name = "") +
    ggtitle("Total Litter Cpool") + #,  subtitle = "Forest Europe 2015 (mean 2013-2017) and simulated (mean 2013-2017)") +
    ylab("M Tonnes") + xlab("Country") +
    theme_bw()  +
    theme(legend.position = "right",
          text = element_text(size = 16),                # Increase all text
          axis.title = element_text(size = 18),          # Axis titles
          axis.text = element_text(size = 14),           # Axis text (x and y)
          axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text
          plot.title = element_text(size = 20, face = "bold"),  # Plot title
          legend.text = element_text(size = 14))
  
  gg_bar_Soil <- ggplot(Cpool_country_average , 
                         aes(country, Soil_million_tonnes, fill = Source)) +
    geom_bar(stat = "identity", position = "dodge") +
    # scale_fill_discrete(name = "") +
    scale_fill_manual(values=cols_bar, name = "") +
    ggtitle("Total Soil Cpool") + #, subtitle = "Forest Europe 2015 (mean 2013-2017) and simulated (mean 2013-2017)") +
    ylab("M Tonnes") + xlab("Country") +
    theme_bw()  +
    theme(legend.position = "right",
          text = element_text(size = 16),                # Increase all text
          axis.title = element_text(size = 18),          # Axis titles
          axis.text = element_text(size = 14),           # Axis text (x and y)
          axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text
          plot.title = element_text(size = 20, face = "bold"),  # Plot title
          legend.text = element_text(size = 14))
  
  
  
  plot(gg_bar_Litter +
         gg_bar_Soil + 
         plot_layout(ncol = 1, guides = "collect") &
         theme(axis.title.x = element_blank(),
               text = element_text(size = 16)))
  }
  if (param == "AGB"){
    
    gg_bar_AGB <- ggplot(AGB_country_average , 
                         aes(country, AGB_million_tonnes, fill = Source)) +
      geom_bar(stat = "identity", position = "dodge") +
      # scale_fill_discrete(name = "") +
      scale_fill_manual(values=cols_bar, name = "") +
      ggtitle("Total AGB") + #, subtitle = "Forest Europe 2015 (mean 2013-2017) and simulated (mean 2013-2017)") +
      ylab("AGB (Mt)") + xlab("Country") +
      theme_bw()  +
      theme(legend.position = "right",
            text = element_text(size = 16),                # Increase all text
            axis.title = element_text(size = 18),          # Axis titles
            axis.text = element_text(size = 14),           # Axis text (x and y)
            axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text
            plot.title = element_text(size = 20, face = "bold"),  # Plot title
            legend.text = element_text(size = 14))
    
  plot(gg_bar_AGB+
         plot_layout(ncol = 1, guides = "collect") &
         theme(axis.title.x = element_blank(),
               text = element_text(size = 16)))
  }

if (param == "cpool"){
  Harv_stats_country_average <- Harv_stats_country_average %>%
    select(harvest_type, Forest_area_1000ha, Vol_Mm3, Vol_m3perha)
  
  Harv_stats_long <- Harv_stats_country_average %>%
    pivot_longer(
      cols = c(Forest_area_1000ha, Vol_Mm3, Vol_m3perha),
      names_to = "Parameter", 
      values_to = "Value"
    )
  Harv_stats_wide <- Harv_stats_long %>%
    pivot_wider(
      names_from = country,
      values_from = Value
    ) %>%
    arrange(Parameter)}

if (param == "AGB"){
  
  Harv_stats_long <- AGB_country_average %>%
    pivot_longer(
      cols = c(AGB_million_tonnes),
      names_to = "Parameter", 
      values_to = "Value"
    )
  Harv_stats_wide <- Harv_stats_long %>%
    pivot_wider(
      names_from = country,
      values_from = Value
    ) %>%
    arrange(Parameter)
  }


return(Harv_stats_wide)
}
