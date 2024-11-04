
#' Harvest table
#'
#' @param param What metrics to produce, either "Total Harvest" for harvest volume metrics or "NAI" for net annual increment metrics.
#' @param benchmark The current benchmark.
#' @param all_sim_full List holding simulations.
#'
#' @return
#' @export
#'
#' @examples
#' @author Karl Piltz (karl.piltz@@nateko.lu.se), Susanne Suvanto (susanne.suvanto@@luke.fi)
HarvestTable <- function(param, benchmark = this_benchmark, all_sim_full = all_sim_full){

# Read and process --------------------------------------------------------

### LPJG outputs ----
### preparations
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




### Original input files ----
LC2010 <- read.table(file.path(system.file("extdata/Harvest/LC_europe_nat_for_1801_2010_Pucher_noNatural_twin_2010-2010.tab", package = "DGVMBenchmarks")), header = T)


### Expansion factors ----

cmass_to_m3 <- read.table(file.path(system.file("extdata/Harvest/cmass_to_m3_expansionFactors.txt", package = "DGVMBenchmarks")),header = 1)
cmass_to_m3$Country <- ifelse(cmass_to_m3$Country == "Czech", "Czechia", cmass_to_m3$Country)

#cmass_to_agb <- read.csv2("./data/from_Mats/AGFractions_ForestEurope_checked.csv") # file from Mats, not clear what the 2 last cols are!


### Grid (country + cell area) + process ----
grid_table <- read.csv(file.path(system.file("extdata/Harvest/gridcell_country_cluster.csv", package = "DGVMBenchmarks")), row.names = 1)
grid_countries <- create_grid(grid_table, coords = c("lon05", "lat05"), cellsize = 0.5)

grid_countries <- grid_countries %>%
  st_transform(crs = "EPSG:3035")

grid_countries$area_m2 <- as.numeric(st_area(grid_countries))


### Forest Europe + Avitable processed file ----
### (file processing in "00_process_FEdata.R")

harvests_SoEF <- read_csv(file.path(system.file("extdata/Harvest/Harvest_statistics_SoEF2020_Avitabile.csv", package = "DGVMBenchmarks")))


results <- list()

for (this_sim in all_sim_full){
  
if (param == "Total Harvest"){  
# Total harvest per country -----------------------------------------------
# (1) multiplying the harvested cmass with the expansion factors, 
# (2) multiplying with the area of the grid cell, and 
# (3) summing up the grid cells in a country
  if(this_sim@source@id == "EFISCEN"){
    wood_harv_processed_2013_2017 <- this_sim@data %>%
      left_join(grid_countries, by = c(Lat = "lat05", Lon = "lon05")) %>%
      left_join(cmass_to_m3 %>% select(Country, Mean_volume_expansion_factor),
                by = c(country = "Country")) %>%
      left_join(LC2010) %>%
      mutate(area_m2_FOREST = area_m2 * FOREST,
             wood_harv_vol_perHa_FOREST = .[[benchmark@guess_var]]/1000 * Mean_volume_expansion_factor * 1e4,
             wood_harv_vol_tot = .[[benchmark@guess_var]]/1000 * Mean_volume_expansion_factor * area_m2_FOREST,
             harvest_type = this_sim@source@id)
    
    Harv_stats_country_2013_2017 <- wood_harv_processed_2013_2017 %>%
      group_by(country, harvest_type) %>%
      summarise(wood_harv_vol_tot = sum(wood_harv_vol_tot, na.rm=TRUE)/1e3,
                sim_forest_area_1000ha = sum(area_m2_FOREST, na.rm = TRUE)* 1e-07,
                wood_harv_vol_ha_FOREST = sum(wood_harv_vol_tot,na.rm = TRUE)/sim_forest_area_1000ha) %>%
      ungroup() %>%
      mutate(Vol_m3perha = wood_harv_vol_ha_FOREST,
             Vol_1000m3 = wood_harv_vol_tot,
             Forest_area_1000ha = sim_forest_area_1000ha) %>%
      filter(!country %in% c("Norway"))
    
    Harv_stats_country_average <- Harv_stats_country_2013_2017 %>%
      group_by(country, harvest_type) %>%
      summarise(Vol_1000m3 = mean(Vol_1000m3),
                Vol_m3perha = mean(Vol_m3perha),
                Forest_area_1000ha = unique(Forest_area_1000ha))
    
    results[[this_sim@source@id]] <- Harv_stats_country_average
  }
  else{
### for 2013-2017 (range represented by 2015 in SoEF) -----

wood_harv_processed_2013_2017 <- this_sim@data %>%
  filter(Year %in% 2013:2017) %>%
  left_join(grid_countries, by = c(Lat = "lat05", Lon = "lon05")) %>%
  left_join(cmass_to_m3 %>% select(Country, Mean_volume_expansion_factor),
            by = c(country = "Country")) %>%
  left_join(LC2010) %>%
  mutate(area_m2_FOREST = area_m2 * FOREST,
         wood_harv_vol_perHa_FOREST = .[[benchmark@guess_var]]/1000 * Mean_volume_expansion_factor * 1e4,
         wood_harv_vol_tot = .[[benchmark@guess_var]]/1000 * Mean_volume_expansion_factor * area_m2_FOREST,
         harvest_type = this_sim@source@id)

Harv_stats_country_2013_2017 <- wood_harv_processed_2013_2017 %>%
  group_by(country, harvest_type, Year) %>%
  summarise(wood_harv_vol_tot = sum(wood_harv_vol_tot, na.rm=TRUE)/1e3,
            sim_forest_area_1000ha = sum(area_m2_FOREST)* 1e-07,
            wood_harv_vol_ha_FOREST = sum(wood_harv_vol_tot)/sim_forest_area_1000ha) %>%
  ungroup() %>%
  mutate(Vol_m3perha = wood_harv_vol_ha_FOREST,
         Vol_1000m3 = wood_harv_vol_tot,
         Forest_area_1000ha = sim_forest_area_1000ha) %>%
  filter(!country %in% c("Norway"))

Harv_stats_country_average <- Harv_stats_country_2013_2017 %>%
  group_by(country, harvest_type) %>%
  summarise(Vol_1000m3 = mean(Vol_1000m3),
            Vol_m3perha = mean(Vol_m3perha),
            Forest_area_1000ha = unique(Forest_area_1000ha))

results[[this_sim@source@id]] <- Harv_stats_country_average
}

}

if (param == "NAI"){    
# %NAI harvested ----------------------------------------------------------
# NAI for full forest area from Avitable et al., total harvest amount per country from Forest Europe



if(this_sim@source@id == "EFISCEN"){
  forest_cflux_veg_processed <- this_sim@data %>%
    left_join(grid_countries, by = c(Lat = "lat05", Lon = "lon05")) %>% # join to get country info for cells
    left_join(LC2010 %>% select(!year)) %>%
    mutate(for_NAI_forestArea_ha = .[[benchmark@guess_var]], # new input directly in m³/ha
           NAI_tot_cell = for_NAI_forestArea_ha * FOREST * area_m2 / 1e4, # converting ha to m²
           harvest_type = this_sim@source@id) 
  
  NAI_country_2013_2017 <- forest_cflux_veg_processed %>%
    filter(!country %in% c("Norway")) %>%
    group_by(country, harvest_type) %>%
    summarise(NAI_perHA = NAI_perHA = sum(for_NAI_forestArea_ha * FOREST, na.rm = T) / sum(FOREST,na.rm = T), # / sum(FOREST,na.rm = T), # no scaling by 1e4, already in m³/ha
              NAI_tot = sum(for_NAI_forestArea_ha * FOREST * area_m2 / 1e4,na.rm = T), # NAI in m³ across forest areas
              NAI_tot_Mm3 = NAI_tot / 1e6,
              ForestArea_HA = sum(FOREST * area_m2) / 1e4, na.rm = T) %>%
    summarise(NAI_perHA = mean(NAI_perHA,na.rm = T), # average over years
              NAI_tot = mean(NAI_tot,na.rm = T),
              NAI_tot_Mm3 = mean(NAI_tot_Mm3,na.rm = T),
              ForestArea_HA = mean(ForestArea_HA,na.rm = T),
              harvest_type = this_sim@source@id)
  results[[this_sim@source@id]] <- NAI_country_2013_2017
  
}
else{
  forest_cflux_veg_processed <- this_sim@data %>%
    left_join(grid_countries, by = c(Lat = "lat05", Lon = "lon05")) %>% # join to get country info for cells
    left_join(cmass_to_m3 %>% select(Country, Mean_volume_expansion_factor), # join to get expansion factors
              by = c(country = "Country")) %>%
    left_join(LC2010 %>% select(!year)) %>%
    mutate(for_NAI_m3 = .[[benchmark@guess_var]]/1000 * Mean_volume_expansion_factor,
           for_NAI_forestArea = for_NAI_m3, # FOREST = share of forests from the total grid cell area
           for_NAI_forestArea_ha = for_NAI_forestArea * 1e4,# as 1 ha = 10 000 m2 
           NAI_tot_cell = for_NAI_forestArea * area_m2,
           harvest_type = this_sim@source@id) 
  
NAI_country_2013_2017 <- forest_cflux_veg_processed %>%
  filter(Year %in% 2013:2017) %>%
  filter(!country %in% c( "Norway")) %>%
  group_by(country, harvest_type, Year) %>%
  summarise(NAI_perHA =  sum(for_NAI_forestArea) / sum(FOREST)*1e4, # average over grid cells in country, weighting by forest area in cell
            NAI_tot = sum(NAI_tot_cell),
            NAI_tot_Mm3 = NAI_tot/1e6,
            ForestArea_HA = sum(FOREST * area_m2)/1e4) %>%
  summarise(NAI_perHA = mean(NAI_perHA), # average over years
            NAI_tot = mean(NAI_tot),
            NAI_tot_Mm3 = mean(NAI_tot_Mm3),
            ForestArea_HA = mean(ForestArea_HA)  )}

results[[this_sim@source@id]] <- NAI_country_2013_2017
}
}

final_results <- bind_rows(results)
## Combine with country statistics ----

# modify country-stats df to be combined with sim results (with rbind -> need to have same columns)
harvests_SoEF_2 <- harvests_SoEF %>%
  mutate(harvest_type = "ForestEurope",
         harvest_subset = "all") %>%
  mutate(country = Country,
         Vol_1000m3 = HarvVol_2015_1000m3,
         Vol_Mm3 = Vol_1000m3/1000,
         Vol_m3perha = HarvVol_2015_m3perha,
         NAI_perHA = NAI_m3_ha_yr,
         NAI_tot_Mm3 = NAI_m3_yr/1e6) %>%
  filter(country %in% final_results$country) %>%
  select(country, harvest_type, Forest_area_1000ha, Vol_1000m3, Vol_Mm3, Vol_m3perha, 
         NAI_tot_Mm3, NAI_perHA, Harv_percent_NAI)

if (param == "Total Harvest"){
Harv_stats_country_average <- final_results %>%
  mutate(Vol_Mm3 = Vol_1000m3/1000) %>%
  select(country, harvest_type, Forest_area_1000ha, Vol_1000m3, Vol_Mm3, Vol_m3perha) %>%
  rbind(harvests_SoEF_2 )

Harv_stats_country_average <- Harv_stats_country_average %>%
  left_join(harvests_SoEF_2 %>% 
              select(country, Forest_area_1000ha) %>%
              rename(Forest_area_1000ha_FE = Forest_area_1000ha)) %>%
  mutate(Vol_Mm3_FEarea = (Vol_m3perha  * Forest_area_1000ha_FE)/1000) 
Harv_stats_country_average <- Harv_stats_country_average %>%
  filter(!is.na(country))
}
if (param == "NAI"){
# Harv_stats_country_average <- Harv_stats_country_average %>%
#   left_join(NAI_country_2013_2017) %>%
#   mutate(Vol_Mm3 = Vol_1000m3/1000,
#          Harv_percent_NAI  = Vol_Mm3/NAI_tot_Mm3) %>%
#   select(country, harvest_type, Forest_area_1000ha, Vol_1000m3, Vol_Mm3, Vol_m3perha, 
#          NAI_perHA, NAI_tot_Mm3, Harv_percent_NAI) %>%
#   rbind(harvests_SoEF_2 )
# 
# Harv_stats_country_average <- Harv_stats_country_average %>%
#   left_join(harvests_SoEF_2 %>% 
#               select(country, Forest_area_1000ha) %>%
#               rename(Forest_area_1000ha_FE = Forest_area_1000ha)) %>%
#   mutate(Vol_Mm3_FEarea = (Vol_m3perha  * Forest_area_1000ha_FE)/1000) 

  Harv_stats_country_average <- final_results %>%
  select(country, harvest_type, NAI_perHA, NAI_tot_Mm3) %>%
  rbind(harvests_SoEF_2)
  Harv_stats_country_average <- Harv_stats_country_average %>%
    filter(!is.na(country))
}
## Bar plots -----

 #cols_bar <- c("black", "#56641a", "#e6a176", "#00678a", "#984464")
 #cols_bar <- c("black", "#c0affb", "#e6a176", "#984464", "#5eccab")
cols_bar <- c("black",  brewer.pal(n = 4, name = "Paired"))

if (param == "Total Harvest"){
gg_bar_total <- ggplot(Harv_stats_country_average , 
                       aes(country, Vol_Mm3, fill = harvest_type )) +
  geom_bar(stat = "identity", position = "dodge") +
  # scale_fill_discrete(name = "") +
  scale_fill_manual(values=cols_bar, name = "") +
  ggtitle("Total harvested volume") + #,  subtitle = "Forest Europe 2015 (mean 2013-2017) and simulated (mean 2013-2017)") +
  ylab("M m3") + xlab("Country") +
  theme_bw()  +
  theme(legend.position = "right",
        text = element_text(size = 16),                # Increase all text
        axis.title = element_text(size = 18),          # Axis titles
        axis.text = element_text(size = 14),           # Axis text (x and y)
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text
        plot.title = element_text(size = 20, face = "bold"),  # Plot title
        legend.text = element_text(size = 14))  

gg_bar_perHA <- ggplot(Harv_stats_country_average , 
                       aes(country, Vol_m3perha, fill = harvest_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  # scale_fill_discrete(name = "") +
  scale_fill_manual(values=cols_bar, name = "") +
  ggtitle("Harvests per ha") + #, subtitle = "Forest Europe 2015 (mean 2013-2017) and simulated (mean 2013-2017)") +
  ylab("m3 ha-1") + xlab("Country") +
  theme_bw()  +
  theme(legend.position = "right",
        text = element_text(size = 16),                # Increase all text
        axis.title = element_text(size = 18),          # Axis titles
        axis.text = element_text(size = 14),           # Axis text (x and y)
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text
        plot.title = element_text(size = 20, face = "bold"),  # Plot title
        legend.text = element_text(size = 14))  


# gg_bar_NAIharv <- ggplot(Harv_stats_country_average, 
#                          aes(country, Harv_percent_NAI, fill = harvest_type)) + 
#   geom_bar(stat="identity", position = "dodge") +
#   # scale_fill_discrete(name = "") +
#   scale_fill_manual(values=cols_bar, name = "") +
#   ggtitle("C. Share of NAI harvested") + #, subtitle = "Forest Europe 2015 (mean 2013-2017) and simulated (mean 2013-2017)") +
#   ylab("ratio") + xlab("Country") +
#   theme_bw()  +
#   theme(legend.position = "right")

gg_bar_area <- ggplot(Harv_stats_country_average, 
                      aes(country, Forest_area_1000ha/1000, fill = harvest_type)) + 
  geom_bar(stat="identity", position = "dodge") +
  # scale_fill_discrete(name = "") +
  scale_fill_manual(values=cols_bar, name = "") +
  ggtitle("Forest area" ) + #,  subtitle = "Forest Europe and simulated") +
  ylab("M ha") + xlab("Country") +
  theme_bw()  +
  theme(legend.position = "right",
        text = element_text(size = 16),                # Increase all text
        axis.title = element_text(size = 18),          # Axis titles
        axis.text = element_text(size = 14),           # Axis text (x and y)
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text
        plot.title = element_text(size = 20, face = "bold"),  # Plot title
        legend.text = element_text(size = 14))  

plot(gg_bar_total +
    gg_bar_perHA + 
  #   gg_bar_NAIharv +
     gg_bar_area +
  #gg_bar_NAItotal+
  #gg_bar_NAIperHa +
  plot_layout(ncol = 1, guides = "collect") &
  theme(axis.title.x = element_blank(),
        text = element_text(size = 16)))
}

if (param == "NAI"){
gg_bar_NAIperHa <- ggplot(Harv_stats_country_average, 
                          aes(country, NAI_perHA, fill = harvest_type)) + 
  geom_bar(stat="identity", position = "dodge") +
  # scale_fill_discrete(name = "") +
  scale_fill_manual(values=cols_bar, name = "") +
  ggtitle(" NAI per ha" ) + #,  subtitle = "Forest Europe and simulated") +
  ylab("m3 ha-1") + xlab("Country") +
  theme_bw()  +
  theme(legend.position = "right",
        text = element_text(size = 16),                # Increase all text
        axis.title = element_text(size = 18),          # Axis titles
        axis.text = element_text(size = 14),           # Axis text (x and y)
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text
        plot.title = element_text(size = 20, face = "bold"),  # Plot title
        legend.text = element_text(size = 14))       # Legend text

gg_bar_NAItotal <- ggplot(Harv_stats_country_average, 
                          aes(country, NAI_tot_Mm3, fill = harvest_type)) + 
  geom_bar(stat="identity", position = "dodge") +
  # scale_fill_discrete(name = "") +
  scale_fill_manual(values=cols_bar, name = "") +
  ggtitle("Total NAI" ) + #,  subtitle = "Forest Europe and simulated") +
  ylab("M m3") + xlab("Country") +
  theme_bw()  +
  theme(legend.position = "right",
        text = element_text(size = 16),                # Increase all text
        axis.title = element_text(size = 18),          # Axis titles
        axis.text = element_text(size = 14),           # Axis text (x and y)
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text
        plot.title = element_text(size = 20, face = "bold"),  # Plot title
        legend.text = element_text(size = 14))

# gg_bar_total +
#   gg_bar_perHA + 
#   gg_bar_NAIharv +
#   gg_bar_area +
  plot(gg_bar_NAItotal+
  gg_bar_NAIperHa +
  plot_layout(ncol = 1, guides = "collect") &
  theme(axis.title.x = element_blank()),
  text = element_text(size = 16))

}

if (param == "Total Harvest"){
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

if (param == "NAI"){
  Harv_stats_country_average <- Harv_stats_country_average %>%
    select(harvest_type, NAI_tot_Mm3, NAI_perHA)
  
  Harv_stats_long <- Harv_stats_country_average %>%
  pivot_longer(
    cols = c(NAI_perHA, NAI_tot_Mm3),
    names_to = "Parameter", 
    values_to = "Value"
  )
Harv_stats_wide <- Harv_stats_long %>%
  pivot_wider(
    names_from = country,
    values_from = Value
  ) %>%
  arrange(Parameter)}

return(Harv_stats_wide)

}

