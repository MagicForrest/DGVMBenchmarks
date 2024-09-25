#' Regrowth benchmark. Produces a plot of regrowth using binned growth after disturbance by biome. 
#' Function reads internal gridlist with lon, lat, biome. 
#' Also an internal regrowth dataset containing Lat, Lon, Year, Biome, bin. 
#'
#' @param benchmark Current benchmark used to select column to base calculations on.
#' @param all_sim_full List containing your simulations
#' @param opt.gridlist Full "path" to gridlist if you provide your own for special cases. Default = NULL.
#' @param opt.dataset Full "path" to dataset if you provide your own for special cases. Default = NULL.
#'
#' @return
#' @export
#'
#' @examples Regrowth(this_benchmark, all_sim_full)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se) Anne-marie Eckes-Shepard (annemarie.eckes-shephard@@nateko.lu.se)
Regrowth <- function(benchmark = this_benchmark, all_sim_full, opt.gridlist = NULL, opt.dataset = NULL){

# read the regrowth data
if (is.null(opt.gridlist)){
# first the gridlist
input_dir <- file.path(system.file("extdata", "Regrowth_Europe", package = "DGVMBenchmarks"))
gridlist_file <- file.path(input_dir, "gridlist_update19092024.txt")
gridlist <- read.table(gridlist_file)
names(gridlist) <- c("Lon","Lat","distyear","Biome")
}else{gridlist_file <- file.path(opt.gridlist)
gridlist <- read.table(gridlist_file,header = F)
names(gridlist) <- c("Lon","Lat","distyear","Biome")}

if (is.null(opt.dataset)){
# now the data
input_dir <- file.path(system.file("extdata", "Regrowth_Europe", package = "DGVMBenchmarks"))
regrowth_file <- file.path(input_dir, "benchmark_regrowth_19092024.csv")
regrowth <- read.csv(file =regrowth_file)
regrowth_full_dt <- data.table(regrowth)[, list(bin, Year, AGcwood_kgCm2_med, AGcwood_kgCm2_10, AGcwood_kgCm2_90, Biome)]
regrowth_full_dt[, bin_centres := as.factor(Year)]
regrowth_full_dt$Source <- "Data"
}else{regrowth_file <- file.path(opt.dataset)
regrowth <- read.csv(file =regrowth_file)
regrowth_full_dt <- data.table(regrowth)[, list(bin, Year, AGcwood_kgCm2_med, AGcwood_kgCm2_10, AGcwood_kgCm2_90, Biome)]
regrowth_full_dt[, bin_centres := as.factor(Year)]
regrowth_full_dt$Source <- "Data"}
# read the model output
# loop through model runs to be processed
for(this_simulation_regrowth in all_sim_full) {
    
    #merge with gridcell to find disturbance year, needed to reconstruct stand age
    model_joined <- inner_join(this_simulation_regrowth@data, gridlist)
    
    #introduce some new variables, Age, age-bins and summary stats:
    model_joined <- model_joined %>% select(Lat,Lon,Year,benchmark@guess_var,distyear,Biome)
    model_regr <- model_joined %>% group_by(Lat,Lon) %>% filter(Year >= distyear)
    age <- model_regr %>% group_by(Lat,Lon) %>% mutate(Age = Year - distyear)
    age_bins <- age %>% group_by(Lat,Lon) %>% mutate(bin = cut(Age, breaks=seq(0,300,20),include.lowest = TRUE,ordered_result = TRUE))
    #remove entries where the number of sites which achieve this stand age is lower than 10.
    counts_filtered <- age_bins %>% group_by(Biome,Age) %>% mutate(count= n()) %>% filter(count > 10) 
    
    #summary stats on bins:
    regrowth_mod_full <- age_bins %>% group_by(bin, Biome) %>% summarise(AGcwood_kgCm2_med = median(Total),
                                                                                    AGcwood_kgCm2_10 = quantile(Total, c(.10) ),
                                                                                    AGcwood_kgCm2_90 = quantile(Total, c(.90) ),
                                                                                    bin_centres =  floor(mean(Age))) 
    
    
    regrowth_mod_full$Source <- this_simulation_regrowth@source@name
    regrowth_full_dt <- rbindlist(list(regrowth_full_dt, regrowth_mod_full), fill = TRUE)
    
 
  
} # for each simulation

# set bin centres
regrowth_full_dt[, bin_centres := as.numeric(as.character(bin_centres))]
# For Temperate Biome with Source = "Data"
regrowth_full_dt <- regrowth_full_dt[!(
  Source == paste(this_simulation_regrowth@source@name) & 
    ( 
      (Biome == "Temperate" & 
         bin_centres > regrowth_full_dt[Biome == "Temperate" & Source == "Data", max(bin_centres)]) |
        (Biome == "Boreal" & 
           bin_centres > regrowth_full_dt[Biome == "Boreal" & Source == "Data", max(bin_centres)])))]
# and finally plot
regrowth_plot <- ggplot(regrowth_full_dt, aes(x=bin_centres)) + geom_point(aes(y = AGcwood_kgCm2_med,
                                                                               col = Source)) +
  geom_errorbar(aes(ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90, col = Source)) +
  facet_wrap(~Biome, ncol = 2, scales = "fixed" ) +
  labs(x= "Years after disturbance", y = expression(AG~wood~kgC~m^{"-2"})) 

plot(regrowth_plot)
}
