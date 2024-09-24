#' ForestFraction. Build table of forest fraction per country. Can be used for one benchmark but can also be continually updated by using rbind() when calling function.
#'
#' @param benchmark The current benchmark object
#' @param all_Fields_list The list holding all your fields
#'
#' @return
#' @export
#' 
#' @examples ForestFraction(benchmark = this_benchmark, all_Fields_list = all_Fields_list)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
ForestFraction <- function(benchmark = this_benchmark, all_Fields_list = all_Fields_list){

# Initialize an empty data frame to store results
results_table <- data.frame(
  Quantity = character(),
  Dataset = character(),
  Simulation = character(),
  Country = character(),
  Value = numeric(),
  stringsAsFactors = FALSE
)



for (field in all_Fields_list){

forest_LC <- read.table(file.path(system.file("extdata/Forest_Fraction/LC_europe_nat_for_1801_2010_Pucher_noNatural_twin_2010-2010.tab", package = "DGVMBenchmarks")), header = T)
  
Country_grid <- read.table(file.path(system.file("extdata/Forest_Fraction/European_gridlist.txt", package = "DGVMBenchmarks")), header = T)
  
Forest_per_country <- merge(forest_LC, 
                              Country_grid, 
                                by = c("Lon", "Lat"))  
    
field_Dt <- field@data 

quantity_per_country <- merge(field_Dt, 
                                Forest_per_country,
                                  by = c("Lon", "Lat"))

field_var <- benchmark@guess_var

country_means <- quantity_per_country %>%
  group_by(Country) %>%
  summarize(
    weighted = sum(get(field_var) * FOREST, na.rm = TRUE) / sum(FOREST, na.rm = TRUE),
    
  )

if (field@source@id == "Obs"){
results_table <- rbind(results_table,
                        data.frame(
                          Quantity = paste(benchmark@id, benchmark@unit),
                            Dataset = field@source@name,
                              Simulation = "-",
                                Country = country_means$Country,
                                  Value = country_means$weighted))}

else{
  results_table <- rbind(results_table, 
                          data.frame(
                            Quantity = paste(benchmark@id, benchmark@unit),
                              Dataset = "-",
                                Simulation = field@source@name,
                                  Country = country_means$Country,
                                    Value = country_means$weighted))}

results_table_wide <- results_table %>%
  pivot_wider(names_from = c(Country),
                values_from = Value)}

return(results_table_wide)

}



