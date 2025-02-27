#'   Produce a model summary table
#' 
#'   The table produced here compares multiple model versions for all layers of a single variable for an arbitrary selection of time periods.
#'   Optionally it can be subset over a spatial extent id selected in the YAML file
#' 
#' @export
modelSummaryTable <- function(simulations,
                              settings,
                              var,
                              comparison_periods,
                              area_unit = "m^2",
                              unit_multiplier = 1,
                              sigfigs = 3,
                              version_label = character(0)){
  
  # sort comparison years argument and  determine the years needed 
  if(missing(comparison_periods))  periods <- settings$summary_periods
  else {
    if(!is.list(comparison_periods)) periods <- list(comparison_periods)
    else periods <-comparison_periods
  }
  first_year_needed <- min(unlist(periods))
  last_year_needed <- max(unlist (periods))

  
  # read the files with the maximum needed data period
  simulation_Fields <- list()
  for(this_sim_Source in simulations) {
    
    # check if file is present (if not don't include this run)
    this_benchmark_run_dir <- file.path(this_sim_Source@dir, "tellus")
    if(file.exists(file.path(this_benchmark_run_dir, paste0(var, ".out"))) || file.exists(file.path(this_benchmark_run_dir, paste0(var, ".out.gz")))) {
      
      # make local sources pointing to the crop_global directory
      this_tellus_Source <- this_sim_Source
      this_tellus_Source@dir <- this_benchmark_run_dir
      
      # read the full data and
      suppressWarnings(
        simulation_Fields[[this_tellus_Source@name]] <- getField(source = this_tellus_Source,
                                                                 var,
                                                                 first.year = first_year_needed,
                                                                 last.year = last_year_needed,
                                                                 spatial.extent = settings$spatial_extent,
                                                                 spatial.extent.id = settings$spatial_extent_id,
                                                                 quick.read.autodelete = settings$quick_read_autodelete,
                                                                 quick.read.file = switch(settings$quick_read+1,
                                                                                          NULL,
                                                                                          paste(var, settings$version_label, "ForSummaryTable", sep = "_")))
      )
    }
  }
  
  
  # loop over all periods and Fields to build the table
  this_complete_df <- data.frame()
  for(this_period in periods){
    
    # loop through model runs to be processed
    source_names <- list()
    this_period_df <- data.frame()
    for(this_sim in simulation_Fields) {
      
      this_sim_annual <- aggregateYears(selectYears(this_sim, first = this_period[1], last = this_period[2]), "mean")
      suppressWarnings(this_sim_final <- aggregateSpatial(this_sim_annual, method = "w.sum", unit = area_unit ))
      this_row <- c(c("Source" = this_sim@source@name), signif(this_sim_final@data * unit_multiplier, sigfigs))
      this_period_df <- rbind(this_period_df, this_row)
      source_names[[this_sim@source@name]] <- this_sim@source@name
      
    }
    
    # if a reference simulation has been specified calculate the absolute and precentage differences
    if(!is.null(settings$reference_simulation)){
      # check the the reference simulation is in the sims list
      if(settings$reference_simulation %in% source_names) {
        # for each simulation that is not the reference simulation
        for(new_sim in source_names) {
          if(new_sim != settings$reference_simulation) {
            
            # absolute difference
            difference_string <- paste0(source_names[[new_sim]], " - ", source_names[[settings$reference_simulation]])
            diff_row <- c(c("Source" = difference_string), signif(this_period_df[this_period_df$Source == new_sim, 2:ncol(this_period_df) ] - this_period_df[this_period_df$Source == settings$reference_simulation, 2:ncol(this_period_df) ], sigfigs))
            this_period_df <- rbind(this_period_df, diff_row)
            
            # percentage difference
            perc_diff_string <-  paste0(source_names[[new_sim]], " - ", source_names[[settings$reference_simulation]], " (% diff)")
            perc_diff_row <- c(c("Source" = perc_diff_string), signif(100 * this_period_df[this_period_df$Source == difference_string, 2:ncol(this_period_df) ]/ this_period_df[this_period_df$Source == settings$reference_simulation, 2:ncol(this_period_df) ], sigfigs ))
            this_period_df <- rbind(this_period_df, perc_diff_row)
            
            
          } # if not reference simulations
        } # for each simulationa
      } # if reference simulations present
    } # if reference simulation defined
    
    # add Period column and bind
    this_period_df <- cbind("Period" = c(paste0(this_period[1],"-", this_period[2]),  rep(NA, nrow(this_period_df)-1)),this_period_df )
    this_complete_df <- rbind(this_complete_df, this_period_df)
    
  }
  
  return(this_complete_df)
  
  
}