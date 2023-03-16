
#' @export
modelSummaryTable <- function(simulations,
                              var,
                              periods,
                              new_sim,
                              old_sim,
                              area_unit = "m^2",
                              unit_multiplier = 1,
                              sigfigs = 4,
                              version_label = character(0)){
  
  
  # read the files with the maximum needed data period
  first_year_needed <- min(unlist(periods))
  last_year_needed <- max(unlist (periods))
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
                                                                 quick.read = quick_read,
                                                                 quick.read.file = paste(var, version_label, sep = "_"))
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

    # if an old and new simualtion are specified calculate the absolute and precentage differences
    if(!missing(old_sim) & !missing(new_sim) & !is.null(new_sim) & !is.null(old_sim)){
    
      # absolute difference
      difference_string <- paste0(source_names[[new_sim]], " - ", source_names[[old_sim]])
      diff_row <- c(c("Source" = difference_string), signif(this_period_df[this_period_df$Source == new_sim, 2:ncol(this_period_df) ] - this_period_df[this_period_df$Source == old_sim, 2:ncol(this_period_df) ], sigfigs))
      this_period_df <- rbind(this_period_df, diff_row)
      
      # percentage difference
      perc_diff_string <-  paste0(source_names[[params$new_name]], " - ", source_names[[params$old_name]], " (% diff)")
      perc_diff_row <- c(c("Source" = perc_diff_string), signif(100 * this_period_df[this_period_df$Source == difference_string, 2:ncol(this_period_df) ]/ this_period_df[this_period_df$Source == old_sim, 2:ncol(this_period_df) ], sigfigs ))
      this_period_df <- rbind(this_period_df, perc_diff_row)
      
      # add Period column and bind
      this_period_df <- cbind("Period" = c(paste0(this_period[1],"-", this_period[2]),  rep(NA, nrow(this_period_df)-1)),this_period_df )
      
    }
    
    this_complete_df <- rbind(this_complete_df, this_period_df)
    
  }
  
  return(this_complete_df)
  
  
}