
#' @export
benchmark_GCP <- function(simulation_sources,
                          this_benchmark,
                          settings,
                          tables_list,
                          summary_col_names){
  
  ### Read the data and calculate the annual mean over the whole period
  GCP_full_Field <- read_GCP(skip_rows = this_benchmark@custom$skip_rows,
                             nyears_GCP = this_benchmark@custom$nyears_GCP,
                             sheet = this_benchmark@custom$sheet)
  this_benchmark@datasets <- list(GCP_full_Field@source) # needed for building the metrics table
  
  
  # Get first year from the data
  GCP_first_year <- GCP_full_Field@first.year
  # this_benchmark@last.year <- GCP_full_Field@last.year
  
  # make a list of all Fields to be compared  (this will also include whatever model runs are available)
  all_NBP_Fields_list <- list()
  all_NBP_Fields_restricted_list <- list()
  all_NBP_global_sums <- list()
  all_NBP_Fields_list[[GCP_full_Field@source@name]] <- GCP_full_Field
  
  # subset to the target benchmark period and the vector of labels to eventual put on the plot
  all_NBP_Fields_restricted_list[[GCP_full_Field@source@name]] <- selectYears(GCP_full_Field, first = this_benchmark@first.year, last = this_benchmark@last.year)
  GCP_Field_subset_ymean  <- suppressWarnings(aggregateYears(all_NBP_Fields_restricted_list[[GCP_full_Field@source@name]], "mean"))
  all_NBP_labels_vector <- c(paste0("Mean for ", this_benchmark@first.year, "-",this_benchmark@last.year, ":"),
                             paste0("    GCB: ", signif(GCP_Field_subset_ymean@data[["NBP"]], 3), " PgC/year"))
  
  
  # make the line for the global summary table
  summary_NBP_line <- makeSummaryLine(this_benchmark, summary_col_names)
  
  
  
  ### Read the LPJ-GUESS data
  
  # loop through model runs to be processed and store the full data 
  all_sim_full_NBP <- list()
  for(this_sim_Source in simulation_sources) {
    
    # check if file is present (if not don't include this run)
    this_benchmark_run_dir <- file.path(this_sim_Source@dir, this_benchmark@simulation)
    if(file.exists(file.path(this_benchmark_run_dir, "cflux.out")) || file.exists(file.path(this_benchmark_run_dir, "cflux.out.gz"))) {
      
      # make local sources pointing to the simulation subrun directory
      this_subrun_Source <- this_sim_Source
      this_subrun_Source@dir <- this_benchmark_run_dir
      
      # read the data and process it into global sums
      this_simulation_NBP <- getField(source = this_subrun_Source,
                                      this_benchmark@guess_var,
                                      first.year = GCP_first_year,
                                      verbose = settings$verbose_read,
                                      quick.read = settings$quick_read,
                                      quick.read.file = paste("cflux", version_label, sep = "_") 
      )
      
      if("NEE" %in% names(this_simulation_NBP)) renameLayers(this_simulation_NBP, "NEE", "NBP")
      
      # save for possibly using later
      all_sim_full_NBP[[this_sim_Source@name]] <- this_simulation_NBP
      
      # aggregate with weighted sum, adjust units and save for later
      this_simulation_NBP_agg <- aggregateSpatial(this_simulation_NBP, method = "w.sum", lon_centres = hd_lons, lat_centres = hd_lats)
      this_simulation_NBP_agg <- layerOp(x = this_simulation_NBP_agg, operator = "mulc", layers = layers(this_simulation_NBP_agg), new.layer = layers(this_simulation_NBP_agg), constant = -KG_TO_PG)
      all_NBP_Fields_list[[this_sim_Source@name]] <- this_simulation_NBP_agg
      
      # also subset to the target period and record the mean over this period
      all_NBP_Fields_restricted_list[[this_sim_Source@name]] <- selectYears(this_simulation_NBP_agg, first = this_benchmark@first.year, last = this_benchmark@last.year)
      this_sim_annual_mean <- suppressWarnings(aggregateYears(all_NBP_Fields_restricted_list[[this_sim_Source@name]], "mean"))
      all_NBP_labels_vector <- append(all_NBP_labels_vector, c(paste0("    ", this_sim_Source@name, ": ", signif(this_sim_annual_mean@data[["NBP"]], 3), " PgC/year")))
      
    } 
    
  }
  
  #### MAKE NBP LINE PLOT ####
  NBP_plot <- plotTemporal(all_NBP_Fields_list, 
                           col.by = "Source", 
                           layers = "NBP", 
                           title = "Global NBP", 
                           subtitle = NULL, 
                           y.label = "Global NBP (PgC/year)",
                           text.multiplier = 2.5, 
                           sizes = 1)
  
  # # make a simple data.frame to put numbers on the plot, and then add it to the plot
  global.numbers.df <- data.frame(x= rep(as.Date(paste(GCP_first_year), "%Y")),
                                  y = seq(from =7, length.out = length(all_NBP_Fields_list)+1, by = -0.5),
                                  label = all_NBP_labels_vector)
  
  NBP_plot <-  NBP_plot + geom_text(data = global.numbers.df,  mapping = aes(x = x, y = y, label = label), size = 8, hjust = 0, col = "black")
  print(NBP_plot)
  
  # calculate R^2 on this data
  # dirty fix until all version catch up with renaming of NEE to NBP
  this_benchmark@guess_layers <- "NBP"
  all_NBP_temporal_comparisons <- fullTemporalComparison(benchmark = this_benchmark, 
                                                         all_ts = all_NBP_Fields_restricted_list, 
                                                         new_model = settings$new_name,
                                                         old_model = settings$old_name) 
  
  tables_list[["metrics"]] <- rbind(tables_list[["metrics"]], 
                                    makeMetricTable(benchmark = this_benchmark, 
                                                    all_comparisons_list = all_NBP_temporal_comparisons, 
                                                    simulation_sources = simulation_sources))
  
  ## tables_list[["metrics"]] <-  metric_table
  
  
  
  #### CALCULATE SUMMARY TABLE LINE ###
  # with mean annual NBP over the largest common period 
  
  #update the earliest common year
  common_sta_info <- commonSTAInfo(all_NBP_Fields_list)
  #this_benchmark@first.year <- common_sta_info@first.year
  #this_benchmark@last.year <- common_sta_info@last.year
  summary_NBP_line$Period <- paste(this_benchmark@first.year, this_benchmark@last.year, sep = "-")
  
  for(this_NBP_agg in all_NBP_Fields_list) {
    
    # calculate yearly means of whole period
    # supressWarnings is just to stops warnings that there is no spatial or temporal data in the resulting Field
    # (all averaged away to give a single number)
    this_NBP_ymean <- suppressWarnings(aggregateYears(selectYears(this_NBP_agg, first = this_benchmark@first.year, last = this_benchmark@last.year), "mean"))
    # make a text label for putting the yearly mean on th plot and aa it to the label vector of labels
    all_NBP_labels_vector <- append(all_NBP_labels_vector, paste0(this_NBP_ymean@source@name, 
                                                                  ": ", 
                                                                  signif(this_NBP_ymean@data[["NBP"]],3), 
                                                                  " PgC/year"))
    
    # save this to the summary table, but it *may* be over-written later is spatial subsetting was selected
    if(this_NBP_agg@source@name == "GCP NBP") summary_NBP_line[["Data"]] <- signif(this_NBP_ymean@data[["NBP"]], 3) 
    else  summary_NBP_line[[this_NBP_agg@source@name]] <- signif(this_NBP_ymean@data[["NBP"]], 3) 
    
  }
  
  
  # Calculate area sum based on subset, if the spatial_extent is not NULL
  if(!is.null(settings$spatial_extent)) {
    
    for(this_sim_Source in settings$all_simulation_Sources_list) {
      
      # subset and aggregate the already read data
      # if the provided spatial yields a valid extent, use the crop function
      possible.error <- try (extent(settings$spatial_extent), silent=TRUE )
      # note that data.tables *do* return a valid extent, but we don't want to crop with that here (hence the second condition)
      if (!inherits(possible.error, "try-error") && !is.data.table(settings$spatial.extent)) {
        this_simulation_NBP_subset <- crop(x =  all_sim_full_NBP[[this_sim_Source@name]], y = settings$spatial_extent, spatial.extent.id = settings$spatial_extent_id)  
      }
      # else check if some gridcells to be selected with getGridcells
      else if(is.data.frame(settings$spatial_extent) || is.data.table(settings$spatial_extent) || is.numeric(settings$spatial_extent) || class(settings$spatial_extent)[1] == "SpatialPolygonsDataFrame"){
        this_simulation_NBP_subset <- selectGridcells(x = all_sim_full_NBP[[this_sim_Source@name]], gridcells = settings$spatial_extent, spatial.extent.id = settings$spatial_extent_id)
      }
      
      # aggregate with weighted sum and adjust units
      this_simulation_NBP_agg <- aggregateSpatial(this_simulation_NBP_subset, method = "w.sum", lon_centres = hd_lons, lat_centres = hd_lats)
      this_simulation_NBP_agg <- layerOp(x = this_simulation_NBP_agg, operator = "mulc", layers = layers(this_simulation_NBP_agg), new.layer = layers(this_simulation_NBP_agg), constant = -KG_TO_PG)
      summary_NBP_line[[this_sim_Source@name]] <- signif(suppressWarnings(aggregateYears(this_simulation_NBP_agg,"mean"))@data[["NBP"]], 3)
      
    } # for each simulation
    
  } # if spatial extent is not NULL
  
  
  # save the summary line to the table
  this_total_table <- rbind(tables_list[["totals"]], summary_NBP_line)
  names(this_total_table) <- summary_col_names
  tables_list[["totals"]] <-  this_total_table
  
  return(tables_list)
  
  
}

