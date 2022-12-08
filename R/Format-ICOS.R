#!/usr/bin/Rscript

#######################################################################################################################
############################ FUNCTIONS TO HANDLE ICOS FILES ###########################################################
#######################################################################################################################

#' Get a Field for LPJ-GUESS
#' 
#' An internal function that reads data from an LPJ-GUESS run.  
#' It actually calls one of three other functions depending on the type of quantity specified.   
#' 
#' @param source A \code{\linkS4class{Source}} containing the meta-data about the LPJ-GUESS run
#' @param quant A string the define what output file from the LPJ-GUESS run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param layers A character string (or a vector of character strings) specifying which layer columns are to be read.  NULL (default) means read all.
#' @param target.STAInfo The spatial-temporal target domain
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is just taken to be 
#' "<quant@id>.out" (also "<quant@id>.out.gz")
#' @param verbose A logical, set to true to give progress/debug information
#' @return A list containing firstly the data.table containing the data, and secondly the STA.info 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' 
#' 
#' 
# ****************************** CREATE ICOS FORMAT ************************



# extracting the quantities that are present in the observation dataset
availableQuantities_obs <- function() {
  
  return(ICOS.quantities)
  
}

# gridcells <-
#   data.table(
#     lon = lapply(sites_input, function(x) {
#       slot(x, "longitude")
#     }),
#     lat = lapply(sites_input, function(x) {
#       slot(x, "latitude")
#     })
#   )
# 
# gridcells[] <- lapply(gridcells, as.numeric)

getField_ICOS <- function(source,
                          quant,
                          layers,
                          sta.info,
                          file.name,
                          verbose = FALSE,
                          ...) {
  
  
  # read each individual ICOS text files, and compile everything into one big table
  
  
  # determine which sites are available
  
  
  # loop over each side 
  for (s in 1:length(sites_input)) {
    
    # read the table
    
    # select the columns that we want
    
    # preselect years 
    
    # look up lon and lat
    
    # add to list data.tables
    
    # sets some commonly used input
    variables_fluxes <-
      trimws(strsplit(sites_input[[s]]@variables@variables_fluxes, ",")[[1]])
    
    # read the data from .csv file
    obs_data <-
      read.csv(
        file = paste0(
          sites_input[[s]]@paths@path_ICOS,
          "/",
          list.files(
            path = sites_input[[s]]@paths@path_ICOS,
            pattern = names(sites_input[s])
          ) %>% str_subset("DD") %>%
            str_subset("VARINFO", negate = T)
        ),
        na = c("-9999", "NA")
      )
    
    # declare a new data table to which the selected obs data will be added
    obs_tbl <- data.table(
      Site = names(sites_input[s]),
      Year = str_sub(obs_data$TIMESTAMP, 1, 4),
      Day = str_sub(obs_data$TIMESTAMP, 7, 8),
      ymd = obs_data$TIMESTAMP,
      Lon = sites_input[[s]]@longitude,
      Lat = sites_input[[s]]@latitude
    )
    
    # selects obs data corresponding to the flux variable
    # adds day and night to get daily
    # divides by a 1000 to convert gC/m^2 to kgC/m^2
    for (v in variables_fluxes) {
      obs_select <-
        select(obs_data, contains(sites_input[[s]]@variables@UT_method)) %>%
        select(contains(v)) %>%
        select(contains(sites_input[[s]]@variables@suffix)) %>%
        select(contains(c("_DT", "_NT")) |
                 ends_with(c("DAY", "NIGHT"))) %>%
        rowSums() / 1000
      
      obs_tbl <- cbind(obs_tbl, obs_select)
      setnames(obs_tbl, "obs_select", v)
    }
    
    # selects data based on first/last year
    # throws an error if no data is available for this time period
    if (sites_input[[s]]@first_year != "") {
      obs_tbl <-
        obs_tbl[!(
          as.numeric(obs_tbl$Year) < sites_input[[s]]@first_year |
            as.numeric(obs_tbl$Year) > sites_input[[s]]@last_year
        ),]
      if (nrow(obs_tbl) == 0) {
        stop("No data for the selected data seems to be available. Change the input file!")
      }
    }
    
    # convert day to day of year (doy)
    obs_tbl$Day <- yday(ymd(obs_tbl$ymd))
  
  }
  
  # combine list of data.tables
  
  
  if (quant@id %in% variables_fluxes) {
    ICOS_cfluxes %>% select(Year, Day, Lon, Lat, quant@id) -> quant_data
  }
  
  field.id <-
    makeFieldID(
      source = source,
      quant.string = quant@id,
      sta.info = sta.info
    )
  
  return.field <- new(
    "Field",
    id = field.id,
    quant = quant,
    data = quant_data,
    first.year =  min(as.numeric(quant_data$Year)),
    last.year = max(as.numeric(quant_data$Year)),
    year.aggregate.method = "none",
    spatial.extent = gridcells,
    spatial.extent.id = "Gridcells",
    spatial.aggregate.method = "none",
    subannual.resolution = "Day",
    subannual.aggregate.method = "none",
    subannual.original = "Day",
    source = source
  )
  
  return(return.field)
  
}


#####################################################################
########### LPJ-GUESS(-SPITFIRE) QUANTITIES ########################
#####################################################################


#' @format The \code{\linkS4class{Quantity}} class is an S4 class with the slots defined below
#' @rdname Quantity-class
#' @keywords datasets
#' 
#' 
ICOS.quantities <- list(
  new("Quantity",
      id = "GPP",
      name = "GPP",
      units = "gC/m^2",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("ICOS"),
      standard_name = "gross_primary_productivity"),
  new("Quantity",
      id = "NEE",
      name = "NEE",
      units = "gC/m^2",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("ICOS"),
      standard_name = "net_ecosystem_exchange"),
  new("Quantity",
      id = "Reco",
      name = "Reco",
      units = "gC/m^2",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("ICOS"),
      standard_name = "ecosystem_respiration")
)


ICOS <- new(
  "Format",
  id = "ICOS",
  predefined.layers = list(),
  quantities = ICOS.quantities,
  availableQuantities =
    availableQuantities_obs,
  getField = getField_ICOS
)