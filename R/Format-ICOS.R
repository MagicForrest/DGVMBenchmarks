#!/usr/bin/Rscript

#####################################################################################################
############################ FUNCTIONS TO HANDLE ICOS FILES #########################################
#####################################################################################################

#' Get a Field for ICOS
#' 
#' An internal function that reads data from ICOS observations.   
#' 
#' @param source A  \code{\linkS4class{Source}} containing the meta-data about the ICOS observations
#' @param quant A Quantity object to define what quantity from the ICOS observations to extract
#' @param layers Ignored for ICOS
#' @param target.STAInfo The spatial-temporal target domain
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is just taken to be 
#' "<quant@id>.out" (also "<quant@id>.out.gz")
#' @param verbose A logical, set to true to give progress/debug information
#' @param UT.threshold USTAR threshold, variable (VUT) or constant (CUT). VUT is default
#' @param partition.method Partition method, e.g. REF, USTAR50, MEAN. REF is default
#' @param first.year Optional, will exclude data before this year
#' @param last.year Optional, will exclude data beyond this year
#' @import stringr
#' @import dplyr
#' @import lubridate
#' @return A list containing firstly the data.table containing the data, and secondly the STA.info 
#' @author Margot Knapen \email{margot.knapen@@nateko.lu.se}
#' @keywords internal
#'

getField_ICOS <- function(source,
                          quant,
                          layers = NULL,
                          target.STAInfo,
                          file.name,
                          verbose,
                          UT.threshold = "VUT",
                          partition.method = "REF",
                          first.year,
                          last.year,
                          ...) {
  
  ### CHECK ARGUEMENTS
  if(!missing(first.year) & !missing(last.year) ) {
    if(first.year > last.year) stop("first.year cannot be greater than last.year!")
  }
  
  
  # variables that are currently supported
  variables.cfluxes = c("GPP", "NEE", "Reco")
  
  # lists all files with daily fluxes
  daily.files <- list.files(path = source@dir) %>%
    stringr::str_subset("DD") %>% stringr::str_subset("VARINFO", negate = T)
  
  # list all files with site information
  siteinfo.files <- list.files(path = source@dir) %>% stringr::str_subset("SITEINFO")
  
  
  # loops through all available files with daily fluxes
  for (i in 1:length(daily.files)) {
    # reading the daily data from the .csv file
    site.data <- read.csv(file = file.path(source@dir, daily.files[i]),
                          na = c("-9999", "NA"))
    
    # reading the site info from the .csv file
    siteinfo.data <- read.csv(file = file.path(source@dir, siteinfo.files[i]))
    
    # determining the site name abbreviation
    site <- unlist(stringr::str_extract_all(string = daily.files[i],
                                            pattern = "(?<=ICOSETC_).+(?=_FLUXNET)"))
    
    # determining the longitude and latitude
    lon <- siteinfo.data$DATAVALUE[siteinfo.data$VARIABLE == "LOCATION_LONG"]
    lat <- siteinfo.data$DATAVALUE[siteinfo.data$VARIABLE == "LOCATION_LAT"]
    
    # determining the full site name
    site.name <- siteinfo.data$DATAVALUE[siteinfo.data$VARIABLE == "SITE_NAME"]
    
    
    # declaring a new data table with metadata that will be used to append the daily fluxes to
    site.data.selected <- data.table(Site = site,
                                     Site_name = site.name,
                                     Year = stringr::str_sub(site.data$TIMESTAMP, 1, 4),
                                     Day = stringr::str_sub(site.data$TIMESTAMP, 7, 8),
                                     ymd = site.data$TIMESTAMP,
                                     Lon = lon,
                                     Lat = lat)
    
    
    # selecting the required columns (GPP, NEE, Reco) of the daily fluxes file
    # adds day and night to get daily values
    # divides by a 1000 to convert gC/m^2 to kgC/m^2
    for (v in variables.cfluxes) {
      to.cbind <- select(site.data, contains(UT.threshold)) %>%
        select(contains(v)) %>%
        select(contains(partition.method)) %>%
        select(contains(c("_DT", "_NT")) | ends_with(c("DAY", "NIGHT"))) %>%
        rowSums() / 1000
      
      site.data.selected <- cbind(site.data.selected, to.cbind)
      setnames(site.data.selected, "to.cbind", v)
    }
    
    # convert day to day of year (doy)
    site.data.selected$Day <- lubridate::yday(lubridate::ymd(site.data.selected$ymd))
    
    if (!missing(first.year)) {
      site.data.selected <- site.data.selected[!(site.data.selected$Year) < first.year,]
    } else {
      first.year <- min(as.numeric(site.data.selected$Year))
    }
    
    if (!missing(last.year)) {
      site.data.selected <- site.data.selected[!(site.data.selected$Year) > last.year,]
    } else {
      last.year <- max(as.numeric(site.data.selected$Year))
    }
    
    if (nrow(site.data.selected) == 0) {
      warning("No data for this time period available. Check first and/or last year")
      stop()
    }
    
    # combine the data tables with selected daily fluxes for all sites
    if (i == 1) {
      ICOS.cfluxes <- site.data.selected
    } else {
      ICOS.cfluxes <- rbind(ICOS.cfluxes, site.data.selected)
    }
  }
  
  
  # select the required columns for the current quantity
  if (quant@id %in% variables.cfluxes) {
    ICOS.cfluxes %>% select(Year, Day, Lon, Lat, quant@id) -> quant.data
  }
  
 
  # creating a data table with the lon/lat
  gridcells <- data.table(Lat = as.numeric(unique(ICOS.cfluxes$Lat)),
                          Lon = as.numeric(unique(ICOS.cfluxes$Lon)),
                          Site = unique(ICOS.cfluxes$Site),
                          Site_name = unique(ICOS.cfluxes$Site_name))
  field.id <-
    makeFieldID(
      source = source,
      quant.string = quant@id,
      sta.info = target.STAInfo
    )
  
  return.field <- new(
    "Field",
    id = field.id,
    quant = quant,
    data = quant.data,
    first.year = first.year,
    last.year = last.year,
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



#####################################################################################################
############################ ICOS AVAILABLE QUANTITIES ##############################################
#####################################################################################################

#' List ICOS quantities available
#'
#' Simply lists all carbon flux variables available in the ICOS data
#' 
#' @return A list of all ICOS quantities
#' @keywords internal
#'

availableQuantities_ICOS <- function() {
  
  return(ICOS.quantities)
  
}

#####################################################################################################
############################ ICOS QUANTITIES ########################################################
#####################################################################################################

#' @format The \code{\linkS4class{Quantity}} class is an S4 class with the slots defined below
#' @rdname Quantity-class
#' @keywords datasets
#' 

ICOS.quantities <- list(
  new("Quantity",
      id = "GPP",
      name = "GPP",
      units = "kgC/m^2",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("ICOS"),
      standard_name = "gross_primary_productivity"),
  new("Quantity",
      id = "NEE",
      name = "NEE",
      units = "kgC/m^2",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("ICOS"),
      standard_name = "net_ecosystem_exchange"),
  new("Quantity",
      id = "Reco",
      name = "Reco",
      units = "kgC/m^2",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("ICOS"),
      standard_name = "ecosystem_respiration")
)



#####################################################################################################
############################ ICOS FORMAT ############################################################
#####################################################################################################

#' @description \code{ICOS} - a Format for reading ICOS observations
#' 
#' @format A \code{\linkS4class{Format}} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' @export
#' 

ICOS <- new("Format",
            id = "ICOS",
            availableQuantities = availableQuantities_ICOS,
            getField = getField_ICOS,
            predefined.layers = list(),
            quantities = ICOS.quantities
)