#!/usr/bin/Rscript

#####################################################################################################
########################### FUNCTIONS TO HANDLE FLUXNET FILES #######################################
#####################################################################################################

#' Get a Field for FLUXNET
#' 
#' An internal function that reads data from FLUXNET observations.   
#' 
#' @param source A  \code{\linkS4class{Source}} containing the meta-data about the FLUXNET observations
#' @param quant A Quantity object to define what quantity from the FLUXNET observations to extract
#' @param layers Ignored for FLUXNET
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

getField_FLUXNET <- function(source,
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
  
  # list all files with daily fluxes
  daily.files <- list.files(path = source@dir) %>%
    stringr::str_subset("DD") %>% stringr::str_subset("VARINFO", negate = T) %>%
    stringr::str_subset("ERAI", negate = T) %>% stringr::str_subset("AA", negate = T)
  
  # determine file type
  CH4.files <- daily.files[grep("CH4", daily.files)]
  FLUXNET2015.files <- daily.files[-grep("CH4", daily.files)]
  
  # loop over all FLUXNET-CH4 files
  for (i in 1:length(CH4.files)) {
    # read the daily data from the .csv file
    site.data <- read.csv(file = file.path(source@dir, CH4.files[i]), na = c("-9999", "NA"))
    
    # read the site info from the .csv file
    siteinfo.files <- list.files(path = source@dir) %>% stringr::str_subset("AA-Flx") %>%
      stringr::str_subset("CH4")
    siteinfo.data <- read.csv(file = file.path(source@dir, siteinfo.files))
    
    # determine site code
    site <- unlist(stringr::str_extract_all(string = CH4.files[i],
                                            pattern = "(?<=FLX_).+(?=_FLUXNET)"))
    
    # determine the longitude and latitude
    lon <- as.numeric(as.character(siteinfo.data$LON[siteinfo.data$SITE_ID == site]))
    lat <- as.numeric(as.character(siteinfo.data$LAT[siteinfo.data$SITE_ID == site]))
    
    # determine the full site name
    site.name <- siteinfo.data$SITE_NAME[siteinfo.data$SITE_ID == site]
    
    # declaring a new data table with metadata that will be used to append the daily fluxes to
    site.data.selected <- data.table(Code = site,
                                     Name = site.name,
                                     Year = as.integer(stringr::str_sub(site.data$TIMESTAMP, 1, 4)),
                                     Day = as.integer(stringr::str_sub(site.data$TIMESTAMP, 7, 8)),
                                     ymd = site.data$TIMESTAMP,
                                     Lon = as.numeric(lon),
                                     Lat = as.numeric(lat))
    
    # selecting the required columns (GPP, NEE, Reco) of the daily fluxes file
    # adds day and night to get daily values
    # divides by a 1000 to convert gC/m^2 to kgC/m^2
    for (v in variables.cfluxes) {
      to.cbind <- select(site.data, contains(toupper(v))) %>%
        select(contains(c("_DT", "_NT")) | ends_with("_F")) %>%
        rowSums() / 1000
      
      site.data.selected <- cbind(site.data.selected, to.cbind)
      setnames(site.data.selected, "to.cbind", v)
    }
   
    # convert day to day of year (doy)
    site.data.selected$Day <- as.integer(lubridate::yday(lubridate::ymd(site.data.selected$ymd)))
    
    if (!missing(first.year)) {
      site.data.selected <- site.data.selected[!(site.data.selected$Year) < first.year,]
    } 
    
    if (!missing(last.year)) {
      site.data.selected <- site.data.selected[!(site.data.selected$Year) > last.year,]
    } 

    if (nrow(site.data.selected) == 0) {
      warning("No data for this time period available. Check first and/or last year")
      stop()
    }
    
    # combine the data tables with selected daily fluxes for all sites
    if (i == 1) {
      FLUXNETCH4.cfluxes <- site.data.selected
    } else {
      FLUXNETCH4.cfluxes <- rbind(FLUXNETCH4.cfluxes, site.data.selected)
    }
  }
  
  
  # loop over all FLUXNET2015 files
  for (i in 1:length(FLUXNET2015.files)) {
    # read the daily data from the .csv file
    site.data <- read.csv(file = file.path(source@dir, FLUXNET2015.files[i]), na = c("-9999", "NA"))

    # read the site info from the .csv file
    siteinfo.files <- list.files(path = source@dir) %>% stringr::str_subset("AA-Flx") %>%
      stringr::str_subset("DD")
    siteinfo.data <- read_xlsx(path = file.path(source@dir, siteinfo.files))
    
    # determine site code
    site <- unlist(stringr::str_extract_all(string = FLUXNET2015.files[i],
                                            pattern = "(?<=FLX_).+(?=_FLUXNET)"))
    
    # determine the longitude and latitude
    lon <- as.numeric(as.character(siteinfo.data$DATAVALUE[siteinfo.data$VARIABLE == "LOCATION_LONG" &
                                                             siteinfo.data$SITE_ID == site]))
    lat <- as.numeric(as.character(siteinfo.data$DATAVALUE[siteinfo.data$VARIABLE == "LOCATION_LAT" &
                                                             siteinfo.data$SITE_ID == site]))
    
    # determine the full site name
    site.name <- siteinfo.data$DATAVALUE[siteinfo.data$VARIABLE == "SITE_NAME" &
                                           siteinfo.data$SITE_ID == site]
    
    # declaring a new data table with metadata that will be used to append the daily fluxes to
    site.data.selected <- data.table(Code = site,
                                     Name = site.name,
                                     Year = as.integer(stringr::str_sub(site.data$TIMESTAMP, 1, 4)),
                                     Day = as.integer(stringr::str_sub(site.data$TIMESTAMP, 7, 8)),
                                     ymd = site.data$TIMESTAMP,
                                     Lon = as.numeric(lon),
                                     Lat = as.numeric(lat))
    
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
    site.data.selected$Day <- as.integer(lubridate::yday(lubridate::ymd(site.data.selected$ymd)))
    
    if (!missing(first.year)) {
      site.data.selected <- site.data.selected[!(site.data.selected$Year) < first.year,]
    } 
    
    if (!missing(last.year)) {
      site.data.selected <- site.data.selected[!(site.data.selected$Year) > last.year,]
    } 
    
    if (nrow(site.data.selected) == 0) {
      warning("No data for this time period available. Check first and/or last year")
      stop()
    }
    
    # combine the data tables with selected daily fluxes for all sites
    if (i == 1) {
      FLUXNET2015.cfluxes <- site.data.selected
    } else {
      FLUXNET2015.cfluxes <- rbind(FLUXNET2015.cfluxes, site.data.selected)
    }
  }
  
  if (exists("FLUXNETCH4.cfluxes") == T & exists("FLUXNET2015.cfluxes") == T) {
    FLUXNET.cfluxes <- rbind(FLUXNETCH4.cfluxes, FLUXNET2015.cfluxes)
  } else if (exists("FLUXNETCH4.cfluxes") == T) {
    FLUXNET.cfluxes <- FLUXNETCH4.cfluxes
  } else {
    FLUXNET.cfluxes <- FLUXNET2015.cfluxes
  }
  
  
  # select the required columns for the current quantity
  if (quant@id %in% variables.cfluxes) {
    FLUXNET.cfluxes %>% select(Year, Day, Lon, Lat, quant@id) -> quant.data
  }
  
  
  # creating a data table with the lon/lat
  gridcells <- data.table(Lon = as.numeric(unique(FLUXNET.cfluxes$Lon)),
                          Lat = as.numeric(unique(FLUXNET.cfluxes$Lat)),
                          Code = unique(FLUXNET.cfluxes$Code),
                          Name = unique(FLUXNET.cfluxes$Name))
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
    first.year = min(quant.data$Year),
    last.year = max(quant.data$Year),
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
########################### FLUXNET AVAILABLE QUANTITIES ############################################
#####################################################################################################

#' List FLUXNET quantities available
#'
#' Simply lists all carbon flux variables available in the FLUXNET data
#' 
#' @return A list of all FLUXNET quantities
#' @keywords internal
#'

availableQuantities_FLUXNET <- function() {
  
  return(FLUXNET.quantities)
  
}

#####################################################################################################
############################ FLUXNET QUANTITIES #####################################################
#####################################################################################################

#' @format The \code{\linkS4class{Quantity}} class is an S4 class with the slots defined below
#' @rdname Quantity-class
#' @keywords datasets
#' 

FLUXNET.quantities <- list(
  new("Quantity",
      id = "GPP",
      name = "GPP",
      units = "kgC/m^2",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("FLUXNET"),
      standard_name = "gross_primary_productivity"),
  new("Quantity",
      id = "NEE",
      name = "NEE",
      units = "kgC/m^2",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("FLUXNET"),
      standard_name = "net_ecosystem_exchange"),
  new("Quantity",
      id = "Reco",
      name = "Reco",
      units = "kgC/m^2",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("FLUXNET"),
      standard_name = "ecosystem_respiration")
)



#####################################################################################################
############################ FLUXNET FORMAT #########################################################
#####################################################################################################

#' @description \code{FLUXNET} - a Format for reading FLUXNET observations
#' 
#' @format A \code{\linkS4class{Format}} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' @export
#' 

FLUXNET <- new("Format",
            id = "FLUXNET",
            availableQuantities = availableQuantities_FLUXNET,
            getField = getField_FLUXNET,
            predefined.layers = list(),
            quantities = FLUXNET.quantities
)