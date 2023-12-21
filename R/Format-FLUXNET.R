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
#' @param day.night.method Nighttime (NT) or daytime (DT) partition method for GPP and Reco. Default is NT
#' @param first.year Optional, will exclude data before this year
#' @param last.year Optional, will exclude data beyond this year
#' @param rm.leap A logical, set to true to remove leap days
#' @param data.cleaning A logical, set to true to apply data cleaning on FLUXNET2015 data
#' @param qc.threshold Optional, set to a value between 0 and 1 below which data will be set to NA. Default is 0.5. Set to NULL to omit this from data cleaning 
#' @import stringr
#' @import dplyr
#' @import lubridate
#' @import readxl
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
                              day.night.method = "NT",
                              first.year,
                              last.year,
                              rm.leap = TRUE,
                              data.cleaning = TRUE,
                              qc.threshold = 0.5,
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
  if (length(CH4.files) == 0) {
    FLUXNET2015.files <- daily.files
  } else {
    FLUXNET2015.files <- daily.files[-grep("CH4", daily.files)]
  }
  
  if (length(CH4.files) != 0) {
    
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
      # MF: Some sites have more than one location recorded in the site description file, due to reasons like the tower being moved
      # or the meteorogical instrumentation being set located slightly further away.
      #  The above applies to sites in the 2015 release, I have no idea how it applies to the CH4 community product.  
      # I have adopted the convention of just taking the first location in the file
      lon <- as.numeric(as.character(siteinfo.data$LON[siteinfo.data$SITE_ID == site]))[1]
      lat <- as.numeric(as.character(siteinfo.data$LAT[siteinfo.data$SITE_ID == site]))[1]
      
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
      # umolCO2/m^2/s to kgC/m^2
      to.cbind <- select(site.data, contains(quant@id)) %>%
        select(contains(day.night.method) | ends_with("_F")) %>%
        rowSums() * 1e-06 * 0.012011 * 86400 
      
      if (data.cleaning == TRUE) {
        # set negative GPP / Reco values to NA
        if (quant@name == "GPP" | quant@name == "Reco") {
          to.cbind[which(to.cbind < 0)] <- NA
        }
      }
      
      site.data.selected <- cbind(site.data.selected, to.cbind)
      setnames(site.data.selected, "to.cbind", quant@id)
      
      site.data.selected$Day <- as.integer(lubridate::yday(lubridate::ymd(site.data.selected$ymd)))
      
      # remove leap days and / or convert day to day
      if (rm.leap == TRUE) {
        indx <- which(site.data.selected$Day == 60 & leap_year(site.data.selected$Year))
        if (length(indx) > 0) {
          site.data.selected <- site.data.selected[-indx,]
          
          site.data.selected$Day[which(leap_year(site.data.selected$Year) &
                                         site.data.selected$Day > 60)] <- site.data.selected$Day[which(leap_year(site.data.selected$Year) &
                                                                                                         site.data.selected$Day > 60)]  - 1
        }
      }
      
      if (!missing(first.year)) {
        site.data.selected <- site.data.selected[!(site.data.selected$Year < first.year),]
      } 
      
      if (!missing(last.year)) {
        site.data.selected <- site.data.selected[!(site.data.selected$Year > last.year),]
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
  }
  
  if (length(FLUXNET2015.files) != 0) {
    
    # make list of the sites (individual rows to be rbind-ed at the end)
    all.site.data <- list()
    
    # read the site info from the .csv file (same for all sites so only need to do this once)
    siteinfo.files <- list.files(path = source@dir) %>% stringr::str_subset("AA-Flx") %>%
      stringr::str_subset("DD")
    siteinfo.data <- readxl::read_xlsx(path = file.path(source@dir, siteinfo.files))
    
    # loop over all FLUXNET2015 files
    for (i in 1:length(FLUXNET2015.files)) {
      
      # read the daily data from the .csv file
      site.data <- read.csv(file = file.path(source@dir, FLUXNET2015.files[i]), na = c("-9999", "NA"))
      
       
      # determine site code
      site <- unlist(stringr::str_extract_all(string = FLUXNET2015.files[i],
                                              pattern = "(?<=FLX_).+(?=_FLUXNET)"))
      
      # determine the longitude and latitude
      # MF: Some sites have more than one location recorded in the site description file, due to reasons like the tower being moved
      # or the meteorogical instrumentation being set located slightly further aaway
      # This currently affects: DE-Gri, DE-RuS, RU-Sam, US-MyB, US-Tw3, US-TwT
      # I have adopted the convention of just taking the first location in the file
      lon <- as.numeric(as.character(siteinfo.data$DATAVALUE[siteinfo.data$VARIABLE == "LOCATION_LONG" &
                                                               siteinfo.data$SITE_ID == site]))[1]
      lat <- as.numeric(as.character(siteinfo.data$DATAVALUE[siteinfo.data$VARIABLE == "LOCATION_LAT" &
                                                               siteinfo.data$SITE_ID == site]))[1]
      
      # determine the full site name
      site.name <- siteinfo.data$DATAVALUE[siteinfo.data$VARIABLE == "SITE_NAME" &
                                             siteinfo.data$SITE_ID == site]
      #print(site)
      # declaring a new data table with metadata that will be used to append the daily fluxes to
      site.data.selected <- data.table(Code = site,
                                       Name = site.name,
                                       Year = as.integer(stringr::str_sub(site.data$TIMESTAMP, 1, 4)),
                                       Day = as.integer(stringr::str_sub(site.data$TIMESTAMP, 7, 8)),
                                       ymd = site.data$TIMESTAMP,
                                       Lon = as.numeric(lon),
                                       Lat = as.numeric(lat))
      
      # declare a new data.table with only the site data
      all.site.data[[site]] <- data.table(Lon = as.numeric(lon),
                                       Lat = as.numeric(lat),
                                       Code = site,
                                       Name = site.name)

      # selecting the required columns (GPP, NEE, Reco) of the daily fluxes file
      # divides by a 1000 to convert gC/m^2 to kgC/m^2
      if (quant@name == "NEE" & day.night.method == "NT") {
        day.night.method <- "NIGHT"
      } else if (quant@name == "NEE" & day.night.method == "DT") {
        day.night.method <- "DAY"
      }
      
      if (quant@name == "NEE") {
        to.cbind <- select(site.data, contains(UT.threshold)) %>%
          select(contains(quant@name)) %>%
          select(contains(partition.method)) %>%
          select(ends_with(day.night.method)) %>%
          rowSums() / 1000
        
      } else {
        to.cbind <- select(site.data, contains(UT.threshold)) %>%
          select(contains(quant@name)) %>%
          select(contains(partition.method)) %>%
          select(contains(day.night.method)) %>%
          rowSums() / 1000
      }

      # data cleaning
      if (data.cleaning == TRUE) {
 
        # set negative GPP / Reco values to NA
        if (quant@name == "GPP" | quant@name == "Reco") {
          to.cbind[which(to.cbind < 0)] <- NA
          
          # select diff < 50%
          temp <- select(site.data, contains(UT.threshold)) %>%
            select(contains(quant@name)) %>%
            select(contains(partition.method)) %>%
            select(contains("DT") | contains("NT"))
          
          colnames(temp) <- c("DT", "NT")
          
          for (c in 1:length(to.cbind)) {
            if (is.na(temp$DT[c]) == T | is.na(temp$NT[c]) == T) {
              next
            }
            else if (temp$DT[c] == 0 & temp$NT[c] == 0) {
              next
            }
            else if (100 * (abs(temp$NT[c] - temp$DT[c]) / (abs(temp$NT[c] + temp$DT[c]) / 2)) >= 50) {
              to.cbind[c] <- NA
            }
          }
        }
    

        # set values with NEE QC below threshold as NA
        if (is.null(qc.threshold) == FALSE) {
          if (day.night.method == "NT" | day.night.method == "NIGHT") {
            day.night.method.NEE <- "NIGHT"
          } else if (day.night.method == "DT" | day.night.method == "DAY") {
            day.night.method.NEE <- "DAY"
          }
          
          nee.qc.data <- select(site.data, contains(UT.threshold)) %>%
            select(contains("NEE")) %>% select(contains(partition.method)) %>%
            select(contains(day.night.method.NEE)) %>% select(ends_with("QC"))
          
          to.cbind[which(nee.qc.data < qc.threshold)] <- NA
        }
        
      }

      site.data.selected <- cbind(site.data.selected, to.cbind)
      setnames(site.data.selected, "to.cbind", quant@name)
      
      site.data.selected$Day <- as.integer(lubridate::yday(lubridate::ymd(site.data.selected$ymd)))
      
      # remove leap days and / or convert day to day
      if (rm.leap == TRUE) {
        indx <- which(site.data.selected$Day == 60 & leap_year(site.data.selected$Year))
        if (length(indx) > 0) {
          site.data.selected <- site.data.selected[-indx,]
          
          site.data.selected$Day[which(leap_year(site.data.selected$Year) &
                                         site.data.selected$Day > 60)] <- site.data.selected$Day[which(leap_year(site.data.selected$Year) &
                                                                                                         site.data.selected$Day > 60)]  - 1
        }
      }
      
      if (!missing(first.year)) {
        site.data.selected <- site.data.selected[!(site.data.selected$Year < first.year),]
      } 
      
      if (!missing(last.year)) {
        site.data.selected <- site.data.selected[!(site.data.selected$Year > last.year),]
      } 
      
      if (nrow(site.data.selected) == 0) {
        warning("No data for this time period available. Check first and/or last year")
        stop()
      }
      
      # combine the data tables with selected daily fluxes for all sites
      if (i == 1) {
        FLUXNET2015.cfluxes <- site.data.selected
        #FLUXNET2015.gridcells <- site.location.data
      } else {
        FLUXNET2015.cfluxes <- rbind(FLUXNET2015.cfluxes, site.data.selected)
        #FLUXNET2015.gridcells <- rbind(FLUXNET2015.gridcells, site.location.data)
      }
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
  gridcells <- rbindlist(all.site.data)
    
   # FLUXNET2015.gridcells 
  print(gridcells)
  # gridcells <- data.table(Lon = as.numeric(unique(FLUXNET.cfluxes$Lon)),
  #                         Lat = as.numeric(unique(FLUXNET.cfluxes$Lat)),
  #                         Code = unique(FLUXNET.cfluxes$Code),
  #                         Name = unique(FLUXNET.cfluxes$Name))
  
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
      units = "kgC/m^2/day",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("FLUXNET"),
      standard_name = "gross_primary_productivity"),
  new("Quantity",
      id = "NEE",
      name = "NEE",
      units = "kgC/m^2/day",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("FLUXNET"),
      standard_name = "net_ecosystem_exchange"),
  new("Quantity",
      id = "Reco",
      name = "Reco",
      units = "kgC/m^2/day",
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