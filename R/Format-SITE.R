#####################################################################################################
############################ FUNCTIONS TO HANDLE SITE FILES #########################################
#####################################################################################################

#' Get a Field for ICOS
#' 
#' An internal function that reads data from ICOS observations.   
#' 
#' @param source A  \code{\linkS4class{Source}} containing the meta-data about the ICOS observations
#' @param quant A Quantity object to define what quantity from the ICOS observations to extract
#' @param layers Ignored for SITE
#' @param target.STAInfo The spatial-temporal target domain
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is just taken to be 
#' "<quant@id>.out" (also "<quant@id>.out.gz")
#' @param verbose A logical, set to true to give progress/debug information
#' @param first.year Optional, will exclude data before this year
#' @param last.year Optional, will exclude data beyond this year
#' @import stringr
#' @import dplyr
#' @import lubridate
#' @return A list containing firstly the data.table containing the data, and secondly the STA.info 
#' @author Karl Piltz \email{karl.piltz@@nateko.lu.se}
#' @keywords internal
#'
getField_SITE <- function(source,
                              quant,
                              layers = NULL,
                              target.STAInfo,
                              file.name,
                              verbose,
                              first.year,
                              last.year,
                              ...) {
  
  ### CHECK ARGUEMENTS
  if(!missing(first.year) & !missing(last.year) ) {
    if(first.year > last.year) stop("first.year cannot be greater than last.year!")
  }
  
  variable <- quant@id
  
  always.read <- c("Lon", "Lat", "Year", "Month", "Day", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  # extract from the target.sta
  first.year = target.STAInfo@first.year
  last.year = target.STAInfo@last.year
  
  
  # Make the filename and read the file using the handy utility function
  file_path <- file.path(paste0(source@dir, "//", variable, ".csv"))
  
  dt <- fread(file_path,header = TRUE)
  dt[is.na(dt)] <- 0
  
  
  # correct the dimension names if they are lower case
  dim.names <- c("Lon", "Lat", "Year", "Month", "Day", "Site", "Station")
  dt.header <- names(dt)
  for(dim.name in dim.names) {
    if(tolower(dim.name) %in% tolower(dt.header) && !identical(dim.name, dt.header[which(tolower(dim.name) == tolower(dt.header))])) {
      if(verbose) message(paste("Correcting column name", dt.header[which(tolower(dim.name) == tolower(dt.header))], "to", dim.name))
      setnames(dt, dt.header[which(tolower(dim.name) == tolower(dt.header))], dim.name)
    }
  }
  
  #  Print messages
  if(verbose) {
    message("Read table. It has header:")
    print(names(dt))
    message("It has shape:")
    print(dim(dt))      
  }
  
  # Correct year
  if(source@year.offset != 0) {
    dt[,Year := Year + source@year.offset]
    if(verbose) message("Correcting year with offset.")
  }
  
  # Select year if necessary
  call.selectYear <- FALSE
  
  actual.first.year <- min(dt[["Year"]])
  actual.last.year <- max(dt[["Year"]])
  
  # check if cropping needs to be done based on first year
  if(length(target.STAInfo@first.year) == 1 && target.STAInfo@first.year != actual.first.year) {
    call.selectYear <- TRUE
    first.year <- target.STAInfo@first.year
  }
  else first.year <- actual.first.year
  
  # check if cropping needs to be done based on last year
  if(length(target.STAInfo@last.year) == 1 && target.STAInfo@last.year != actual.last.year) {
    call.selectYear <- TRUE
    last.year <- target.STAInfo@last.year
  }
  else last.year <- actual.last.year
  
  # 
  if(call.selectYear) {
    if(verbose) message(paste("Selecting years from", first.year, "to", last.year, "in table.", sep = " "))
    this.Field <- selectYears(dt, first = first.year, last = last.year) 
  }
  else{
    if(verbose) message("No year selection being applied to table.")
  }
  
  
  # also correct days to be 1-365 instead of 0-364, if necessary
  if("Day" %in% names(dt)) {
    if(0 %in% unique(dt[["Day"]])) dt[, Day := Day+1]
  }
  
  # Correct lon and lats
  if(length(source@lonlat.offset) == 2 ){
    if(verbose) message("Correcting lons and lats with offset.")
    if(source@lonlat.offset[1] != 0) dt[, Lon := Lon + source@lonlat.offset[1]]
    if(source@lonlat.offset[2] != 0) dt[, Lat := Lat + source@lonlat.offset[2]]
  }
  else if(length(source@lonlat.offset) == 1 ){
    if(verbose) message("Correcting lons and lats with offset.")
    if(source@lonlat.offset[1] != 0) dt[, Lon := Lon + source@lonlat.offset[1]]
    if(source@lonlat.offset[1] != 0) dt[, Lat := Lat + source@lonlat.offset[1]]
  }
  
  if(verbose) {
    message("Offsets applied. Head of full .csv file (after offsets):")
    print(utils::head(dt))
  }
  
  # if london.centre is requested, make sure all longitudes greater than 180 are shifted to negative
  if(source@london.centre){
    if(max(dt[["Lon"]]) > 180) {
      dt[, Lon := LondonCentre(Lon)]
    }
  }
  
  # if spatial extent specified, crop to it
  # new.extent <- NULL
  # full.extent <- extent(dt)
  # if(!is.null(target.STAInfo@spatial.extent)) {
  #   
  #   spatial.extent.class <- class(target.STAInfo@spatial.extent)[1]
  #   
  #   if(spatial.extent.class == "SpatialPolygonsDataFrame" || spatial.extent.class == "numeric" || is.data.frame(target.STAInfo@spatial.extent) || is.data.table(target.STAInfo@spatial.extent)) {
  #     dt <- selectGridcells(x = dt, gridcells = target.STAInfo@spatial.extent, spatial.extent.id = target.STAInfo@spatial.extent.id, ...)
  #     new.extent <- target.STAInfo@spatial.extent
  #     # if new.extent is a data.frame, convert it to a data.table for consistency
  #     #if(is.data.frame(new.extent) & !is.data.table(new.extent)) new.extent <- as.data.table(new.extent)
  #   }
  #   
  #   else {
  #     dt <- crop(x = dt, y = target.STAInfo@spatial.extent, spatial.extent.id = target.STAInfo@spatial.extent.id)
  #     new.extent <- extractRasterExtent(target.STAInfo@spatial.extent)
  #   } 
  #   
  # }
  #gc()
  # if year cropping selected, do that here, before aggregating
  all.years <- sort(unique(dt[["Year"]]))
  
  crop.first <- FALSE
  if(length(target.STAInfo@first.year) == 1) {
    if(target.STAInfo@first.year != min(all.years)) {
      first.year <- target.STAInfo@first.year
      crop.first <- TRUE
    }
    else {
      first.year <- min(all.years)
      crop.first <- FALSE
    }
  }
  
  crop.last <- FALSE
  if(length(target.STAInfo@last.year) == 1) {
    if(target.STAInfo@last.year != max(all.years)) {
      last.year <- target.STAInfo@last.year
      crop.last <- TRUE
    }
    else {
      last.year <- target.STAInfo@last.year
      crop.last <- FALSE
    }
  }
  
  if(crop.first || crop.last) {
    
    if(verbose) message(paste("Selecting years from", first.year, "to", last.year, sep = " "))
    dt <- selectYears(dt, first = first.year, last = last.year) 
    all.years <- sort(unique(dt[["Year"]]))
  }
  else {
    if(verbose) message("No year selection being applied")
  }
  
  
  
  # if yearly aggregating requested, so it before melting (so save on memory)
  # first store all the years before averaging them away
  
  this.year.aggregate.method <- "none"
  if(target.STAInfo@year.aggregate.method != "none") {
    
    dt <- aggregateYears(x = dt, method = target.STAInfo@year.aggregate.method, verbose = verbose)
    this.year.aggregate.method <- target.STAInfo@year.aggregate.method
    
  }
  #gc()
  
  # If data is has monthly or daily columns, melt to long/tidy data where "Month" becomes a column
  
  # first get of all the columns which are not spatial-temporal info
  all.cols <- names(dt)
  st.cols <- getDimInfo(dt)
  nonst.cols <- all.cols[!all.cols %in% st.cols]
  
  # if monthly then melt
  standard.monthly.ljp.col.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  if(identical(nonst.cols, standard.monthly.ljp.col.names)){
    
    # replace column names with 1,2,3.. etc before melting, and then melt
    setnames(dt, old = standard.monthly.ljp.col.names, new = paste(1:12))
    dt <- melt(dt, id.vars = st.cols, measure.vars = paste(1:12), variable.name = "Month", value.name = layers)
    dt[, Month := as.integer(Month)]
    
  }
  
  
  
  # set the keys (very important!)
  setKeyDGVM(dt)
  
  # Build as STAInfo object describing the data
  dimensions <- getDimInfo(dt)
  subannual <- "Year"
  if("Month" %in% dimensions){ subannual <- "Month"
  }else if("Day" %in% dimensions) subannual <- "Day"
  
  
  if("Site" %in% names(dt)){
  gridcells <- data.table(Lon = as.numeric(dt$Lon),
                          Lat = as.numeric(dt$Lat),
                          xmin = as.numeric(min(dt$Lon)),
                          ymin = as.numeric(min(dt$Lat)),
                          xmax = as.numeric(max(dt$Lon)),
                          ymax = as.numeric(max(dt$Lat)),
                          Site = as.character(dt$Site))}
  else{
    gridcells <- data.table(Lon = as.numeric(dt$Lon),
                            Lat = as.numeric(dt$Lat),
                            xmin = as.numeric(min(dt$Lon)),
                            ymin = as.numeric(min(dt$Lat)),
                            xmax = as.numeric(max(dt$Lon)),
                            ymax = as.numeric(max(dt$Lat)))}

  
  # Create STAInfo object
  final.STAInfo <- new("STAInfo",
                  first.year = min(dt$Year),
                  last.year = max(dt$Year),
                  year.aggregate.method = this.year.aggregate.method,
                  spatial.extent = gridcells,
                  spatial.extent.id = paste("All_", quant@id, "_Sites", sep = ""),
                  spatial.aggregate.method = "none",
                  subannual.resolution = subannual,
                  subannual.aggregate.method = subannual,
                  subannual.original = subannual)
 
  # if cropping has been done, set the new spatial.extent and spatial.extent.id
  # if(!is.null(new.extent))  {
  #   final.STAInfo@spatial.extent = new.extent
  #   final.STAInfo@spatial.extent.id <- target.STAInfo@spatial.extent.id
  # }
  # # otherwise set to the (potentially what the extent was before spatial aggregating)
  # else {
  #   final.STAInfo@spatial.extent = full.extent
  #   final.STAInfo@spatial.extent.id <- paste("All_", quant@id, "_Sites", sep = "")
  # }
  
  #gc()
  
  
  # Create Field object
  field.id <-
    makeFieldID(
      source = source,
      quant.string = quant@id,
      sta.info = final.STAInfo
    )
  
  return.field <- new(
    "Field",
    id = field.id,
    quant = quant,
    data = dt,
    source = source,
    final.STAInfo
  )
  
  return(return.field)
}

#####################################################################################################
############################ SITE AVAILABLE QUANTITIES ##############################################
#####################################################################################################

#' List SITE quantities available
#'
#' Simply lists all variables available in the SITE data
#' 
#' @return A list of all SITE quantities
#' @keywords internal
#'

availableQuantities_SITE <- function() {
  
  return(SITE.quantities)
  
}

#####################################################################################################
############################ SITE QUANTITIES ########################################################
#####################################################################################################

#' @format The \code{\linkS4class{Quantity}} class is an S4 class with the slots defined below
#' @rdname Quantity-class
#' @keywords datasets
#' 

SITE.quantities <- list(
  new("Quantity",
      id = "lai",
      name = "lai",
      units = "m^2/m^2/y",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("SITE"),
      standard_name = "leaf_area_index"),
  
  new("Quantity",
      id = "cmass",
      name = "cmass",
      units = "kgC/m^2/y",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("SITE"),
      standard_name = "carbon_mass"),
  
  new("Quantity",
      id = "dens",
      name = "dens",
      units = "indiv/ha",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("SITE"),
      standard_name = "stem_density"),
  
  new("Quantity",
      id = "QDBH",
      name = "QDBH",
      units = "cm",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("SITE"),
      standard_name = "quadratic_mean_diamiter"),
  
  new("Quantity",
      id = "DBH",
      name = "DBH",
      units = "cm",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("SITE"),
      standard_name = "diamiter_at_breast_height")
)



#####################################################################################################
############################ SITE FORMAT ############################################################
#####################################################################################################

#' @description \code{SITE} - a Format for reading SITE observations
#' 
#' @format A \code{\linkS4class{Format}} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' @export
#' 

SITE <- new("Format",
            id = "SITE",
            availableQuantities = availableQuantities_SITE,
            getField = getField_SITE,
            predefined.layers = list(),
            quantities = SITE.quantities
)