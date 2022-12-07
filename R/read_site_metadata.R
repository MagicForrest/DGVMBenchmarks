#' Read site metadata
#' 
#' blah
#' 
#' @param file_name Character, an ICOS text file (no processing required)
#' @name read_site_metadata
#' @rdname read_site_metadata
#' @import DGVMTools dplyr
#' @import data.table
#' @export
#' @return A list of S4 objects of class Site.
#' @author Margot Knapen \email{Margot.Knapen@@nateko.lu.se}
#' 
#' details

read_site_metadata <- function(file_name) {
  library(dplyr)
  
  # ***************************** READ INPUT FILE ******************************
  # reads the input file as a table with two columns
  # assigns column names
  input <-
    read.delim(
      file = file_name,
      header = F,
      sep = "",
      comment.char = "!",
      stringsAsFactors = F
    )
  
  colnames(input) <- c("Variable", "Value")
  
  
  # ************************** DEFINE S4 CLASS *********************************
  # defines the required S4 classes
  anc_var_names <-
    as.list(trimws(strsplit(input$Value[input$Variable == 
                                          "variables_ancillary"], ",")[[1]]))
  ancillary_slots <-
    lapply(anc_var_names, function(x) {
      x <- "numeric"
    }) %>% setNames(anc_var_names)
  ancillary <- setClass("ancillary", slots = ancillary_slots)
  
  variables_var_names <-
    c("variables_fluxes", "UT_method", "suffix")
  variables_slots <-
    lapply(variables_var_names, function(x) {
      x <- "character"
    }) %>% setNames(variables_var_names)
  variables <- setClass("variables", slots = variables_slots)
  
  paths_var_names <-
    c("path_ICOS", "path_LPJGUESS", "path_ancillary")
  paths_slots <-
    lapply(paths_var_names, function(x) {
      x <- "character"
    }) %>% setNames(paths_var_names)
  paths <- setClass("paths", slots = paths_slots)
  
  sites_var_names <-
    c(
      "name",
      "longitude_ICOS",
      "latitude_ICOS",
      "longitude",
      "latitude",
      "first_year",
      "last_year"
    )
  
  sites_slots <- lapply(sites_var_names, function(x) {
    if (x == "name") {
      x <- "character"
    } else {
      x <- "numeric"
    }
  }) %>%
    setNames(sites_var_names)
  
  sites <- setClass("sites",
                    slots = append(
                      sites_slots,
                      list(
                        paths = "paths",
                        variables = "variables",
                        ancillary = "ancillary"
                      )
                    ))
  
  # empty list to which the output will be appended to
  output <- list()
  
  
  # ******************************* DEFINE INPUT *******************************
  # selects the rows that belong to each site
  # loops through all sites and creates empty S4 classes to add the data to
  # populates the empty S4 classes with the available input data
  # checks whether the required input data is specified
  # throws errors or shows warnings
  start_r <- which(input$Variable == "site")
  end_r <- tail(start_r,-1) %>% -1 %>% append(nrow(input))
  
  for (r in 1:length(start_r)) {
    temp_s <- new("sites")
    temp_p <- new("paths")
    temp_v <- new("variables")
    input_slice <- input[(1 + start_r[r]):end_r[r], ]
    
    # adds site info to the temporary, empty S4 class
    for (i in names(sites_slots)) {
      if (sites_slots[[i]] == "character") {
        slot(temp_s, i) <- input_slice$Value[input_slice$Variable == i][1]
      } else {
        slot(temp_s, i) <-
          as.numeric(input_slice$Value[input_slice$Variable == i][1])
      }
    }
    
    # error if no grid lon/lat has been specified
    for (i in c("longitude", "latitude")) {
      if (length(slot(temp_s, i)) == 0 | is.na(slot(temp_s, i)) == TRUE) {
        warning(
          paste0(
            "No ",
            i,
            " has been given for ",
            temp_s@name,
            ". But this is required to run the scripts!"
          )
        )
        stop("Check input file!")
      }
    }
    
    # adds path info, either site-specific or general
    for (i in names(paths_slots)) {
      if (i %in% input_slice$Variable & i != "path_LPJGUESS") {
        slot(temp_p, i) <- input_slice$Value[input_slice$Variable == i]
      } else {
        input %>% head(start_r[1]) %>% filter(Variable == i) %>%
          select(Value) %>% as.character() -> slot(temp_p, i)
      }
    }
    temp_s@paths <- temp_p
    
    # adds variable info
    for (i in names(variables_slots)) {
      input %>% head(start_r[1]) %>% filter(Variable == i) %>%
        select(Value) %>% as.character() -> slot(temp_v, i)
    }
    temp_s@variables <- temp_v
    
    for (i in c("name",
                "longitude_ICOS",
                "latitude_ICOS",
                "first_year",
                "last_year")) {
      if (length(slot(temp_s, i)) == 0 ||
          is.na(slot(temp_s, i)) == TRUE) {
        warning(paste0("No ", i, " has been given for ",
                       temp_s@name))
      }
    }
    output <- append(output, temp_s)
  }
  
  # ***************************** OUTPUT ***************************************
  # output as list with all sites
  names(output) <- input$Value[input$Variable == "site"]
  return(output)
  
}