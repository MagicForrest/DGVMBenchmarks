#' Read GCP
#' 
#' Read the Global Carbon Project's Global Carbon Budget.  The default are for the GCB 2020 Excel sheet.
#' 
#' @param data_file Optional, a character string defining the benchmarking data to be used.  If not supplied the file internal 
#' to the package will be used
#' @param sheet Optional, a character string giving the name of the shet to be read
#' @param skip_rows Optional, integer, the number of rows on the sheet to skip to get to the data
#' @param nyears_GCP Optional, integer, the number if years (rows) of data to read
#' @param land_sink_col_name Optional, a character string giving the column name for the land sink
#' @param luc_col_name Optional, a character string giving the column name for the land use change column
#' @param imbalance_col_name Optional, a character string giving the column name for the budget imbalance column
#' 
#' @name read_GCP
#' @rdname read_GCP
#' @import DGVMTools
#' @import data.table
#' @import readxl
#' @export
#' @return A DGVMTools::Field object containing the GCP data
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 

read_GCP <- function(data_file,
                     sheet,
                     skip_rows,
                     nyears_GCP,
                     land_sink_col_name = "land sink",
                     luc_col_name = "land-use change emissions",
                     imbalance_col_name = "budget imbalance") {
  
  #### READ THE BENCHMARKING DATA ####
  
  # location of input data
  if(missing(data_file)) {
    input_dir <- system.file("extdata", "GCP", package = "DGVMBenchmarks")
    GCP_file_name <- "Global_Carbon_Budget_2020v1.0.xlsx"
    GCP_file_name <- "Global_Carbon_Budget_2024_v1.0.xlsx"
    data_file <- file.path(input_dir, GCP_file_name)
  }
  else {
    input_dir <- strsplt(data_file, "/")
    input_dir <- "cheese"
  }
  
  # read the .xlxs file (nothing to do with DGVMTools here)
  require(readxl, quietly = TRUE)
  GCP_full_dt <- data.table::as.data.table(read_xlsx(data_file, skip = skip_rows, n_max = nyears_GCP, sheet = sheet))
  
  
  
 #### PUT THE BENCHMARKING DATA INTO MANUALLY DEFINE DGVMTOOLS FIELD OBJECT ####
  
  # define the GCP data source
  # note that the "format" and "dir" arguments is arbitrary because we are short-cutting DGVMTools's normal means for reading in data 
  GCP_Source <- defineSource(id = "GCP",
                             name = "GCP NBP",
                             dir = input_dir,
                             format = GUESS)
  
  # calculate the residual 
  #GCP_full_dt[, NBP := `fossil emissions excluding carbonation` - `atmospheric growth` - `ocean sink` - `cement carbonation sink`]
  GCP_full_dt[, NBP := get(land_sink_col_name) + get(imbalance_col_name) - get(luc_col_name)]
  
 
  # hack this into a DGVMTools::Field object
  # note that this is not the preferred method get getting Fields with LPJ-GUESS (that would be a call to getField())
  # but this is a work around for working with Excel files
  GCP_full_Field <- new(Class = "Field",
                        source = GCP_Source,
                        quant = lookupQuantity("cflux", GUESS),
                        data = GCP_full_dt,
                        first.year = min(GCP_full_dt[["Year"]]),
                        last.year = max(GCP_full_dt[["Year"]]),
                        year.aggregate.method = "none",
                        subannual.resolution = "Year",
                        subannual.original = "Year",
                        subannual.aggregate.method = "none",
                        spatial.aggregate.method = "w.sum",
                        spatial.extent.id = "Full",
                        spatial.extent = raster::extent(c(-180, 180, -90, 90)))

 return(GCP_full_Field)  
  
}