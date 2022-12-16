#' Calculate linear trend
#' 
#' Calculate a linear trend from a single variable spatial Field.  Field must have a "Year" dimension, and can optionally be monthly or daily.
#' 
#' @param field The Field for which the trend is to be calculated.  Should be spatial, and have a least a "Year" time dimension. 
#' @param signif_level Numeric, if not NULL or NA then the significance level for "Significant_Trend" layer (default is 0.05) 
#' @name calcLinearTrend
#' @rdname calcLinearTrend
#' @import DGVMTools
#' @import data.table
#' @export
#' @return A DGVMTools::Field object containing the trend and the associated p-value.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 

calcLinearTrend <- function(field, signif_level = 0.05) {
  
  
  # check  layers (there should be only one)
  field_names <- layers(field)
  if(!length(field_names) ==  1) stop("Please run calcLinearTrend on a Field with a single layer only.")
  
  # build data table for trend calculation
  full_dt <- copy(field@data)
  setnames(full_dt, old = field_names, new ="the_variable")
  if("Month" %in% getDimInfo(field)) full_dt[, Time := (Year*12) + Month]
  else if("Day" %in% getDimInfo(field)) full_dt[, Time := (Year*365) + Day]
  else if("Year" %in% getDimInfo(field)) full_dt[, Year := Year]
  
  # calculate the trend (and p-value)
  trends_dt <- full_dt[, as.list(summary(lm(the_variable~Time))$coefficients[2,c(1,4)]), by = .(Lon, Lat)]
  setnames(trends_dt, c("Estimate", "Pr(>|t|)"), c("Trend", "p.value"))
  
  
  # # optionally make a layer with only significant trends
  if(!is.na(signif_level) && !is.null(signif_level)) {
     trends_dt[ , Significant_Trend := (ifelse(p.value > signif_level, NA, Trend))]
  }
  
  # pop into a Field and return
  trend_field <- copy(field)
  trend_field@data <- trends_dt
  trend_field@year.aggregate.method <- "linear_trend"
  trend_field@id <- makeFieldID(trend_field)
  
  return(trend_field)
  
}