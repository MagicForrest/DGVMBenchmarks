% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelSummaryTable.R
\name{modelSummaryTable}
\alias{modelSummaryTable}
\title{Produce a model summary table}
\usage{
modelSummaryTable(
  simulations,
  settings,
  var,
  periods,
  new_sim,
  old_sim,
  spatial_extent,
  spatial_extent_id,
  area_unit = "m^2",
  unit_multiplier = 1,
  sigfigs = 3,
  version_label = character(0)
)
}
\description{
The table produced here compares multiple model versions for all layers of a single variable for an arbitrary selection of time periods.
Optionally it can be subset over a spatial extent.
}
