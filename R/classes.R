#!/usr/bin/Rscript


#' Benchmark class
#'
#' This class encapsulates all the specific details benchmark comparison.
#' 
#' @slot id Simple character string to gave an uniquely identify this Benchmark
#' @slot name More complex character string to describe this Benchmark
#' @slot description An even more complex string to describe the Benchmark, probably redundant
#' @slot simulation A character string to describe which type of simulation is needed to compare to this benchmark.  Can be a vector, in which they
#' will be tried in order
#' @slot guess_var Character describing the modelled variable(s) needed for this benchmark
#' @slot guess_layers Character describing the modelled layers(s) needed for this benchmark
#' @slot unit Character string for the unit used in individual gridcells in the spatial plots, will be evaluated as an expression.
#' @slot agg.unit Character string for the aggregated units used for global summaries, will be evaluated as an expression.
#' @slot datasets A list of DGVMTools::Source objects that are used for comparison in this benchmark
#' @slot custom A list (of lists of lists...) defining custom parameters for this benchmark.  This gives the required flexibility to 
#' to handle whatever idiosyncrasies a benchmark has to complement the more defined slots above.
#' 
#' @details This class defines metadata about a benchmark and the DGVMTools::Field to which the model output should be compared.  *But* it should be kept in
#' mind that benchmarks and datasets can vary a lot in terms of units, processing needed, how to deal with different time period etc, so the actual information
#' stored in the class needs to be used carefully and should not be too automated.
#' 
#' @name Benchmark-class
#' @rdname Benchmark-class
#' @exportClass Benchmark
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("Benchmark", 
         slots = c(id = "character",
                   name = "character",
                   description = "character",
                   simulation = "character",
                   guess_var = "character",
                   guess_layers = "character",
                   unit = "character",
                   agg.unit = "character",
                   datasets = "list",
                   metrics = "character",
                   custom = "list"),
         contains = "STAInfo",
         
         # TODO make a function to check the validity of a created benchmark, at a minimum that all elements of the datasets slot list are DGVMTools::Fields
)

