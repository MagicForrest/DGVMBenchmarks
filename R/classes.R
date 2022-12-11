#!/usr/bin/Rscript


#' Benchmark class
#'
#' This class encapsulates all the specific details of a supported model or dataset.  This comprises functions to read the data off the disk, 
#' and metadata describing certain pre-defined \code{\linkS4class{Quantity}} and \code{\linkS4class{Layer}} objects. From a user-perspective, these are used as the \code{\linkS4class{Format}} argument to
#' \code{\link{defineSource}} function.
#' 
#'  Format objects which are included in the package are listed below:
#'
#' @slot id Simple character string to gave an uniquely identify this format
#' @slot predefined.layers 'Standard' Layer type that this format uses, as a list of \code{\linkS4class{Layer}} objects.  
#' This is most likely applicable to formats describing DGVM output, but also for some data sets.  
#' This is just a default Layer set available for convenience, can be easily over-ridden when defining a Source object (see \code{\link{defineSource}}).
#' @slot quantities 'Standard' quantities (as a list of \code{\linkS4class{Quantity}} objects) which might be availably from the model output or dataset.
#' @slot availableQuantities A function to determine which quantities \emph{are actually available} from the model run or dataset.
#' @slot getField A function to retrieve actually data from the model output or dataset.  This is likely to be a fairly complex function, and really depends on the specifics 
#' and idiosynchrasies of the model output format or dataset.
#' 
#' @details For DGVMTools to support a particular model or dataset, this is the object that needs to be defined.  But normally a user won't need to deal with this 
#' class since it defines model/dataset specific metadata and functions which should be defined once and then 'just work' (haha) in the future. 
#' If someone wants their model to be supported by DGVMTools then this is the object that needs to be defined correctly.
#' 
#' Note that the 'predefined.layers' and  'quantities' arguments are just default values, it is easy to add new ones.  Equally they don't all need
#' to be available for a particular run or dataset.  You can define your own for your own data format if you, for example, want to include a new model type.    
#' 
#' @name Benchmark-class
#' @rdname Benchmark-class
#' @exportClass Format
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("Benchmark", 
         slots = c(id = "character",
                   name = "character",
                   description = "character",
                   simulation = "character",
                   guess_var = "character",
                   guess_layers = "character",
                   unit = "character",
                   agg.unit = "character"),
         contains = "STAInfo",
)

