#' Creates modularSynth object for use in funcSynth()
#' 
#' @param y Numeric vector of outcome values.
#' @param time Either a numeric vector or vector of class Date, posixlt, or posixct
#' @param unit Character, numeric, or factor variable indicating unit.
#' @param intervention Binary or logical vector indicating whether an i
#'                     intervention has occurred on any unit at the corresponding
#'                     time.
#' @param treated Binary or logical variable indicating if a unit is ever 
#'                treated at any time.
#'                
#' @export

#TODO order data by increasing time
#TODO add data argument.
modularSynth = function(y, time, unit, intervention, treated){
  if(!(is.numeric(y))){
    stop("y must be a numeric vector")
  }
  if(any(is.na(as.numeric(time)))){
    stop("time must be numeric vector or coercible into numeric such as a Date object")
  }
  #check time is numeric, integer, Date, posixlt, or posixct
  #Check intervention is either logical vector or length 1 in the same type as 
  #        time
  #Check time and unit are the same length
  #Check treated is logical same length as time and unit or a subset
  #        of unit
  out <- as.matrix(cbind(y = y, time = as.numeric(time), unit = as.numeric(unit), 
                    intervention = as.numeric(intervention), treated = as.numeric(treated)))
  class(out) <- c("modularSynth", "matrix")
  
  return(out)
}

is.modularSynth = function(x){
  "modularSynth" %in% class(x)
}
