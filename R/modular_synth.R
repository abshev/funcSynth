#' Creates modularSynth object for use in funcSynth

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
