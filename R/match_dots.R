#' Internal function to handle the ... argument of funcSynth() appropariately.
#' 
#' @param dots additional arguments for optim() and ipop() from funcSynth call

matchDots = function(dots){
  if(length(dots)){
    optimOptionNames <- names(formals(stats::optim))
    ipopOptionNames <- names(formals(kernlab::ipop))
    optimOptionsIndex <- pmatch(names(dots), optimOptionNames, nomatch = 0L)
    ipopOptionsIndex <- pmatch(names(dots), ipopOptionNames, nomatch = 0L)
    if(any((optimOptionsIndex * ipopOptionsIndex) == 0L)){
      stop(gettextf("Argument %s not a valid option for optim or ipop", 
                    names(dots)[(optimOptionsIndex * ipopOptionsIndex) == 0L]),
           domain = NA)
    }
    if(any(optimOptionsIndex != 0L)){
      optimOptions = dots[optimOptionsIndex != 0L]
    }
    else{
      optimOptions = NULL
    }
    if(any(ipopOptionsIndex != 0L)){
      ipopOptions = dots[ipopOptionsIndex != 0L]
    }
    else{
      ipopOptions = NULL
    }
  }
  else{
    optimOptions = NULL
    ipopOptions = NULL
  }
  return(list(optimOptions = optimOptions, ipopOptions = ipopOptions))
}
