

matchDots = function(dots){
  if(length(dots)){
    fpcaOptionNames <- names(formals(fdapace::setOptions))
    ipopOptionNames <- names(formals(kernlab::ipop))
    fpcaOptionsIndex <- pmatch(names(dots), fpcaOptionNames, nomatch = 0L)
    ipopOptionsIndex <- pmatch(names(dots), ipopOptionNames, nomatch = 0L)
    if(any((fpaceOptionsIndex * ipopOptionsIndex) == 0L)){
      stop(gettextf("Argument %s not a valid option for FPCA or ipop", 
                    names(dots)[(fpaceOptionsIndex * ipopOptionsIndex) == 0L]),
           domain = NA)
    }
    if(any(fpcaOptionsIndex != 0L)){
      fpcaOptions = matchedDots[fpcaOptionsIndex != 0L]
    }
    else{
      fpcaOptions = NULL
    }
    if(any(ipopOptionsIndex != 0L)){
      ipopOptions = matchedDots[ipopOptionsIndex != 0L]
    }
    else{
      ipopOptions = NULL
    }
  }
  else{
    fpcaOptions = NULL
    ipopOptions = NULL
  }
  return(list(fpcaOptions = fpcaOptions, ipopOptions = ipopOptions))
}
