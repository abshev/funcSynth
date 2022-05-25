#' Function subsets data to a single list unit or "lint".
#' 
#' @param u unit to subset
#' @param y outcome variable
#' @param t time variable
#' @param ut unit variable
#' @param it intervention variable
#' @param prepost pre or post intervention
#' @param yt selects where to make a list unit for outcome or time


lintifyData = function(u, y, t, ut, it, prepost, yt){
  if(prepost == "pre"){
    if(yt == "y"){
      y[ut == u & it == 0]
    }
    else if(yt == "t"){
      t[ut == u & it == 0]
    }
  }
  else if(prepost == "post"){
    if(yt == "y"){
      y[ut == u & it == 1]
      # y[ut == u]
    }
    else if(yt == "t"){
      t[ut == u & it == 1]
      # t[ut == u]
    }
  }
}
