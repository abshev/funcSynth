#' Function subsets data to a single list unit or "lint".

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
