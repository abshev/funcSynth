#` Function subsets data to a single list unit (lint)

lintifyData = function(u, y, t, ut, it, prepost, yt){
  if(prepost == "pre"){
    if(yt == "y"){
      y[ut == u & t < it]
    }
    else if(yt == "t"){
      t[ut == u & t < it]
    }
  }
  else if(prepost == "post"){
    if(yt == "y"){
      y[ut == u & t >= it]
      # y[ut == u]
    }
    else if(yt == "t"){
      t[ut == u & t >= it]
      # t[ut == u]
    }
  }
}