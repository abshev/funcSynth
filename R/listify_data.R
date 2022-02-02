#` Converts data frame into list format needed for funkySynth()

listifyData = function(y, t, ut, it){
  ylistPre = lapply(unique(ut), lintifyData, y = y, t = t, ut = ut,
                    it = it, prepost = "pre", yt = "y")
  ylistPost = lapply(unique(ut), lintifyData, y = y, t = t, ut = ut,
                     it = it, prepost = "post", yt = "y")
  tlistPre = lapply(unique(ut), lintifyData, y = y, t = t, ut = ut,
                    it = it, prepost = "pre", yt = "t")
  tlistPost = lapply(unique(ut), lintifyData, y = y, t = t, ut = ut,
                     it = it, prepost = "post", yt = "t")
  return(list(ylistPre = ylistPre, ylistPost = ylistPost,
              tlistPre = tlistPre, tlistPost = tlistPost))
}