#' Converts modular synth object into list format needed for FPCA()

listifyData = function(mSynth){
  ylistPre = lapply(unique(mSynth[,3]), lintifyData, y = mSynth[,1], 
                    t = mSynth[,2], ut = mSynth[,3], it = mSynth[,4], 
                    prepost = "pre", yt = "y")
  ylistPost = lapply(unique(mSynth[,3]), lintifyData, y = mSynth[,1], 
                     t = mSynth[,2], ut = mSynth[,3], it = mSynth[,4], 
                     prepost = "post", yt = "y")
  tlistPre = lapply(unique(mSynth[,3]), lintifyData, y = mSynth[,1], 
                    t = mSynth[,2], ut = mSynth[,3], it = mSynth[,4], 
                    prepost = "pre", yt = "t")
  tlistPost = lapply(unique(mSynth[,3]), lintifyData, y = mSynth[,1], 
                     t = mSynth[,2], ut = mSynth[,3], it = mSynth[,4], 
                     prepost = "post", yt = "t")
  return(list(ylistPre = ylistPre, ylistPost = ylistPost,
              tlistPre = tlistPre, tlistPost = tlistPost))
}
