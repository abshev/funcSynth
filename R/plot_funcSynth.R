


#' @importFrom ggplot2 ggplot

# library(kernlab);library(fdapace); data("cigsales"); test <- funcSynth(modularSynth(y = cigsale, time =year, unit = fips, intervention = intervention, treated = treated)~retprice, data = cigsales)


plot.funcSynth <- function(object,...){

  if(!inherits(object, "funcSynth")) stop( "not a funcSynth object")
  
  objectCall <- getCall(object)

  time <- objectCall[["formula"]][[2]][["time"]]
  y <- objectCall[["formula"]][[2]][["y"]]
  unit <- objectCall[["formula"]][[2]][["unit"]]
  intervention <- objectCall[["formula"]][[2]][["intervention"]]
  trt <-  objectCall[["formula"]][[2]][["treated"]]

  intBegin <- min(get(as.character(time), object$data)[as.logical(get(as.character(intervention), object$data))])

 
  toPlot <- as.data.frame(object$data)
  toPlot$.treated_color <- factor(as.logical(toPlot[[trt]]), levels = c(TRUE, FALSE), labels = c("Treated", "Donor Pool"))
  synthPlot <- toPlot[which(toPlot[[trt]]), ]
  synthPlot$.treated_color <- "Func.Synth.Con."
  synthPlot[[y]] <- object$syntheticControl
  
  ci <- confint(object)
  
  y_rng <- range(c(toPlot[[y]], toPlot$CIlower.sc, toPlot$CIupper.sc ) )
  
  wholePlot <-
  ggplot(toPlot, aes_(y = y, x = time, group = unit, color = ~.treated_color))+
    geom_line(data = toPlot[-which(toPlot[[trt]]), ], alpha = .5, size = 1)+
    geom_line(data = toPlot[which(toPlot[[trt]]), ], size = 1)+
    geom_vline(xintercept = intBegin, linetype = 2)+
    annotate("text", y = y_rng[2], x = intBegin, label = " - Intervention", vjust = "bottom", hjust = "outward")+
    geom_line(data = synthPlot, size = 1)+
    geom_ribbon(data = cbind(synthPlot, ci$syntheticControl), aes(ymin = CIlower.sc, ymax = CIupper.sc), fill = "blue", color = NA, alpha = .5)+
    scale_color_manual(NULL,values =  c("Treated" = "red", 
                                        "Donor Pool" = "grey30",
                                        "Func.Synth.Con." = "blue"))+
    scale_y_continuous(paste("Outcome:", y))+
    scale_x_continuous(paste("Time:",time), expand = expansion(0,0))+
    theme_bw(base_size = 16)+
    theme(panel.border = element_blank())
  
  toGapPlot <- within(
    cbind(object$data[which(object$data[[trt]]) , c(as.character(time), as.character(y))],
          ci$gap,
          object["functionalTreated"]),
    {  
      .syntheticControl <- object$syntheticControl
      .functionalTreated <- object$functionalTreated
      .diff <- .functionalTreated - .syntheticControl
    })
  
  dif_rng <- range(c(toGapPlot$CIupper.gp, toGapPlot$CIlower.gp)   )
  
  gapPlot <- 
  ggplot(toGapPlot, aes_(y = ~.diff, x = time, ymax = ~CIupper.gp, ymin = ~CIlower.gp))+
    geom_line()+
    geom_ribbon(fill = NA, color = "grey")  +
    geom_vline(xintercept = intBegin, lty = 2)+
    annotate("text", y = dif_rng[2], x = intBegin, label = " - Intervention", vjust = "bottom", hjust = "outward")+
    scale_y_continuous(paste("Functional", y, "- synthetic control"))+
    scale_x_continuous(paste("Time:",time), expand = expansion(0,0))+
    theme_bw(base_size = 16)+
    theme(panel.border = element_blank())
  
  plot(wholePlot)
  plot(gapPlot)
  
  return(invisible(list(wholePlot = wholePlot, gapPlot = gapPlot)))
  
}



#plot(test)


