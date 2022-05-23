

# library(kernlab);library(fdapace); data("cigsales"); test <- funcSynth(modularSynth(y = cigsale, time =year, unit = fips, intervention = intervention, treated = treated)~retprice, data = cigsales)


library(ggplot2)


plot.funcSynth <- function(object,...){

  if(!inherits(object, "funcSynth")) stop( "not a funcSynth object")
  
  objectCall <- getCall(object)

  time <- objectCall[["formula"]][[2]][["time"]]
  y <- objectCall[["formula"]][[2]][["y"]]
  unit <- objectCall[["formula"]][[2]][["unit"]]
  intervention <- objectCall[["formula"]][[2]][["intervention"]]
  treated <-  objectCall[["formula"]][[2]][["treated"]]

  intBegin <- min(get(as.character(time), object$data)[as.logical(get(as.character(intervention), object$data))])

  toPlot <- object$data
  toPlot$.treated_color <- factor(as.logical(with(toPlot, treated)), levels = c(TRUE, FALSE), labels = c("Treated", "Donor Pool"))
  synthPlot <- toPlot[which(toPlot$treated), ]
  synthPlot$.treated_color <- "Func.Synth.Con."
  synthPlot[[y]] <- object$syntheticControl
  
  ci <- confint(object)
  
  wholePlot <-
  
  ggplot(toPlot, aes_(y = y, x = time, group = unit, color = ~.treated_color))+
    geom_line(data = toPlot[-which(toPlot$treated), ], alpha = .5, size = 1)+
    geom_line(data = toPlot[which(toPlot$treated), ], size = 1)+
    geom_vline(xintercept = intBegin, linetype = 2)+
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
    cbind(object$data[which(object$data[[treated]]) , c(as.character(time), as.character(y))],
          ci$gap,
          object["functionalTreated"]),
    {  
      .syntheticControl <- object$syntheticControl
      .functionalTreated <- object$functionalTreated
      .diff <- .functionalTreated - .syntheticControl
    })
  
  gapPlot <- 
  ggplot(toGapPlot, aes_(y = ~.diff, x = time, ymax = ~CIupper.gp, ymin = ~CIlower.gp))+
    geom_line()+
    geom_ribbon(fill = NA, color = "grey")  +
    geom_vline(xintercept = intBegin, linetype = 2)+
    scale_y_continuous(paste("Difference: Functional", y, "- synthetic control"))+
    scale_x_continuous(paste("Time:",time), expand = expansion(0,0))+
    theme_bw(base_size = 16)+
    theme(panel.border = element_blank())
  
  plot(wholePlot)
  plot(gapPlot)
  
  return(invisible(list(wholePlot = wholePlot, gapPlot = gapPlot)))
  
}


plot(test)


