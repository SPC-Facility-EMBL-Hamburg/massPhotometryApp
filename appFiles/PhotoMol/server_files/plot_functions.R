configFig <- function(fig,plot_width, plot_height, plot_type,plot_title_for_download) {
 
  fig %>%  config(
    toImageButtonOptions = list(
      format = plot_type,
      filename = plot_title_for_download,
      width = plot_width * 50,
      height = plot_height * 50
    ), displaylogo = FALSE,
    modeBarButtonsToRemove = list(
      'sendDataToCloud',
      'hoverClosestCartesian',
      'hoverCompareCartesian',
      'lasso2d',
      'select2d'))
   
}

add_mass_histogram <- function(fig,dfMass,bin_info,normalize,hex_color) {

  histnorm <- ifelse(normalize,"probability","count")

  xbins_lst <- list(start=bin_info$start, end=bin_info$end, size=bin_info$size)

  fig <- fig %>% add_trace(type="histogram",x=dfMass$mass, color = I(hex_color),alpha = 0.5, xbins = xbins_lst, histnorm = histnorm,showlegend=FALSE)

  return(fig)

}

add_gaussian_sum_trace <- function(
  fig,refeynFit,baseline,scaling_factor,
  color,legend,showlegend,linewidth) {

  gaussianSum <- refeynFit[,c(1,ncol(refeynFit))] %>% filter(y > baseline+0.05 | y == 0)
  gaussianSum$y <- gaussianSum$y * scaling_factor

  fig <- fig %>% add_trace(data=gaussianSum,color=I(color),x=~x,y=~y,
                           type = 'scatter', mode = 'lines',
                           showlegend=showlegend,
                           name=legend,
                           line=list(width = linewidth))

  return(fig)

}

add_labels_to_fig <- function(fig,refeynFitTable,contrasts,scaling_factor,axis_size,sels,stacked,mass=TRUE,counts=TRUE) {

  fitted_means       <- refeynFitTable[,1]
  fitted_counts_perc <- refeynFitTable[,4]

  if (contrasts) fitted_means <- fitted_means * cstFactorForContrast
  fitted_amp   <- refeynFitTable[,5] * scaling_factor

  tf <- list(family = "Roboto", size = axis_size-2, color = toRGB("black"))

  yShift <- max(fitted_amp)*0.02

  labels <- rep("",length(fitted_means))

  if (mass) {
    labels <- paste0(" ",round(fitted_means))
    if (!contrasts) labels <- paste0(labels," kDa")
  }

  if (counts) {
    labels <- paste0(labels," (",round(fitted_counts_perc)," % counts )")
  }

  dfLabel <- data.frame("means"=fitted_means,"amplitudes"=fitted_amp+yShift,"labels"=labels)

  dfLabel <- dfLabel[sels,]

  fig <- fig %>% add_text(
    data = dfLabel, x = ~means, y = ~amplitudes, text = ~labels,
    textfont = tf, textposition = "top right", showlegend=FALSE)

  if (stacked) {
    fig <- fig %>% layout(yaxis = list(range=c(0,max(dfLabel$amplitudes)*1.3)))
  }

  return(fig)

}

add_masses_to_legend <- function(legends,refeynFitTable,contrasts) {

  fitted_means <- refeynFitTable[,1]

  if (contrasts) fitted_means <- fitted_means * cstFactorForContrast

  legends <- paste0(legends," : ",round(fitted_means))
  if (!contrasts) legends <- paste0(legends," kDa")

  return(legends)
}

add_percentages_to_legend <- function(legends,refeynFitTable) {

  fitted_counts_perc <- refeynFitTable[,4]

  legends <- paste0(legends," (",round(fitted_counts_perc)," % counts )")

  return(legends)
}

add_gaussian_traces <- function(
  fig,axis_size,refeynFit,refeynFitTable,legends,
  colorPalette,sels,leg_sels,baseline,scaling_factor,
  addMassesToLegend=TRUE,addPercentageToLegend=TRUE,
  contrasts=FALSE,add_labels=TRUE,add_percentages=TRUE,stacked=TRUE,
  linewidth=3) {

  if (contrasts) refeynFit$x <- refeynFit$x * cstFactorForContrast

  if (ncol(refeynFit) > 3) {

      if (sels[1]) fig <- add_gaussian_sum_trace(
        fig,refeynFit,baseline,scaling_factor,
        colorPalette[1],legends[1],leg_sels[1],linewidth)

      sels         <- sels[-1]
      leg_sels     <- leg_sels[-1]

      proceed <- sum(sels) > 0
      if (proceed) {
        legends      <- legends[-1]
        colorPalette <- colorPalette[-1]
      } else {
        return(fig)
      }
  }

  if (add_labels || add_percentages)   fig   <- add_labels_to_fig(fig,refeynFitTable,contrasts,scaling_factor,axis_size,sels,stacked,add_labels,add_percentages)

  if (addMassesToLegend) legends     <- add_masses_to_legend(legends,refeynFitTable,contrasts)
  if (addPercentageToLegend) legends <- add_percentages_to_legend(legends,refeynFitTable)

  gaussianInd <- refeynFit[,-ncol(refeynFit)]
  colnames(gaussianInd) <- c("x",legends)

  gaussianInd <- reshape2::melt(gaussianInd,id.vars="x") %>% filter(value > baseline+0.05)
  gaussianInd$variable <- as.factor(gaussianInd$variable)

  counterColorPalette <- 1

  for (var in unique(gaussianInd$variable)) {

    if (sels[counterColorPalette]) {

      hexColor  <- colorPalette[counterColorPalette]
      tempDf    <- gaussianInd[gaussianInd$variable == var,]

      tempDf$value <- tempDf$value * scaling_factor

      fig    <- fig %>% add_trace(
        data=tempDf,x=~x,y=~value,
        color=I(hexColor),type = 'scatter', mode = 'lines',
        showlegend=leg_sels[counterColorPalette],
        name=var,line = list(width = linewidth))
    }

    counterColorPalette <- counterColorPalette + 1

  }

  return(fig)

}

plotRefeynFit <- function(
  photoMolModels,
  plot_config,
  legends_config,
  colorsHist,
  contrasts=FALSE,
  normalize=FALSE,
  stacked=FALSE) {

  legendsAll <- legends_config$legends
  colorPaletteAll <- legends_config$colors
  selsAll <- legends_config$sels
  selLegAll <- legends_config$leg_sels

  plot_width  <- plot_config$width
  plot_height <- plot_config$height
  plot_type   <- plot_config$type
  axis_size   <- plot_config$axis_size
  legend_font_size <- plot_config$legend_font_size

  addMassesToLegend <- plot_config$addMassesToLegend
  addPercentageToLegend <- plot_config$addPercentageToLegend
  add_labels <- plot_config$add_labels
  add_percentages <- plot_config$add_percentages

  show_grid_x <- plot_config$show_grid_x
  show_grid_y <- plot_config$show_grid_y

  show_axis_lines <- plot_config$show_axis_lines

  tickwidth <- plot_config$tickwidth
  ticklen   <- plot_config$ticklen

  linewidth <- plot_config$linewidth

  yLabel <- ifelse(normalize,"Normalised counts","Counts")

  y <- list(
    title = yLabel,
    titlefont = list(size = axis_size),
    tickfont = list(size = axis_size),
    showgrid= show_grid_y,
    zeroline = FALSE,
    showline = show_axis_lines,
    automargin = TRUE,
    ticks = "outside",
    tickwidth = tickwidth,
    ticklen = ticklen
  )

  xtitle <- "Mass (kDa)"
  if (contrasts) xtitle <- "Ratiometric contrast * 10³"

  x <- list(
    title = xtitle,
    titlefont = list(size = axis_size),
    tickfont = list(size = axis_size),
    showgrid = show_grid_x,
    zeroline = FALSE,
    showline = show_axis_lines,
    automargin = TRUE,
    ticks = "outside",
    tickwidth = tickwidth,
    ticklen = ticklen
  )

  if (stacked) {
    figs <- list()
  } else {
    fig <- plot_ly()
  }

  id_start  <- 1
  model_cnt <- 0

  for (refeyn in photoMolModels) {

    model_cnt <- model_cnt + 1

    if (stacked) {
      figs[[model_cnt]] <- plot_ly()
    }

    dfMass <- get_df_mass(refeyn,contrasts)

    scaling_factor <- ifelse(normalize,1/nrow(dfMass),1)

    baseline       <- refeyn$baseline * scaling_factor

    bin_info <- get_bin_info(refeyn,contrasts)

    color_hst <- colorsHist[model_cnt]

    if (stacked) {
        figs[[model_cnt]] <- add_mass_histogram(figs[[model_cnt]],dfMass,bin_info,normalize,color_hst)
        figs[[model_cnt]] <- figs[[model_cnt]] %>% layout(xaxis = x, yaxis = y,showlegend = TRUE,font="Roboto",legend = list(font = list(size = legend_font_size)))

    } else {
        fig <- add_mass_histogram(fig,dfMass,bin_info,normalize,color_hst)
    }

    if (is.null(refeyn$fitted_data)) next

    refeynFit  <- as.data.frame(refeyn$fitted_data)
    prevNames  <- colnames(refeynFit)[-ncol(refeynFit)]

    colnames(refeynFit) <- c("x",prevNames[-1],"y")

    id_end <- ifelse(ncol(refeynFit) <= 3,id_start, id_start + ncol(refeynFit) - 2)

    colorPalette <- colorPaletteAll[id_start:id_end]
    legends      <- legendsAll[id_start:id_end]
    sels         <- selsAll[id_start:id_end]
    sels_leg     <- selLegAll[id_start:id_end] # To select which legends to show in the plot

    id_start     <- id_end + 1

    if (sum(sels) == 0) next

    fit_table <- refeyn$fit_table
    # convert to R data frame
    fit_table <- pandas_to_r(fit_table)

    if (stacked) {

      figs[[model_cnt]] <- add_gaussian_traces(
        figs[[model_cnt]],axis_size,refeynFit,fit_table,
        legends,colorPalette,sels,sels_leg,baseline,scaling_factor,
        addMassesToLegend,addPercentageToLegend,contrasts,
        add_labels,add_percentages,stacked,linewidth)

    } else {
      fig <- add_gaussian_traces(
        fig,axis_size,refeynFit,fit_table,
        legends,colorPalette,sels,sels_leg,baseline,
        scaling_factor,addMassesToLegend,addPercentageToLegend,contrasts,
        add_labels,add_percentages,stacked,linewidth)
    }

    # refeynFit is a matrix with n columns whose 1st column is the x axis,
    # the 2nd to (n-1) column are the predicted values using the individual gaussians
    # the n column is the predicted value from the gaussian sum

  }

  if (!stacked) {
    fig <- fig %>% layout(barmode = "overlay") %>%
    layout(xaxis = x, yaxis = y,showlegend = TRUE,font="Roboto",
           legend = list(font = list(size = legend_font_size)))
  } else {
    fig <- subplot(figs,nrows = length(figs), shareY = TRUE, shareX = TRUE)
  }

  # Font size 12 requires x -0.1 and left margin 60
  # Font size 20 requires x -0.2 and left margin 100

  #x_offset_paper <- -0.11*(axis_size/12)
  #left_margin    <- 62*(axis_size/12)

  #if (stacked) {

    # Include centered annotation for the y axis title
  #  fig <- fig %>% layout(annotations = list(
  #    x = x_offset_paper,
  #    y = 0.5,
  #    text = yLabel,
  #    showarrow = FALSE,
  #    xref = "paper",
  #    yref = "paper",
  #    textangle = -90,
  #    font = list(size = axis_size)
  #  ))

    # Increase left margin to allow the y axis title to be fully visible
  #  fig <- fig %>% layout(margin = list(l = left_margin))
  #}

  fig <- configFig(fig,plot_width, plot_height, plot_type,paste0("MP_plot-",Sys.Date()))
  
  return(fig)
  
}

truncatedGaussian <- function(x,mean,std,amplitude,leftBound) {
  return( (x >= leftBound) * (amplitude * exp( - (x-mean)**2 / (2 * (std**2) ))))
}

addSimulation2plotRefeynFit <- function(fig,mean,std,amplitude,leftBound) {
  
  xSeq <- seq(max(leftBound,mean-std*2.5),mean+std*2.5)
  y    <- truncatedGaussian(xSeq,mean,std,amplitude,leftBound)
  
  df   <- data.frame(xSeq,y)
  
  fig %>% add_trace(data=df,color=I("black"),x=~xSeq,y=~y,
                           type = 'scatter', mode = 'lines',
                           name="Simulation",list(width = 3))
  
}

plotMass_vs_contrast <- function(
  mass,contrast,slope,intercept,
  plot_config) {

  plot_width  <- plot_config$width
  plot_height <- plot_config$height
  plot_type   <- plot_config$type
  axis_size   <- plot_config$axis_size
  legend_font_size <- plot_config$legend_font_size
  linewidth   <- plot_config$linewidth

  show_grid_x <- plot_config$show_grid_x
  show_grid_y <- plot_config$show_grid_y
  show_axis_lines <- plot_config$show_axis_lines

  tickwidth <- plot_config$tickwidth
  ticklen   <- plot_config$ticklen
  
  fig <- plot_ly()

  df <- data.frame(mass,contrast)

  fig    <- fig %>% add_trace(
    data=df,x=~mass,y=~contrast,
    color=I("#377EB8"),
    type = 'scatter', mode = 'markers'
  )
  
  x1 <- min(mass) 
  y1 <- x1*slope + intercept
  x2 <- max(mass) 
  y2 <- x2*slope + intercept
  
  dfPred <- data.frame(x=c(x1,x2),y=c(y1,y2))

  fig    <- fig %>% add_trace(
    data=dfPred,x=~x,y=~y,
    color=I("#ffa500"),
    type = 'scatter', mode = 'lines',
    list(width = linewidth)
  )
  
  y <- list(
    title = "Ratiometric contrast",
    titlefont = list(size = axis_size),
    tickfont = list(size = axis_size),
    showgrid = show_grid_y,
    zeroline = FALSE,
    showline = show_axis_lines,
    automargin = TRUE,
    ticks = "outside",
    tickwidth = tickwidth,
    ticklen = ticklen
  )
  
  x <- list(
    title = "Mass (kDa)",
    titlefont = list(size = axis_size),
    tickfont = list(size = axis_size),
    showgrid = show_grid_x,
    zeroline = FALSE,
    showline = show_axis_lines,
    automargin = TRUE,
    ticks = "outside",
    tickwidth = tickwidth,
    ticklen = ticklen
  )
  
  fig <- fig %>% layout(
    xaxis = x, yaxis = y,
    showlegend = FALSE,
    font="Roboto",
    legend = list(font = list(size = legend_font_size)))

  fig <- configFig(
    fig,plot_width, plot_height, plot_type,
    paste0("calibrationMassVsContrast-",Sys.Date())
  )

  return(fig)
  
}

plotRefeynMassHist <- function(
  models,
  histogram_palette,
  plot_config,
  contrasts=FALSE) {

  plot_width  <- plot_config$width
  plot_height <- plot_config$height
  plot_type   <- plot_config$type
  axis_size   <- plot_config$axis_size
  legend_font_size <- plot_config$legend_font_size

  show_grid_x <- plot_config$show_grid_x
  show_grid_y <- plot_config$show_grid_y

  show_axis_lines <- plot_config$show_axis_lines

  tickwidth <- plot_config$tickwidth
  ticklen   <- plot_config$ticklen

  fig     <- plot_ly()
  model_cnt <- 0

  for (refeyn in models) {

    model_cnt <- model_cnt + 1

    dfMass <- get_df_mass(refeyn,contrasts,TRUE)

    scale_factor <- ifelse(contrasts,cstFactorForContrast,1)

    fig <- fig %>% add_trace(type="histogram",
      x=dfMass$mass, color = I(histogram_palette[model_cnt]),
      alpha = 0.4, xbins=list(size=refeyn$bin_width*scale_factor),
      showlegend=FALSE)

  }

  y <- list(
    title = "Counts",
    titlefont = list(size = axis_size),
    tickfont = list(size = axis_size),
    standoff = 30,
    showgrid= show_grid_y,
    zeroline = FALSE,
    showline = show_axis_lines,
    automargin = TRUE,
    ticks = "outside",
    tickwidth = tickwidth,
    ticklen = ticklen
  )
  
  xtitle <- ifelse(contrasts,"Contrast * 10³","Mass (kDa)")

  x <- list(
    title = xtitle,
    titlefont = list(size = axis_size),
    tickfont = list(size = axis_size),
    showgrid = show_grid_x,
    zeroline = FALSE,
    showline = show_axis_lines,
    automargin = TRUE,
    ticks = "outside",
    tickwidth = tickwidth,
    ticklen = ticklen
  )
  
  fig <- fig %>% layout(xaxis = x, yaxis = y,showlegend = TRUE,
                        font="Roboto",legend = list(font = list(size = legend_font_size)))

  fig <- fig %>% layout(barmode = "overlay")

  fig <- configFig(fig,plot_width, plot_height, plot_type,paste0("MP_Plot_Binding_Unbinding-",Sys.Date()))
  
  return(fig)
  
}

