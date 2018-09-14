#' Saves plot to stox project location (getProjectPaths(projectname)$RReportDir) to be localised by getPlots in stox.
#' @param projectname name of stox project
#' @param plotname filename for plot without format suffix
#' @param draw function for drawing the plot. Takes no arguments
#' @param format function defining plotting device and file suffix, supports grDevices::pdf, grDevices::png, grDevices::jpeg, grDevices::tiff, grDevices::bmp
#' @param verbose logical, if TRUE info is written to stderr()
#' @param ... parameters to be passed on to format
#' @internal
formatPlot <-
  function(projectname,
           plotname,
           draw,
           format = "png",
           verbose = F,...) {
    lll <- list(...)
    
    filenamebase <-
      file.path(getProjectPaths(projectname)$RReportDir, plotname)
    filename <- paste(filenamebase, format, sep = ".")
    
    if (format %in% c("bmp", "jpeg", "png", "tiff")) {
      moveToTrash(filename)
      do.call(format, c(list(filename = filename), applyParlist(lll,
                                                                format)))
    }
    else if (format %in% c("pdf")){
      moveToTrash(filename)
      do.call(format, c(list(file = filename), applyParlist(lll,
                                                            format)))
    }
    
    tryCatch({
      if (verbose) {
        write(paste("Writing plot to:", filename), stderr())
      }
      draw()},
      finally={
        if (length(format)) {
          dev.off()
        }
      }
    )
    
  }

#
# Result plots
#

#' Constructs equal tailed credibility intervals. The probability mass above and below the interval is approximatelty the same.
#' @param age agegroups integer vector
#' @param values matrix where age indexes rows, and columns index points in the posterior distribution
#' @param alpha value for the credibility intervals
#' @return list with means, upper_ci and lower_ci all numeric vectors corresponding to age
#' @keywords internal
get_eq_tail_ci <- function(age, values, alpha){
  postm <- c()
  upper <- c()
  lower <- c()
  for (i in 1:length(age)){
    postm <- c(postm, mean(values[i,]))
    upper <- c(upper, quantile(values[i,], 1-(alpha/2.0)))
    lower <- c(lower, quantile(values[i,], (alpha/2.0)))
  }
  res <- list()
  res$means <- postm
  res$upper_ci <- upper
  res$lower_ci <- lower
  return(res)
}

#' Plots means with error bars
#' @keywords internal
plot_ci <- function(x, means, upper_ci, lower_ci, ...){
  args <- list(...)
  
  args$x=x
  args$y=upper_ci
  args$type="n"
  do.call(plot, args)
  args$y=means
  args$type="p"
  do.call(points, args)
  
  segments(x, upper_ci, x, lower_ci, lwd = 1.5, ...)
  arrows(x, upper_ci, x,
         lower_ci, lwd = 1.5, angle = 90,
         code = 3, length = 0.05, ...)
  
}

#' Plot catch by age prediction as boxplots
#' @param pred RECA prediction object as returned by eca::eca.predict
#' @param var A key string indicating the variable to plot. ´Abundance´ and ´Weight´ is implemented. 
#' @param unit A unit key string indicating the unit (see getPlottingUnit()$definitions$unlist.units for available key strings)
#' @keywords internal
plot_pred_box <- function(pred, var, unit, xlab="age", ylab=paste("posterior catch", unit), ...){
  
  if (var=="Abundance" | var=="Count"){
    plottingUnit=getPlottingUnit(unit=unit, var=var, baseunit="ones", def.out = F)
    caa <- apply(pred$TotalCount, c(2,3), sum)
  }
  else if (var=="Weight"){
    caa <- apply(pred$TotalCount, c(2,3), sum)*pred$MeanWeight
    plottingUnit=getPlottingUnit(unit=unit, var=var, baseunit="kilograms", def.out = F)
  }
  else{
    stop("Not implemented")
  }
  caa_scaled <- caa/plottingUnit$scale
  
  post <- c()
  age <- c()
  for (i in 1:length(pred$AgeCategories)){
    post <- c(post, caa_scaled[i,])
    age <- c(age, rep(pred$AgeCategories[i], length(caa_scaled[i,])))
  }
  
  args <- list(...)
  if (!("las" %in% names(args))){
    args$las=2
  }
  args$xlab=xlab
  args$ylab=ylab
  
  do.call(boxplot, c(post~age, args))

}

#' Plot equal tailed credible intervals for a catch by age prediction
#' @param pred RECA prediction object as returned by eca::eca.predict
#' @param var A key string indicating the variable to plot. ´Abundance´ and ´Weight´ is implemented. 
#' @param unit A unit key string indicating the unit (see getPlottingUnit()$definitions$unlist.units for available key strings)
#' @param alpha
#' @keywords internal
plot_catch_at_age_ci <- function(pred, var, unit, alpha=0.1, xlab="age", ylab=paste("posterior catch", unit), ...){
  if (var=="Abundance" | var=="Count"){
    plottingUnit=getPlottingUnit(unit=unit, var=var, baseunit="ones", def.out = F)
    caa <- apply(pred$TotalCount, c(2,3), sum)
  }
  else if (var=="Weight"){
    caa <- apply(pred$TotalCount, c(2,3), sum)*pred$MeanWeight
    plottingUnit=getPlottingUnit(unit=unit, var=var, baseunit="kilograms", def.out = F)
  }
  else{
    stop("Not implemented")
  }
  caa_scaled <- caa/plottingUnit$scale

  res <- get_eq_tail_ci(pred$AgeCategories, caa_scaled, alpha)

  args <- alist(...)
  if (!("las" %in% names(args))){
    args$las=1
  }
  if (!("pch" %in% names(args))){
    args$pch=19
  }
  if (!("lty" %in% names(args))){
    args$lty=1
  }
  if (!("main" %in% names(args))){
    args$main=paste("Catch at age (", var, ")", sep="")
  }
  args$xlab=xlab
  args$ylab=ylab
  
  args$x=pred$AgeCategories
  args$means <- res$means
  args$upper_ci <- res$upper_ci
  args$lower_ci <- res$lower_ci
  
  do.call(plot_ci, args)
  legend("topright", pch=c(args$pch, NA), lty=c(NA, args$lty), legend=c("mean", paste(100-alpha*100, "% interval", sep="")), bty="n")
  
}


#' Plots mean weight at age and sample range
#' @param biotic indiviuals as exported by stox
#' @param pred RECA prediction object as returned by eca::eca.predict
#' @keywords internal
plot_weight_at_age <- function(biotic, pred, unit, alpha=0.01, xlab="age", ylab=paste("ind. weight", unit), ...){
  
  plottingUnitEca=getPlottingUnit(unit=unit, var="Weight", baseunit="kilograms", def.out = F)
  plottingUnitBiotic=getPlottingUnit(unit=unit, var="Weight", baseunit="grams", def.out = F)
  
  #sample weights by age
  maxweights <- aggregate(list(weight=biotic$weight), by=list(age=biotic$age), FUN=function(x){max(x, na.rm=T)})
  minweights <- aggregate(list(weight=biotic$weight), by=list(age=biotic$age), FUN=function(x){min(x, na.rm=T)})
  maxweights$weight <- maxweights$weight/plottingUnitBiotic$scale
  minweights$weight <- minweights$weight/plottingUnitBiotic$scale
  
  #prediction mewans
  weight_scaled <- pred$MeanWeight/plottingUnitEca$scale
  res <- get_eq_tail_ci(pred$AgeCategories, weight_scaled, alpha)
  
  means <- data.frame(age=pred$AgeCategories, means=res$means)
  upper <- data.frame(age=maxweights$age, upper=maxweights$weight)
  lower <- data.frame(age=minweights$age, lower=minweights$weight)
  
  comp <- merge(means, merge(upper, lower), all.x=T)
  
  args <- alist(...)
  
  if (!("las" %in% names(args))){
    args$las=1
  }
  if (!("pch" %in% names(args))){
    args$pch=19
  }
  if (!("lty" %in% names(args))){
    args$lty="dotted"
  }
  if (!("ylim" %in% names(args))){
    args$ylim=c(min(min(lower, na.rm=T), 0), max(0,max(upper, na.rm=T)))
  }
  if (!("main" %in% names(args))){
    args$main="Weight at age"
  }
  
  args$xlab=xlab
  args$ylab=ylab
  
  args$x=comp$age
  args$means <- comp$means
  args$upper_ci <- comp$upper
  args$lower_ci <- comp$lower
  
  do.call(plot_ci, args)
  legend("topleft", pch=c(args$pch, NA), lty=c(NA, args$lty), legend=c("model mean", "sample range"), bty="n")
  
} 

#' Plots mean length at age and sample range
#' @param biotic indiviuals as exported by stox
#' @param pred RECA prediction object as returned by eca::eca.predict
#' @keywords internal
plot_length_at_age <- function(biotic, pred, xlab="age", alpha=0.01, ylab=paste("length cm"), ...){
  
  bioticscale = 1
  ecascale = 1
  
  #sample length by age
  maxlengths <- aggregate(list(length=biotic$length), by=list(age=biotic$age), FUN=function(x){max(x, na.rm=T)})
  minlengths <- aggregate(list(length=biotic$length), by=list(age=biotic$age), FUN=function(x){min(x, na.rm=T)})
  maxlengths$length <- maxlengths$length/bioticscale
  minlengths$length <- minlengths$length/bioticscale
  
  #prediction mewans
  length_scaled <- pred$MeanLength/ecascale
  res <- get_eq_tail_ci(pred$AgeCategories, length_scaled, alpha)
  
  means <- data.frame(age=pred$AgeCategories, means=res$means)
  upper <- data.frame(age=maxlengths$age, upper=maxlengths$length)
  lower <- data.frame(age=minlengths$age, lower=minlengths$length)
  
  comp <- merge(means, merge(upper, lower), all.x=T)
  
  args <- alist(...)
  
  if (!("las" %in% names(args))){
    args$las=1
  }
  if (!("pch" %in% names(args))){
    args$pch=19
  }
  if (!("lty" %in% names(args))){
    args$lty="dotted"
  }
  if (!("ylim" %in% names(args))){
    args$ylim=c(min(min(lower, na.rm=T), 0), max(0,max(upper, na.rm=T)))
  }
  if (!("main" %in% names(args))){
    args$main="Length at age"
  }
  
  args$xlab=xlab
  args$ylab=ylab
  
  args$x=comp$age
  args$means <- comp$means
  args$upper_ci <- comp$upper
  args$lower_ci <- comp$lower
  
  do.call(plot_ci, args)
  legend("topleft", pch=c(args$pch, NA), lty=c(NA, args$lty), legend=c("model mean", "sample range"), bty="n")
  
} 

#' Plots a panel of RECA results
#' @param biotic indiviuals as exported by stox
#' @param pred RECA prediction object as returned by eca::eca.predict
#' @keywords internal
plot_RECA_results_panel <- function(pred, biotic, ...){
  par.old <- par(no.readonly = T)
  par(mfrow=c(2,2))
  plot_catch_at_age_ci(pred, var="Abundance", unit="millions", ...)
  plot_catch_at_age_ci(pred, var="Weight", unit="tt", ...)
  plot_weight_at_age(biotic, pred, unit="kg", ...)
  plot_length_at_age(biotic, pred, ...)
  par(par.old)
}

#
# Diagnostics plots 
#

#' get matrix of sample and landings from the subset of biotic that contains aged individuals
#' @keywords internal
get_g_s_a_frame <- function(eca) {
  agedb <- eca$biotic[!is.na(eca$biotic$age), ]
  
  cols <- c("temporal", "gearfactor", "spatial")
  if (!all(cols %in% names(eca$covariateMatrixBiotic)) |
      !all(cols %in% names(eca$covariateMatrixLanding))) {
    stop("Covariates temporal, gearfactor and spatial needs to be defined for this plot.")
  }
  if (any(is.na(eca$landing$temporal)) |
      any(is.na(eca$landing$gearfactor)) |
      any(is.na(eca$landing$spatial)) |
      any(is.na(eca$biotic$temporal)) |
      any(is.na(eca$biotic$gearfactor)) |
      any(is.na(eca$biotic$spatial))) {
    warning("NAs in covariates")
  }
  
  totland <-
    aggregate(
      list(landed_kt = eca$landing$rundvekt / (1000 * 1000)),
      by = list(
        temporal = eca$landing$temporal,
        gearfactor = eca$landing$gearfactor,
        spatial = eca$landing$spatial
      ),
      FUN = sum
    )
  totsamp <-
    aggregate(
      list(sampled_t = agedb$catchweight / 1000),
      by = list(
        temporal = agedb$temporal,
        gearfactor = agedb$gearfactor,
        spatial = agedb$spatial
      ),
      FUN = function(x) {
        sum(x, na.rm = T)
      }
    )
  totvessel <-
    aggregate(
      list(vessels = agedb$platform),
      by = list(
        temporal = agedb$temporal,
        gearfactor = agedb$gearfactor,
        spatial = agedb$spatial
      ),
      FUN = function(x) {
        length(unique(x))
      }
    )
  tothaul <-
    aggregate(
      list(hauls = agedb$serialno),
      by = list(
        temporal = agedb$temporal,
        gearfactor = agedb$gearfactor,
        spatial = agedb$spatial
      ),
      FUN = function(x) {
        length(unique(x))
      }
    )
  totaged <-
    aggregate(
      list(aged = agedb$age),
      by = list(
        temporal = agedb$temporal,
        gearfactor = agedb$gearfactor,
        spatial = agedb$spatial
      ),
      FUN = function(x) {
        sum(!is.na(x))
      }
    )
  
  m <- merge(totland, totvessel, by = cols, all = T)
  m <- merge(totsamp, m, by = cols, all = T)
  m <- merge(m, tothaul, by = cols, all = T)
  m <- merge(m, totaged, by = cols, all = T)
  
  m$landed_kt[is.na(m$landed_kt)] <- rep(0, sum(is.na(m$landed_kt)))
  m$landed_fr <- m$landed_kt / sum(m$landed_kt)
  
  m[is.na(m)] <- 0
  return(m)
}

#' Extracts all combinations of gear, temporal and spatial from landings
#' @keywords internal
get_gta_landings <- function(stoxexport) {
  gta <- c("gearfactor", "temporal", "spatial")
  landedcol <- lapply(
    gta,
    FUN = function(x) {
      stoxexport$landing[[x]]
    }
  )
  names(landedcol) = gta
  aggland <-
    aggregate(list(landed_kt = stoxexport$landing$rundvekt),
              by = landedcol,
              FUN = sum)
  return(aggland)
}

#' show samples wrp common covariates gear, area and temporal
#' @keyword internal
plot_gear_temporal_area <-
  function(eca,
           titletext = "gear/temporal - area\nlanded (kt)\nage samples: #vessels,#catches,#individuals",
           colgood = "green4",
           colok = "green2",
           colbarely = "yellow",
           colbad = "orange",
           colempty = "gray",
           colwrong = "white") {
    require(plotrix)
    m <- get_g_s_a_frame(eca)
    m$desc <-
      paste(m$landed_kt, "\n", m$vessels, ", ", m$hauls, ",", m$aged, sep = "")
    m$sd <- paste(m$gear, m$temporal, sep = "/")
    
    landed <- xtabs(m$landed_kt ~ m$sd + m$spatial)
    vessels <- xtabs(m$vessels ~ m$sd + m$spatial)
    hauls <- xtabs(m$hauls ~ m$sd + m$spatial)
    aged <- xtabs(m$aged ~ m$sd + m$spatial)
    
    col <- landed * NA
    col[landed == 0 & vessels > 0] <- colwrong
    col[landed == 0 & vessels == 0] <- colempty
    col[landed > 0 & vessels == 0] <- colbad
    col[landed > 0 & vessels == 1 & hauls == 1] <- colbarely
    col[landed > 0 & vessels == 1 & hauls > 1] <- colok
    col[landed > 0 & vessels > 1 & hauls > 1] <- colgood
    
    descr <-
      `dim<-`(sprintf("%.0f\n%d,%d,%d", landed, vessels, hauls, aged),
              dim(landed))
    descr <-
      descr[rowSums(landed) > 0 |
              rowSums(aged) > 0, colSums(landed) > 0 | colSums(aged) > 0]
    col <-
      col[rowSums(landed) > 0 |
            rowSums(aged) > 0, colSums(landed) > 0 | colSums(aged) > 0]
    landed <-
      landed[rowSums(landed) > 0 |
               rowSums(aged) > 0, colSums(landed) > 0 | colSums(aged) > 0]
    colnames(descr) <- colnames(landed)
    rownames(descr) <- rownames(landed)
    
    #deal with sizing and such when output device is clear
    #calculate plot size
    plot.new()
    addtable2plot(
      x = "topleft",
      table = descr,
      bty = "o",
      display.rownames = TRUE,
      display.colnames = TRUE,
      hlines = TRUE,
      vlines = TRUE,
      bg = col,
      xjust = 2,
      yjust = 1,
      cex = 0.5
    )
    title(titletext)
    
    return(descr)
  }

#' Plot aggregated landings for all cells, labeled with sampling level
#' @keywords internal
plot_cell_landings <-
  function(eca,
           xlab = "Cells (gear/temp/spatial)",
           ylab = "landed (kt)",
           frac = 0.001,
           titletext = paste("top", 100 - frac * 100, "weigth-% cells"),
           legendtitle = "sample clusteredness",
           colgood = "green4",
           colok = "green2",
           colbarely = "yellow",
           colbad = "orange",
           colempty = "gray",
           colwrong = "white",
           gooddesc = "> 1 vessel",
           okdesc = "> 1 catch",
           barelydesc = "> 0 catch",
           baddesc = "0 samples") {
    mm <- get_g_s_a_frame(eca)
    mm <- mm[order(mm$landed_kt, decreasing = T), ]
    mm <- mm[mm$landed_kt / sum(mm$landed_kt) > frac, ]
    mm$col <- NA
    
    mm[mm$landed_kt == 0 & mm$vessels > 0, "col"] <- colwrong
    mm[mm$landed_kt == 0 & mm$vessels == 0 , "col"] <- colempty
    mm[mm$landed_kt > 0 &  mm$vessels == 0, "col"] <- colbad
    mm[mm$landed_kt > 0 &
         mm$vessels == 1 & mm$hauls == 1, "col"] <- colbarely
    mm[mm$landed_kt > 0 &  mm$vessels == 1 & mm$hauls > 1, "col"] <-
      colok
    mm[mm$landed_kt > 0 &  mm$vessels > 1 &
         mm$hauls > 1, "col"] <- colgood
    
    barplot(
      mm$landed_kt,
      col = mm$col,
      xlab = xlab,
      ylab = ylab,
      main = titletext,
      border = NA
    )
    legend(
      "topright",
      legend = c(gooddesc, okdesc, barelydesc, baddesc),
      fill = c(colgood, colok, colbarely, colbad),
      title = legendtitle
    )
  }

#' Plot aggregated landings for different kind of sampling level in cells
#' @keywords internal
plot_cell_coverage <-
  function(eca,
           xlab = "sample clusteredness",
           ylab = "Fraction landed (vekt-%)",
           titletext = "Coverage w age\ncells (gear/temp/spatial)",
           colgood = "green4",
           colok = "green2",
           colbarely = "yellow",
           colbad = "orange",
           colempty = "gray",
           colwrong = "white",
           gooddesc = "> 1 vessel",
           okdesc = "> 1 catch",
           barelydesc = "> 0 catch",
           baddesc = "0 samples") {
    mm <- get_g_s_a_frame(eca)
    mm <- mm[order(mm$landed_kt, decreasing = T), ]
    mm$col <- NA
    mm$desc <- NA
    
    mm[mm$landed_kt == 0 & mm$vessels > 0, "col"] <- colwrong
    mm[mm$landed_kt == 0 & mm$vessels > 0, "descr"] <- ""
    mm[mm$landed_kt == 0 & mm$vessels == 0 , "col"] <- colempty
    mm[mm$landed_kt == 0 & mm$vessels == 0 , "descr"] <- ""
    mm[mm$landed_kt > 0 &  mm$vessels == 0, "col"] <- colbad
    mm[mm$landed_kt > 0 &  mm$vessels == 0, "descr"] <- baddesc
    mm[mm$landed_kt > 0 &
         mm$vessels == 1 & mm$hauls == 1, "col"] <- colbarely
    mm[mm$landed_kt > 0 &
         mm$vessels == 1 & mm$hauls == 1, "descr"] <- barelydesc
    mm[mm$landed_kt > 0 &  mm$vessels == 1 & mm$hauls > 1, "col"] <-
      colok
    mm[mm$landed_kt > 0 &
         mm$vessels == 1 & mm$hauls > 1, "descr"] <- okdesc
    mm[mm$landed_kt > 0 &  mm$vessels > 1 &
         mm$hauls > 1, "col"] <- colgood
    mm[mm$landed_kt > 0 &
         mm$vessels > 1 &  mm$hauls > 1, "descr"] <- gooddesc
    
    rankdesc <- function(desc) {
      if (desc == gooddesc) {
        return(5)
      }
      if (desc == okdesc) {
        return(4)
      }
      if (desc == barelydesc) {
        return(3)
      }
      if (desc == baddesc) {
        return(2)
      }
      if (desc == "") {
        return(1)
      }
      else{
        stop()
      }
    }
    rankdesc <- Vectorize(rankdesc)
    
    tot <-
      aggregate(
        list(landed_fr = mm$landed_fr),
        by = list(samples = mm$col, desc = mm$descr),
        FUN = sum
      )
    tot <- tot[order(rankdesc(tot$desc)), ]
    
    barplot(
      tot$landed_fr * 100,
      col = tot$sample,
      xlab = xlab,
      ylab = ylab,
      names = tot$desc,
      main = titletext
    )
  }

#' Get aggregated landings for fixed effects
#' @keywords internal
get_fixed_effects_landings <- function(stoxexport) {
  fixed_effects <-
    stoxexport$resources$covariateInfo[stoxexport$resources$covariateInfo$covType ==
                                         "Fixed", "name"]
  landedcol <-
    lapply(
      fixed_effects,
      FUN = function(x) {
        stoxexport$landing[[x]]
      }
    )
  names(landedcol) = fixed_effects
  aggland <-
    aggregate(list(landed_kt = stoxexport$landing$rundvekt),
              by = landedcol,
              FUN = sum)
  return(aggland)
}

#' Plot table of coverage for fixed effects
#' @param stoxexport
#' @param indparameter the parameters for which data needs to be available
#' @keywords internal
plot_fixed_effect_coverage <-
  function(stoxexport,
           indparameters = c("age"),
           titletext = "Samples for fixed effects",
           okcol = "green",
           wrongcol = "white",
           undersampledcol = "red"
  ) {
    require(plotrix)
    fixed_effects <-
      stoxexport$resources$covariateInfo[stoxexport$resources$covariateInfo$covType ==
                                           "Fixed", "name"]
    if (any(is.na(stoxexport$biotic[, fixed_effects]) |
            any(is.na(stoxexport$landing[, fixed_effects])))) {
      stop("NAs for covariates")
    }
    
    aggland <- get_fixed_effects_landings(stoxexport)
    
    #discard samples without target parameters (indparameters)
    samples <- stoxexport$biotic
    for (p in indparameters) {
      samples <- samples[!is.na(samples[[p]]), ]
    }
    
    biocol <- lapply(
      fixed_effects,
      FUN = function(x) {
        samples[[x]]
      }
    )
    names(biocol) = fixed_effects
    aggsamp <-
      aggregate(
        list(catchsamples = samples$serialno),
        by = biocol,
        FUN = function(x) {
          length(unique(x))
        }
      )
    
    agg <- merge(aggsamp, aggland, by = fixed_effects, all = T)
    agg$landed_kt[is.na(agg$landed_kt)] <-
      rep(0, sum(is.na(agg$landed_kt)))
    agg$catchsamples[is.na(agg$catchsamples)] <-
      rep(0, sum(is.na(agg$catchsamples)))
    
    agg <- agg[order(agg$catchsamples), ]
    
    color <- rep(okcol, nrow(agg))
    color[agg$catchsamples == 0 & agg$landed_kt > 0] <-
      undersampledcol
    color[agg$catchsamples > 0 & agg$landed_kt == 0] <- wrongcol
    names(agg)[names(agg)=="catchsamples"] <- "catch samples"
    names(agg)[names(agg)=="landed_kt"] <- "landed kt"
    plot.new()
    addtable2plot(
      x = "topleft",
      table = agg,
      bty = "o",
      display.rownames = FALSE,
      hlines = TRUE,
      vlines = TRUE,
      bg = color,
      xjust = 2,
      yjust = 1
    )
    title(titletext)
  }

#' Paneled plots of cell coverage
#' @keywords internal
diagnosticsCoverageRECA <- function(stoxexport) {
  par.old <- par(no.readonly = T)
  par(mfrow = c(2, 1))
  plot_cell_coverage(stoxexport)
  plot_cell_landings(stoxexport)
  par(par.old)
}
#' Plot table showing coverage of gear, temporal and spatial combinations
#' @keywords internal
diagnosticsSamplesRECA <- function(stoxexport) {
  plot_gear_temporal_area(stoxexport)
}

#' Plots diagnostics for model configuration. Whether all combinations of fixed effects are sampled
#' @keywords internal
diagnostics_model_configuration <- function(stoxexport, okcol = "green",
                                            wrongcol = "white",
                                            undersampledcol = "red",
                                            oktext="OK", undersampledtext="no samples", wrongtext="no landings") {
  par.old <- par(no.readonly = T)
  par(mfrow = c(1, 2), mar=c(1, 4.1, 4.1, 2.1))
  plot_fixed_effect_coverage(stoxexport,
                             indparameters = c("age", "length"),
                             titletext = "Age samples for fixed effects", okcol=okcol, wrongcol=wrongcol, undersampledcol=undersampledcol)
  legend("bottom", fill=c(okcol, undersampledcol, wrongcol), legend=c(oktext, undersampledtext, wrongtext), bty="n", ncol=1, xpd=T)
  plot_fixed_effect_coverage(stoxexport,
                             indparameters = c("weight", "length"),
                             titletext = "Weight samples for fixed effects", okcol=okcol, wrongcol=wrongcol, undersampledcol=undersampledcol)
  par(par.old)
}