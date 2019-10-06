# Function to plot a circle in ggplot
# Adapted from https://github.com/tyrannomark/bldR/blob/master/R/L2017.R
# GPL-3 license
#' Function to add circles and ellipses to ggplot images.
#'
#' `gg_circle`` produces circles and ellipses that can be added to ggplot images.
#'
#' @param rx parameter.
#' @param ry parameter.
#' @param xc parameter.
#' @param yc parameter.
#' @param color parameter.
#' @param fill parameter.
#'
#' @return a circle.
#'   }
gg_circle <- function(rx, ry, xc, yc, color="black", fill=NA, ...) {
  x <- xc + rx*cos(seq(0, pi, length.out=100))
  ymax <- yc + ry*sin(seq(0, pi, length.out=100))
  ymin <- yc + ry*sin(seq(0, -pi, length.out=100))
  annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
}

#' Produce publication-quality ggplot images of ropls objects.
#'
#' `ropls.plot`` produces publication-quality ggplot images of ropls objects.
#' The functions automatically determines the model type ("PCA", "PLS", "OPLS","PLS-DA","OPLS-DA") from the ropls object.
#' The components shown in the plot are flexible and determined by the xvar and yvar parameters.
#'
#' @param d ropls object created by [opls()].
#' @param plottype Which type of plot to produce. Options include scores (default), loadings and metrics.
#' @param xvar which component to plot on the x-axis. Must refer to a single component ("p1") or orthogonal ("o1") component present in the supplied ropls object.
#' @param yvar which component to plot on the y-axis. Must refer to a single component ("p1") or orthogonal ("o1") component present in the supplied ropls object.
#' @param hotelling If set to TRUE, will add Hotelling's T2 ellipse to the plot.
#' @param ellipse If set to TRUE, will add an ellipse around the groups supplied in the model. Does not work for PCA, PLS and OPLS models.
#' @param col.var Vector of categorical or numerical values used to colour the variables of the loading plot.
#'
#' @return A ggplot object of the specified type.
#'   }
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
ropls.plot <- function(d, plottype = "score", xvar, yvar, hotelling = FALSE, ellipse = FALSE, col.var = NULL, col.pca = NULL){
  N <- nrow(d@scoreMN)
  sm <- d@modelDF
  y <- d@suppLs$yMCN
  hotFisN <- (N - 1) * 2 * (N^2 - 1) / (N^2 * (N - 2)) * qf(0.95, 2, N - 2)

  #Define scores
  if(length(d@orthoScoreMN) == length(d@scoreMN)){
    score = data.frame(d@scoreMN, d@orthoScoreMN)}
  else{score = data.frame(d@scoreMN)}

  #Define loadings
  if(length(d@orthoLoadingMN) == length(d@loadingMN)){
    loading = data.frame(d@loadingMN, d@orthoLoadingMN)}
  else{loading = data.frame(d@loadingMN)}

  #plotting scores
  if(plottype == "score"){
    if(d@typeC == "PCA"){p <- ggplot(score, aes_string(x = xvar, y = yvar, col = col.pca)) + geom_point(size = 2)}

    else{p <- ggplot(score, aes_string(x = xvar, y = yvar, col = "y")) + geom_point(size = 2)}

    p <- p + xlab(paste(xvar,":", sm[xvar, "R2X"] * 100, "%")) + ylab(paste(yvar,":", sm[yvar, "R2X"] * 100, "%"))

    if(hotelling){
    p <- p + gg_circle(
      rx = sqrt(as.numeric(var(score %>% select(xvar))) * hotFisN),
      ry = sqrt(as.numeric(var(score %>% select(yvar))) * hotFisN),
      xc = 0, yc = 0)}

    if (ellipse) {
    p <- p + stat_ellipse(
      geom = "polygon", alpha = 0.3, linetype = "blank",
      aes_string(fill = "y"), type = "norm")}
  }

  #plotting loadings
  if(plottype == "loading"){
    p <- ggplot(loading, aes_string(x = xvar, y = yvar, col = col.var)) + geom_point(size = 2)
    p <- p + xlab(paste(xvar,":", sm[xvar, "R2X"] * 100, "%")) + ylab(paste(yvar,":", sm[yvar, "R2X"] * 100, "%"))
    p <- p + geom_hline(yintercept = 0, color = "gray") + geom_vline(xintercept = 0, color = "gray")
    }

return(p)
}


#' Perform FGSEA enrichment.
#'
#' `ropls.enrich`` performs FGSEA enrichment on a dataframe containing variables, variable information and variable scores.
#' The input of the function is a dataframe with a variable column, one or multiple columns containing variable information and scores.
#' The variable column may contain non-unique names, but will be filtered for distinct entries.
#' The variable information may be one or multiple columns, and can be selected using the filterset variable.
#'
#' @param var data frame containing a variable column, at least one variable information column, and a score column (e.g. p-value, fold-change, OPLS-DA loading).
#' @param var.name The name of the variable column in quotes, must match the variable column name exactly (e.g. "Precursor.Ion.Name").
#' @param value.name The name of the value column in quotes, must match the value column name exactly (e.g. "p-value", "fc" or "p1").
#' @param filterset If FALSE (default) all sets are used for enrichment, or could be used to use a particular set, matching the name of a variable information column (e.g. "class", "cl", "uns")
#'
#' @return A fgsea enrichment result.
#'   }
#'
#' @import dplyr
#'
#' @export
ropls.enrich <- function(var, var.name, value.name, filterset = FALSE){
  var.arrange = var %>% dplyr::distinct(!!sym(var.name), .keep_all = TRUE) %>%
    arrange(-!!sym(value.name))

  fgsea.test = var.arrange %>%
    select(!!sym(var.name), !!sym(value.name)) %>%
    tibble::deframe()

  fgsea.set = var.arrange %>%
    select(-!!sym(value.name)) %>%
    gather(key = "collection", value = "value", (var %>% select(-!!sym(var.name), -!!sym(value.name)) %>% colnames)) %>%
    unite("set", collection, value, sep = "_")

  if(filterset != FALSE){
  fgsea.set = fgsea.set %>% filter(grepl(filterset,.$set))
  }

  fgsea.set = split(fgsea.set %>% select(!!sym(var.name)) %>% as.matrix %>% as.character(), fgsea.set$set)

  fgseaRes <- fgsea(pathways = fgsea.set, stats = fgsea.test, minSize=5, maxSize=500, nperm=10000)

  return(fgseaRes)
}



#' Compare ropls models
#'
#' `ropls.modelcompare`` compares ropls models of the same type
#'
#' @param ... models
#'
#' @return A comparison
#'   }
#'
#' @import dplyr
#'
#' @export
ropls.modelcompare <- function(...){
  arguments <- list(...)
paste(arguments[1])
}

ropls.modelcompare(ropls.opls, ropls.oplsda)
ropls.modelcompare("ropls.opls", "ropls.oplsda")

