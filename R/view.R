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
#'
#' @export
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
ropls.plot <- function(d, plottype = "score", xvar, yvar, hotelling = FALSE, ellipse = FALSE, col.var = NULL){
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
    if(d@typeC == "PCA"){p <- ggplot(score, aes_string(x = xvar, y = yvar)) + geom_point(size = 2)}

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

print(p)

}

#Add scree plot per model, and whether the component is significant, as per modelDF
#Bar R2Y, Q2, R2X per component and significance
#Similarity / permutation plot > remake or just use basic plot anyways?



#Modelcompare
#if function is used without supplying a comparison > compare metrics in common between supplied variables
#if function is used with comparison > plot metrics in bar graph (ggplot)
ropls.modelcompare <- function(..., comparison = "R2Y"){
  nargs()
  x = list(...)
  x[]
}


