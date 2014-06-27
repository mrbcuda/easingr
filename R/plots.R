#' @title Credit easing data xyplot
#' @description Provides a convenience function for passing an \code{easing} object to \code{xyplot}.
#' @param x an object of class \code{easing} as returned by \code{\link[easingr]{getEasingData}} and its many offspring.
#' @param ... other parameters passed to \code{\link[lattice]{xyplot}}.
#' @importFrom lattice xyplot
#' @export
#' @seealso easingLineChart easingAreaChart getEasingData
#' @examples
#' \dontrun{
#' ad <- getEasingAgencyDebt()
#' xyplot(ad)
#' }
xyplot.easing <- function(x,...) {
  xyplot(x$df,main=x$main,ylab=x$ylab,xlab=NULL,...)
}


#' @title Credit easing data as an unstacked line chart.
#' @description Provides a convenience function for passing an \code{easing} object to \code{xyplot}.
#' @details Provides several assumptions about the display of the \code{easing} data to correspond to similar presentations at the Cleveland Fed's data site.
#' @param e an object of class \code{easing} as returned by \code{\link[easingr]{getEasingData}} and its many offspring.
#' @importFrom lattice xyplot
#' @importFrom lattice panel.xyplot
#' @importFrom lattice panel.grid
#' @export
#' @seealso xyplot.easing easingAreaChart getEasingData
#' @examples
#' \dontrun{
#' sd <- getEasingSummary()
#' easingLineChart(sd)
#' }
easingLineChart <- function(e) {
  stopifnot(class(e) == "easing")
  
  xyplot(e$df,
         par.settings = list(superpose.symbol = list(pch=15, col=e$colors, cex=1.2),
                             superpose.line = list(lwd=2, lty=1, col=e$colors)),
         main=e$main,
         ylab=e$ylab,
         xlab=NULL,
         superpose=TRUE,
         scales=list(y=list(limits=c(0,max(e$df)))),
         auto.key=
           list(columns=min(ncol(e$df),4),
                text=colnames(e$df),
                cex=0.8),
         panel=function(x,...){
           panel.xyplot(x,...)
           panel.grid(-1,0,...)
         }
  )
}

#' @title Credit easing data as a stacked area chart.
#' @description Provides a convenience function for passing an \code{easing} object to \code{xyplot} to render a sand (stacked area) chart.
#' @details Provides several assumptions about the display of the \code{easing} data to correspond to similar presentations at the Cleveland Fed's data site.  To implement the stacked area chart the function first computes the column-wise value accumulations, then passes these values to the \code{latticeExtra} \code{xyarea} polygon rendering tools.  Plots the columns in reverse stacking order to show the desired overlaps.
#' @param e an object of class \code{easing} as returned by \code{\link[easingr]{getEasingData}} and its many offspring.
#' @importFrom lattice xyplot
#' @importFrom latticeExtra panel.xyarea
#' @importFrom lattice panel.grid
#' @export
#' @seealso xyplot.easing easingLineChart getEasingData
#' @examples
#' \dontrun{
#' ed <- getEasingDetails()
#' easingAreaChart(ed)
#' }
easingAreaChart <- function(e) {
  stopifnot(class(e) == "easing")
  
  # we don't modify the class's list value
  df <- e$df
  
  # convert to cumulative across columns for sand chart
  if ( ncol(df) > 1 ) {
    df[is.na(df)] <- 0
    for (i in 2:ncol(df)) {
      df[,i] <- df[,i]+df[,i-1]
    }
  }
  
  # use area polygons to implement the stacked area chart
  # plot in reverse column order to show polygon layers
  xyplot(df[,ncol(df):1],
         par.settings = list(superpose.symbol = list(pch=15, col=rev(e$colors), cex=1.2),
                             superpose.polygon = list(lwd=2, lty=1, col=rev(e$colors))),
         main=paste(e$main,ifelse(ncol(df)>1,"(Stacked)","")),
         ylab=e$ylab,
         xlab=NULL,
         superpose=TRUE,
         scales=list(y=list(limits=c(0,max(df)))),
         auto.key=
           list(points=TRUE,
                lines=FALSE,
                columns=min(ncol(e$df),4),
                text=rev(colnames(df)),
                cex=0.8),
         panel=function(x,...) {
           panel.xyarea(x,origin=0,...)
           panel.grid(-1,0,...)
         }
  )
}



