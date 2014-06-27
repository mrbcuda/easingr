require(utils)
require(xts)
require(lattice)
require(latticeExtra)

# summary view, millions of dollars
#cfurl <- "http://www.clevelandfed.org/research/data/credit_easing/chart_csv.cfm?id=1&start=012007&end=072014"

# detail view, millions of dollars

endDate = "122100" # bogus end date beyond life
mainY = "(Millions of Dollars)"

# ---- SUMMARY
fedColors = c("#ce9257", "#d9d28b", "#9a9a9a", "#397093", "#aa947c")
mainTitle = "Credit Easing Policy Tools: Summary"

#' @references \link{http://www.clevelandfed.org/research/data/credit_easing/index.cfm}
cfurl <- paste("http://www.clevelandfed.org/research/data/credit_easing/chart_csv.cfm?id=1&start=012007&end=",endDate,sep='')
df <- read.csv(cfurl)
if (ncol(df) > 6)
  df <- df[,1:6]
colnames(df) <- gsub("\\."," ",colnames(df))
colnames(df)[1] <- "Date"
df$Date <- as.Date(df$Date,format="%m/%d/%y")

# intro plots as time series
dfxts <- xts(df[,-1],order.by=df[,1])

# panels, different scales
xyplot(dfxts,main=mainTitle,xlab=NULL)

# panels, same scales
xyplot(dfxts,
       scales = list(y = "same"),
       xlab=NULL,
       main=mainTitle,
       ylab=mainY)

# unstacked line plot
xyplot(dfxts[,5:1],
       scales = list(y = "same"),
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.line = list(lwd=2, lty=1, col=rev(fedColors))),
       main=mainTitle,
       ylab=mainY,
       superpose=TRUE,
       panel=function(x,...){
        panel.xyplot(x,...)
        panel.grid(-1,0,...)
        }
)

# convert to cumulative across columns for sand chart
dfxts[is.na(dfxts)] <- 0
sapply(2:ncol(dfxts),function(x){
  dfxts[,x] <<- dfxts[,x]+dfxts[,x-1]
})

# use area polygons to implement the stacked area chart
# plot in reverse column order to show polygon layers
xyplot(dfxts[,ncol(dfxts):1],
       scales = list(y = "same"),
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.polygon = list(lwd=2, lty=1, col=rev(fedColors))),
       main=paste(mainTitle,":","Summary View","(Stacked)"),
       ylab=mainY,
       xlab=NULL,
       superpose=TRUE,
       auto.key=list(points=TRUE,lines=FALSE),
       panel=function(x,...) {
        panel.xyarea(x,...)
        panel.grid(-1,0,...)
        }
       )


# ---- DETAILED
fedColors = c(
  "#be7c45", # traditional security
  "#d3c682", # securities lent to dealers
  "#808080", # repurchase agreements
  "#184f73", # other fed assets
  "#917960", # currency swaps
  "#537362", # term auction credit
  "#163e5f", # primary dealer
  "#72758b", # primary credit
  "#527a9c", # secondary credit
  "#529870", # seasonal credit
  "#00317a", # maiden lane 1
  "#c45b24", # maiden lane 2
  "#d7a73a", # maiden lane 3
  "#8a1e10", # asset-backed commercial paper
  "#023360", # net portfolio holdings commercial paper
  "#9c4019", # other credit
  "#53601b", # credit to AIG
  "#001c4c", # mortgage-backed securities
  "#7d2563", # federal agency debt securities
  "#002a8c", # term asset-backed securities
  "#39872e"  # long-term treasury purchases
  )

#' @references \link{http://www.clevelandfed.org/research/data/credit_easing/index.cfm}
mainTitle = "Credit Easing Policy Tools: Detail"

cfurl <- paste("http://www.clevelandfed.org/research/data/credit_easing/chart_csv.cfm?id=2&start=012007&end=",endDate,sep='')
df <- read.csv(cfurl)
if (ncol(df) > 22)
  df <- df[,1:22]
colnames(df) <- gsub("\\."," ",colnames(df))
colnames(df)[1] <- "Date"
df$Date <- as.Date(df$Date,format="%m/%d/%y")

# intro plots as time series
dfxts <- xts(df[,-1],order.by=df[,1])

# panels, different scales
xyplot(dfxts,main=mainTitle,xlab=NULL)

# panels, same scales (not useful)
# xyplot(dfxts,
#        scales = list(y = "same"),
#        xlab=NULL,
#        main=mainTitle,ylab=mainY)

# unstacked line plot
xyplot(dfxts[,ncol(dfxts):1],
       scales = list(y = "same"),
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.line = list(lwd=2, lty=1, col=rev(fedColors))),
       main=mainTitle,
       ylab=mainY,
       superpose=TRUE,
       auto.key=list(columns=2),
       panel=function(x,...){
         panel.xyplot(x,...)
         panel.grid(-1,0,...)
       }
)

# convert to cumulative across columns for sand chart
dfxts[is.na(dfxts)] <- 0
sapply(2:ncol(dfxts),function(x){
  dfxts[,x] <<- dfxts[,x]+dfxts[,x-1]
})

# use area polygons to implement the stacked area chart
# plot in reverse column order to show polygon layers
xyplot(dfxts[,ncol(dfxts):1],
       scales = list(y = "same"),
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.polygon = list(lwd=2, lty=1, col=rev(fedColors))),
       main=paste(mainTitle,":","Detail View","(Stacked)"),
       ylab=mainY,
       xlab=NULL,
       superpose=TRUE,
       auto.key=list(points=TRUE,
                     lines=FALSE,
                     columns=ifelse(ncol(dfxts)>20,3,1),
                     cex=0.8),
       panel=function(x,...) {
         panel.xyarea(x,...)
         panel.grid(-1,0,...)
       }
)

# ---- Lending to Financial Institutions
fedColors = c(
  "#be7c45", # repurchase agreements
  "#d3c682", # credit to depository institutions
  "#808080", # other fed assets
  "#184f73", # currency swaps
  "#917960", # term auction credit
  "#537362", # securities lent to dealers
  "#163e5f"  # credit extensions
  )

#' @references \link{http://www.clevelandfed.org/research/data/credit_easing/index.cfm}
mainTitle = "Lending to Financial Institutions"
cfurl <- paste("http://www.clevelandfed.org/research/data/credit_easing/chart_csv.cfm?id=3&start=012007&end=",endDate,sep='')
df <- read.csv(cfurl)
if (ncol(df) > 8)
  df <- df[,1:8]
colnames(df) <- gsub("\\."," ",colnames(df))
colnames(df)[1] <- "Date"
df$Date <- as.Date(df$Date,format="%m/%d/%y")

# intro plots as time series
dfxts <- xts(df[,-1],order.by=df[,1])

# panels, different scales
xyplot(dfxts,main=mainTitle,xlab=NULL)

# panels, same scales
xyplot(dfxts,
        scales = list(y = "same"),
        xlab=NULL,
        main=mainTitle,ylab=mainY)

# unstacked line plot
xyplot(dfxts[,ncol(dfxts):1],
       scales = list(y = "same"),
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.line = list(lwd=2, lty=1, col=rev(fedColors))),
       main=mainTitle,
       ylab=mainY,
       xlab=NULL,
       superpose=TRUE,
       auto.key=list(columns=floor(ncol(dfxts)/3)),
       panel=function(x,...){
         panel.xyplot(x,...)
         panel.grid(-1,0,...)
       }
)

# convert to cumulative across columns for sand chart
dfxts[is.na(dfxts)] <- 0
sapply(2:ncol(dfxts),function(x){
  dfxts[,x] <<- dfxts[,x]+dfxts[,x-1]
})

# use area polygons to implement the stacked area chart
# plot in reverse column order to show polygon layers
xyplot(dfxts[,ncol(dfxts):1],
       scales = list(y = "same"),
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.polygon = list(lwd=2, lty=1, col=rev(fedColors))),
       main=paste(mainTitle,"(Stacked)"),
       ylab=mainY,
       xlab=NULL,
       superpose=TRUE,
       auto.key=list(points=TRUE,
                     lines=FALSE,
                     columns=floor(ncol(dfxts)/3),
                     cex=0.8),
       panel=function(x,...) {
         panel.xyarea(x,...)
         panel.grid(-1,0,...)
       }
)

# ---- Providing Liquidity
fedColors = c(
  "#c89365", # mainden lane
  "#dcd19a", # asset-backed commercial paper
  "#979797", # net portfolio holdings commercial paper
  "#406f8c" # term asset-backed securities
)

#' @references \link{http://www.clevelandfed.org/research/data/credit_easing/index.cfm}
mainTitle = "Providing Liquidity to Key Credit Markets"
cfurl <- paste("http://www.clevelandfed.org/research/data/credit_easing/chart_csv.cfm?id=6&start=012007&end=",endDate,sep='')
df <- read.csv(cfurl)
df <- df[,-ncol(df)]
colnames(df) <- gsub("\\."," ",colnames(df))
colnames(df)[1] <- "Date"
df$Date <- as.Date(df$Date,format="%m/%d/%y")

# intro plots as time series
dfxts <- xts(df[,-1],order.by=df[,1])

# panels, different scales
xyplot(dfxts,main=mainTitle,xlab=NULL)

# panels, same scales
xyplot(dfxts,
       scales = list(y = "same"),
       xlab=NULL,
       main=mainTitle,ylab=mainY)

# unstacked line plot
xyplot(dfxts[,ncol(dfxts):1],
       scales = list(y = "same"),
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.line = list(lwd=2, lty=1, col=rev(fedColors))),
       main=mainTitle,
       ylab=mainY,
       xlab=NULL,
       superpose=TRUE,
       auto.key=list(columns=floor(ncol(dfxts)/3)),
       panel=function(x,...){
         panel.xyplot(x,...)
         panel.grid(-1,0,...)
       }
)

# convert to cumulative across columns for sand chart
dfxts[is.na(dfxts)] <- 0
sapply(2:ncol(dfxts),function(x){
  dfxts[,x] <<- dfxts[,x]+dfxts[,x-1]
})

# use area polygons to implement the stacked area chart
# plot in reverse column order to show polygon layers
xyplot(dfxts[,ncol(dfxts):1],
       scales = list(y = "same"),
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.polygon = list(lwd=2, lty=1, col=rev(fedColors))),
       main=paste(mainTitle,"(Stacked)"),
       ylab=mainY,
       xlab=NULL,
       superpose=TRUE,
       auto.key=list(points=TRUE,
                     lines=FALSE,
                     columns=floor(ncol(dfxts)/3),
                     cex=0.8),
       panel=function(x,...) {
         panel.xyarea(x,...)
         panel.grid(-1,0,...)
       }
)

# ---- Long-Term Treasury Purchases
fedColors = c(
  "#cb9668" # treasury purchases
)

#' @references \link{http://www.clevelandfed.org/research/data/credit_easing/index.cfm}
mainTitle = "Long-Term Treasury Purchases"
cfurl <- paste("http://www.clevelandfed.org/research/data/credit_easing/chart_csv.cfm?id=30&start=012007&end=",endDate,sep='')
df <- read.csv(cfurl)
df <- df[,-ncol(df)]
colnames(df) <- gsub("\\."," ",colnames(df))
colnames(df)[1] <- "Date"
df$Date <- as.Date(df$Date,format="%m/%d/%y")

# intro plots as time series
dfxts <- xts(df[,-1],order.by=df[,1])
colnames(dfxts) <- colnames(df)[-1]


# unstacked line plot
xyplot(dfxts[,ncol(dfxts):1],
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.line = list(lwd=2, lty=1, col=rev(fedColors))),
       main=mainTitle,
       ylab=mainY,
       xlab=NULL,
       superpose=TRUE,
       auto.key=ifelse(ncol(dfxts)==1,
                       FALSE,
                       list(columns=max(floor(ncol(dfxts)/3),1))),
       panel=function(x,...){
         panel.xyplot(x,...)
         panel.grid(-1,0,...)
       }
)

# convert to cumulative across columns for sand chart
if ( ncol(dfxts) > 1 ) {
  dfxts[is.na(dfxts)] <- 0
  sapply(2:ncol(dfxts),function(x){
    dfxts[,x] <<- dfxts[,x]+dfxts[,x-1]
  })
}

# use area polygons to implement the stacked area chart
# plot in reverse column order to show polygon layers
xyplot(dfxts[,ncol(dfxts):1],
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.polygon = list(lwd=2, lty=1, col=rev(fedColors))),
       main=paste(mainTitle,ifelse(ncol(dfxts)>1,
                                   "(Stacked)","")),
       ylab=mainY,
       xlab=NULL,
       superpose=TRUE,
       auto.key=ifelse(ncol(dfxts)==1,FALSE,
                       list(points=TRUE,
                     lines=FALSE,
                     columns=max(floor(ncol(dfxts)/3),1),
                     cex=0.8)),
       panel=function(x,...) {
         panel.xyarea(x,...)
         panel.grid(-1,0,...)
       }
)

# ---- Traditional Security Holdings
fedColors = c(
  "#cb9668" # treasury holdings
)

#' @references \link{http://www.clevelandfed.org/research/data/credit_easing/index.cfm}
mainTitle = "Traditional Security Holdings"
cfurl <- paste("http://www.clevelandfed.org/research/data/credit_easing/chart_csv.cfm?id=8&start=012007&end=",endDate,sep='')
df <- read.csv(cfurl)
df <- df[,-ncol(df)]
colnames(df) <- gsub("\\."," ",colnames(df))
colnames(df)[1] <- "Date"
df$Date <- as.Date(df$Date,format="%m/%d/%y")

# intro plots as time series
dfxts <- xts(df[,-1],order.by=df[,1])
colnames(dfxts) <- colnames(df)[-1]

# unstacked line plot
xyplot(dfxts[,ncol(dfxts):1],
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.line = list(lwd=2, lty=1, col=rev(fedColors))),
       main=mainTitle,
       ylab=mainY,
       xlab=NULL,
       superpose=TRUE,
       scales=list(y=list(limits=c(0,max(dfxts)))),
       auto.key=ifelse(ncol(dfxts)==1,
                       FALSE,
                       list(columns=max(floor(ncol(dfxts)/3),1))),
       panel=function(x,...){
         panel.xyplot(x,...)
         panel.grid(-1,0,...)
       }
)

# convert to cumulative across columns for sand chart
if ( ncol(dfxts) > 1 ) {
  dfxts[is.na(dfxts)] <- 0
  sapply(2:ncol(dfxts),function(x){
    dfxts[,x] <<- dfxts[,x]+dfxts[,x-1]
  })
}

# use area polygons to implement the stacked area chart
# plot in reverse column order to show polygon layers
xyplot(dfxts[,ncol(dfxts):1],
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.polygon = list(lwd=2, lty=1, col=rev(fedColors))),
       main=paste(mainTitle,ifelse(ncol(dfxts)>1,
                                   "(Stacked)","")),
       ylab=mainY,
       xlab=NULL,
       superpose=TRUE,
       scales=list(y=list(limits=c(0,max(dfxts)))),
       auto.key=ifelse(ncol(dfxts)==1,FALSE,
                       list(points=TRUE,
                            lines=FALSE,
                            columns=max(floor(ncol(dfxts)/3),1),
                            cex=0.8)),
       panel=function(x,...) {
         panel.xyarea(x,...)
         panel.grid(-1,0,...)
       }
)

# ---- Agency Debt and MBS
fedColors = c(
  "#cb9668", # federal agency debt
  "#dcd19a"  # mortgage-backed securities
)

#' @references \link{http://www.clevelandfed.org/research/data/credit_easing/index.cfm}
mainTitle = "Federal Agency Debt and Mortgage-Backed Securities"
cfurl <- paste("http://www.clevelandfed.org/research/data/credit_easing/chart_csv.cfm?id=9&start=012007&end=",endDate,sep='')
df <- read.csv(cfurl)
df <- df[,-ncol(df)]
colnames(df) <- gsub("\\."," ",colnames(df))
colnames(df)[1] <- "Date"
df$Date <- as.Date(df$Date,format="%m/%d/%y")

# intro plots as time series
dfxts <- xts(df[,-1],order.by=df[,1])
colnames(dfxts) <- colnames(df)[-1]


# panels, different scales
xyplot(dfxts,main=mainTitle,xlab=NULL)

# panels, same scales
xyplot(dfxts,
       scales = list(y = "same"),
       xlab=NULL,
       main=mainTitle,ylab=mainY)

# unstacked line plot
xyplot(dfxts[,ncol(dfxts):1],
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.line = list(lwd=2, lty=1, col=rev(fedColors))),
       main=mainTitle,
       ylab=mainY,
       xlab=NULL,
       superpose=TRUE,
       scales=list(y=list(limits=c(0,max(dfxts)))),
       auto.key=
         list(columns=max(floor(ncol(dfxts)/3),2)),
       panel=function(x,...){
         panel.xyplot(x,...)
         panel.grid(-1,0,...)
       }
)

# convert to cumulative across columns for sand chart
if ( ncol(dfxts) > 1 ) {
  dfxts[is.na(dfxts)] <- 0
  sapply(2:ncol(dfxts),function(x){
    dfxts[,x] <<- dfxts[,x]+dfxts[,x-1]
  })
}

# use area polygons to implement the stacked area chart
# plot in reverse column order to show polygon layers
xyplot(dfxts[,ncol(dfxts):1],
       par.settings = list(superpose.symbol = list(pch=15, col=rev(fedColors), cex=1.2),
                           superpose.polygon = list(lwd=2, lty=1, col=rev(fedColors))),
       main=paste(mainTitle,ifelse(ncol(dfxts)>1,
                                   "(Stacked)","")),
       ylab=mainY,
       xlab=NULL,
       superpose=TRUE,
       scales=list(y=list(limits=c(0,max(dfxts)))),
       auto.key=
         list(points=TRUE,
              lines=FALSE,
              columns=max(floor(ncol(dfxts)/3),2),
              cex=0.8),
       panel=function(x,...) {
         panel.xyarea(x,...)
         panel.grid(-1,0,...)
       }
)

