require(utils)
require(xts)
require(lattice)
require(latticeExtra)

# summary view, millions of dollars
#cfurl <- "http://www.clevelandfed.org/research/data/credit_easing/chart_csv.cfm?id=1&start=012007&end=072014"

# detail view, millions of dollars

endDate = "122100" # bogus end date beyond life
mainTitle = "Credit Easing Policy Tools"
mainY = "(Millions of Dollars)"

# ---- SUMMARY
fedColors = c("#aa947c","#397093","#9a9a9a","#d9d28b","#ce9257")

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
xyplot(dfxts,main=mainTitle)

# panels, same scales
xyplot(dfxts,scales = list(y = "same"),main=mainTitle,ylab=mainY)

# unstacked line plot
xyplot(dfxts[,5:1],
       scales = list(y = "same"),
       par.settings = list(superpose.symbol = list(pch=15, col=fedColors, cex=1.2),
                           superpose.line = list(lwd=2, lty=1, col=fedColors)),
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
xyplot(dfxts[,5:1],
       scales = list(y = "same"),
       par.settings = list(superpose.symbol = list(pch=15, col=fedColors, cex=1.2),
                           superpose.polygon = list(lwd=2, lty=1, col=fedColors)),
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
#' @references \link{http://www.clevelandfed.org/research/data/credit_easing/index.cfm}
cfurl <- paste("http://www.clevelandfed.org/research/data/credit_easing/chart_csv.cfm?id=2&start=012007&end=",endDate,sep='')
df <- read.csv(cfurl)
if (ncol(df) > 22)
  df <- df[,1:22]
colnames(df) <- gsub("\\."," ",colnames(df))
colnames(df)[1] <- "Date"
df$Date <- as.Date(df$Date,format="%m/%d/%y")

