#' @title Get credit easing data.
#' @description Downloads Cleveland FRB credit easing policy tool data.
#' @details Forms a query using the given \code{id} and submits the query to the data site, thereby downloading the requested data in CSV format.  Transforms the CSV into a data frame, transforms the character date into \code{Date} objects, and then an \code{xts} object.  
#' @return A list of class \code{easing} containing query items and \code{xts} result.
#' @param id the query type identifier code as specified by the FRB data site, default NA
#' @param startDate the desired start month and year number in MMYYYY format, default 012007
#' @param endDate the desired end month and year number in MMYYYY format, default 122100 for all available data
#' @return List of class type \code{easing} containing \code{xts} time history object \code{df}, query start date \code{startDate}, query stop date \code{stopDate}, plot colors array \code{colors}, plot main title \code{main}, and plot y-axis label \code{ylab}.
#' @export
#' @importFrom utils read.csv
#' @import xts
#' @references \url{http://www.clevelandfed.org/research/data/credit_easing/index.cfm}
#' @seealso getEasingSummary getEasingDetails getEasingLending getEasingCreditDepository getEasingCreditExtensions getEasingProvidingLiquidity getEasingMaidenLane getEasingTraditionalHoldings  getEasingAgencyDebt getEasingLongTermPurchases
#' @note Meant for internal use by the other, more specific, query functions.
#' @examples
#' \dontrun{ 
#' getEasingData(id=1)
#' }
#' 
getEasingData <- function(id=NA,startDate="012007",endDate="122100") {
  stopifnot(is.na(id)==FALSE)
  
  cfurl <- paste("http://www.clevelandfed.org/research/data/credit_easing/chart_csv.cfm?id="
                 ,id,
                 "&start=",
                 startDate,
                 "&end=",
                 endDate,
                 sep='')
  
  df <- read.csv(cfurl)
  df <- df[,-ncol(df)]
  colnames(df) <- gsub("\\."," ",colnames(df))
  colnames(df)[1] <- "Date"
  df$Date <- as.Date(df$Date,format="%m/%d/%y")
  
  # convert to time series
  dfxts <- xts(df[,-1],order.by=df[,1])
  colnames(dfxts) <- colnames(df)[-1]
  rv <- list(id=id,
             df=dfxts,
             startDate=startDate,
             endDate=endDate,
             colors=c(),
             main="Easing Policy Tools",
             ylab="(Millions of Dollars)")
  class(rv) <- "easing"
  rv
}

#' @title Get credit easing summary.
#' @description Downloads FRB credit easing policy summary data.
#' @details Downloads the Cleveland FRB data product for credit easing policy summary weekly time series, including columns for
#' \itemize{
#' \item traditional security holdings
#' \item long-term treasury purchases
#' \item lending to financial institutinos
#' \item liquidity to key credit markets
#' \item federal agency debt mortgage backed securities purchases
#' }
#' @param startDate query start date string in MMYYYY format, default 012007
#' @param endDate query end date string in MMYYYY format, default 122100
#' @return A list of class \code{easing}
#' @export
#' @seealso getEasingData getEasingDetails getEasingLending getEasingCreditDepository getEasingCreditExtensions getEasingProvidingLiquidity getEasingMaidenLane getEasingTraditionalHoldings  getEasingAgencyDebt getEasingLongTermPurchases
#' @examples
#' \dontrun{
#' es <- getEasingSummary()
#' head(es$df)
#' }
getEasingSummary <- function(startDate="012007",endDate="122100") {
  fedColors = c(
    "#ce9257", # traditional security holdings
    "#d9d28b", # long-term treasury purchases
    "#9a9a9a", # lending to financial institutinos
    "#397093", # liquidity to key credit markets
    "#aa947c"  # federal agency debt mortgage backed securities purchases
  )
  rv <- getEasingData(1,startDate,endDate)
  rv$colors = fedColors
  rv$main="Credit Easing Policy Tools Summary"
  rv
}

#' @title Get credit easing details.
#' @description Downloads FRB credit easing policy details data.
#' @details Downloads the Cleveland FRB data product for credit easing policy detail weekly time series, including columns for
#' \itemize{
#' \item traditional security
#' \item securities lent to dealers
#' \item repurchase agreements
#' \item other fed assets
#' \item currency swaps
#' \item term auction credit
#' \item primary dealer
#' \item primary credit
#' \item secondary credit
#' \item seasonal credit
#' \item maiden lane 1
#' \item maiden lane 2
#' \item maiden lane 3
#' \item asset-backed commercial paper
#' \item net portfolio holdings commercial paper
#' \item other credit
#' \item credit to AIG
#' \item mortgage-backed securities
#' \item federal agency debt securities
#' \item term asset-backed securities
#' \item long-term treasury purchases
#' }
#' @param startDate query start date string in MMYYYY format, default 012007
#' @param endDate query end date string in MMYYYY format, default 122100
#' @return A list of class \code{easing}
#' @export
#' @seealso getEasingData getEasingSummary getEasingLending getEasingCreditDepository getEasingCreditExtensions getEasingProvidingLiquidity getEasingMaidenLane getEasingTraditionalHoldings  getEasingAgencyDebt getEasingLongTermPurchases
#' @examples
#' \dontrun{
#' ed <- getEasingDetail()
#' head(ed$df)
#' }
getEasingDetails <- function(startDate="012007",endDate="122100") {
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
  rv <- getEasingData(2,startDate,endDate)
  rv$colors = fedColors
  rv$main="Credit Easing Policy Tools Detail"
  rv
}

#' @title Get credit easing lending.
#' @description Downloads FRB credit easing policy lending data.
#' @details Downloads the Cleveland FRB data product for credit easing policy lending weekly time series, including columns for
#' \itemize{
#' \item repurchase agreements
#' \item credit to depository institutions
#' \item other fed assets
#' \item currency swaps
#' \item term auction credit
#' \item securities lent to dealers
#' \item credit extensions
#' }
#' @param startDate query start date string in MMYYYY format, default 012007
#' @param endDate query end date string in MMYYYY format, default 122100
#' @return A list of class \code{easing}
#' @export
#' @seealso getEasingData getEasingSummary getEasingDetails getEasingCreditDepository getEasingCreditExtensions getEasingProvidingLiquidity getEasingMaidenLane getEasingTraditionalHoldings  getEasingAgencyDebt getEasingLongTermPurchases
#' @examples
#' \dontrun{
#' el <- getEasingLending()
#' head(el$df)
#' }
getEasingLending <- function(startDate="012007",endDate="122100") {
  fedColors = c(
    "#be7c45", # repurchase agreements
    "#d3c682", # credit to depository institutions
    "#808080", # other fed assets
    "#184f73", # currency swaps
    "#917960", # term auction credit
    "#537362", # securities lent to dealers
    "#163e5f"  # credit extensions
  )
  rv <- getEasingData(3,startDate,endDate)
  rv$colors = fedColors
  rv$main="Lending to Financial Institutions"
  rv
}

#' @title Get credit easing credit to depository institutions
#' @description Downloads FRB credit easing policy credit to depository institutions data.
#' @details Downloads the Cleveland FRB data product for credit easing policy credit to depository institutions weekly time series, including columns for
#' \itemize{
#' \item primary credit
#' \item secondary credit
#' \item seasonal credit
#' }
#' @param startDate query start date string in MMYYYY format, default 012007
#' @param endDate query end date string in MMYYYY format, default 122100
#' @return A list of class \code{easing}
#' @export
#' @seealso getEasingData getEasingSummary getEasingDetails getEasingLending getEasingCreditExtensions getEasingProvidingLiquidity getEasingMaidenLane getEasingTraditionalHoldings  getEasingAgencyDebt getEasingLongTermPurchases
#' @examples
#' \dontrun{
#' cd <- getEasingCreditDepository()
#' head(cd$df)
#' }
getEasingCreditDepository <- function(startDate="012007",endDate="122100") {
  fedColors = c(
    "#d1955a", # primary credit
    "#d3ca72", # secondary credit
    "#808080"  # seasonal credit
  )
  rv <- getEasingData(4,startDate,endDate)
  rv$colors = fedColors
  rv$main="Credit to Depository Institutions"
  rv
}

#' @title Get credit easing credit extensions data
#' @description Downloads FRB credit easing policy credit extensions data.
#' @details Downloads the Cleveland FRB data product for credit easing policy credit extensions weekly time series, including columns for
#' \itemize{
#' \item primary/other broker dealer
#' \item credit to AIG
#' \item other credit
#' }
#' @param startDate query start date string in MMYYYY format, default 012007
#' @param endDate query end date string in MMYYYY format, default 122100
#' @return A list of class \code{easing}
#' @export
#' @seealso getEasingData getEasingSummary getEasingDetails getEasingLending getEasingCreditDepository getEasingProvidingLiquidity getEasingMaidenLane getEasingTraditionalHoldings  getEasingAgencyDebt getEasingLongTermPurchases
#' @examples
#' \dontrun{
#' ce <- getEasingCreditExtensions()
#' head(ce$df)
#' }
getEasingCreditExtensions <- function(startDate="012007",endDate="122100") {
  fedColors <- c(
    "#d1955a", # primary/other broker dealer
    "#d3ca72", # credit to AIG
    "#808080"  # other credit
  )
  rv <- getEasingData(5,startDate,endDate)
  rv$colors = fedColors
  rv$main="Credit Extensions"
  rv
}

#' @title Get credit easing credit providing liquidity data
#' @description Downloads FRB credit easing policy tools providing liquidity to key credit markets data.
#' @details Downloads the Cleveland FRB data product for credit easing policy tools providing liquidity weekly time series, including columns for
#' \itemize{
#' \item Maiden Lane
#' \item asset-backed commercial paper
#' \item net portfolio holdings commercial paper
#' \item term asset-backed securities
#' }
#' @param startDate query start date string in MMYYYY format, default 012007
#' @param endDate query end date string in MMYYYY format, default 122100
#' @return A list of class \code{easing}
#' @export
#' @seealso getEasingData getEasingSummary getEasingDetails getEasingLending getEasingCreditDepository getEasingCredtExtensions getEasingMaidenLane getEasingTraditionalHoldings  getEasingAgencyDebt getEasingLongTermPurchases
#' @examples
#' \dontrun{
#' pl <- getEasingProvidingLiquidity()
#' head(pl$df)
#' }
getEasingProvidingLiquidity <- function(startDate="012007",endDate="122100") {
  fedColors = c(
    "#c89365", # maiden lane
    "#dcd19a", # asset-backed commercial paper
    "#979797", # net portfolio holdings commercial paper
    "#406f8c" # term asset-backed securities
  )
  rv <- getEasingData(6,startDate,endDate)
  rv$colors = fedColors
  rv$main="Providing Liquidity to Key Credit Markets"
  rv
}

#' @title Get credit easing credit Maiden Lane data.
#' @description Downloads FRB credit easing policy tools Maiden Lane data.
#' @details Downloads the Cleveland FRB data product for credit easing policy tools Maiden Lane weekly time series, including columns for
#' \itemize{
#' \item Maiden Lane 1
#' \item Maiden Lane 2
#' \item Maiden Lane 3
#' }
#' @param startDate query start date string in MMYYYY format, default 012007
#' @param endDate query end date string in MMYYYY format, default 122100
#' @return A list of class \code{easing}
#' @export
#' @seealso getEasingData getEasingSummary getEasingDetails getEasingLending getEasingCreditDepository getEasingCredtExtensions getEasingProvidingLiquidity getEasingTraditionalHoldings  getEasingAgencyDebt getEasingLongTermPurchases
#' @examples
#' \dontrun{
#' ml <- getEasingMaidenLane()
#' head(ml$df)
#' }
getEasingMaidenLane <- function(startDate="012007",endDate="122100") {
  fedColors <- c(
    "#d1955a", # maiden lane 1
    "#d3ca72", # maiden lane 2
    "#808080"  # maiden lane 3
  )
  rv <- getEasingData(7,startDate,endDate)
  rv$colors = fedColors
  rv$main="Maiden Lane"
  rv
}

#' @title Get credit easing credit traditional security holdings data.
#' @description Downloads FRB credit easing policy tools traditional security holdings data.
#' @details Downloads the Cleveland FRB data product for credit easing policy tools traditional security holdings weekly time series, including columns for
#' \itemize{
#' \item traditional security holdings
#' }
#' @param startDate query start date string in MMYYYY format, default 012007
#' @param endDate query end date string in MMYYYY format, default 122100
#' @return A list of class \code{easing}
#' @export
#' @seealso getEasingData getEasingSummary getEasingDetails getEasingLending getEasingCreditDepository getEasingCredtExtensions getEasingProvidingLiquidity getEasingMaidenLane  getEasingAgencyDebt getEasingLongTermPurchases
#' @examples
#' \dontrun{
#' th <- getEasingTraditionalHoldings()
#' head(th$df)
#' }
getEasingTraditionalHoldings <- function(startDate="012007",endDate="122100") {
  fedColors = c(
    "#cb9668" # treasury holdings
  )
  rv <- getEasingData(8,startDate,endDate)
  rv$colors = fedColors
  rv$main="Traditional Security Holdings"
  rv
}

#' @title Get credit easing credit agency debt data.
#' @description Downloads FRB credit easing policy tools federal agency debt and mortgage-backed securities data.
#' @details Downloads the Cleveland FRB data product for credit easing policy tools federal agency debt and mortgage-backed securities weekly time series, including columns for
#' \itemize{
#' \item federal agency debt
#' \item mortgage-backed securities
#' }
#' @param startDate query start date string in MMYYYY format, default 012007
#' @param endDate query end date string in MMYYYY format, default 122100
#' @return A list of class \code{easing}
#' @export
#' @seealso getEasingData getEasingSummary getEasingDetails getEasingLending getEasingCreditDepository getEasingCredtExtensions getEasingProvidingLiquidity getEasingMaidenLane  getEasingTraditionalHoldings getEasingLongTermPurchases
#' @examples
#' \dontrun{
#' ad <- getEasingAgencyDebt()
#' head(ad$df)
#' }
getEasingAgencyDebt <- function(startDate="012007",endDate="122100") {
  fedColors = c(
    "#cb9668", # federal agency debt
    "#dcd19a"  # mortgage-backed securities
  )
  rv <- getEasingData(9,startDate,endDate)
  rv$colors = fedColors
  rv$main="Federal Agency Debt and Mortgage-Backed Securities"
  rv
}

#' @title Get credit easing long-term treasury purchases data.
#' @description Downloads FRB credit easing policy tools long-term treasury purchases data.
#' @details Downloads the Cleveland FRB data product for credit easing policy tools long-term treasury purchases weekly time series, including columns for
#' \itemize{
#' \item treasury purchases
#' }
#' @param startDate query start date string in MMYYYY format, default 012007
#' @param endDate query end date string in MMYYYY format, default 122100
#' @return A list of class \code{easing}
#' @export
#' @seealso getEasingData getEasingSummary getEasingDetails getEasingLending getEasingCreditDepository getEasingCredtExtensions getEasingProvidingLiquidity getEasingMaidenLane  getEasingTraditionalHoldings getEasingAgencyDebt
#' @examples
#' \dontrun{
#' lt <- getEasingLongTermPurchases()
#' head(lt$df)
#' }
getEasingLongTermPurchases <- function(startDate="012007",endDate="122100") {
  fedColors = c(
    "#cb9668" # treasury purchases
  )
  rv <- getEasingData(30,startDate,endDate)
  rv$colors = fedColors
  rv$main="Long-Term Treasury Purchases"
  rv
}
