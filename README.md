Credit Easing Policy Tools
==========================

[Now on CRAN](http://cran.r-project.org/web/packages/easingr/index.html).

The R package ```easingr``` provides convenient access to the Credit Easing Policy Tools data provided by the Federal Reserve Bank of Cleveland.  The package provides data download functions and some representative plots along the same categories as provided by the bank. See the [FRB's terms of use](http://www.clevelandfed.org/utilities/terms_of_use.cfm?DCS.nav=Footer) regarding these data.   

As a simple example of gathering data and drawing simple plots consider the R code
```{r}
require(easingr)
require(lattice)
dt <- getEasingDetails()
xyplot(dt)
easingLineChart(dt)
```

Several examples of plots are shown by category below.  For details on each query and more example plots, please see the package vignette.  

Summary Report
--------------

<img src="man/figures/summaryLine.png" width="100%"/>
<img src="man/figures/summaryArea.png" width="100%"/>

Details Report
--------------
<img src="man/figures/detailLine.png" width="100%"/>
<img src="man/figures/detailArea.png" width="100%"/>

Lending to Financial Institutions
---------------------------------
<img src="man/figures/lendingLine.png" width="100%"/>
<img src="man/figures/lendingArea.png" width="100%"/>

Credit to Depository Institutions
---------------------------------
<img src="man/figures/creditLine.png" width="100%"/>
<img src="man/figures/creditArea.png" width="100%"/>

Credit Extensions to Financial Institutions
-------------------------------------------
<img src="man/figures/extensionsLine.png" width="100%"/>
<img src="man/figures/extensionsArea.png" width="100%"/>

Providing Liquidity
-------------------
<img src="man/figures/liquidityLine.png" width="100%"/>
<img src="man/figures/liquidityArea.png" width="100%"/>

Maiden Lane
-----------
<img src="man/figures/maidenLine.png" width="100%"/>
<img src="man/figures/maidenArea.png" width="100%"/>

Traditional Security Holdings
-----------------------------
<img src="man/figures/traditionalLine.png" width="100%"/>
<img src="man/figures/traditionalArea.png" width="100%"/>

Federal Agency Debt and Mortgage-Backed Securities
--------------------------------------------------
<img src="man/figures/agencyLine.png" width="100%"/>
<img src="man/figures/agencyArea.png" width="100%"/>

Long Term Treasury Purchases
----------------------------
<img src="man/figures/treasuryLine.png" width="100%"/>
<img src="man/figures/treasuryArea.png" width="100%"/>

References
----------
* See [Cleveland FRB](http://www.clevelandfed.org/research/data/credit_easing/index.cfm) for more details.

