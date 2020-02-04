# Denver-AirBnb-Project
This project was started in the summer semester during my Masters in Business Analytics program at CU Boulder. It is intended to be an exploratory project into the basics of R.

### Let's start by uploading the necessary CSV's into R and install any necessary packages. For this project the only package used was leaflet.

``` r
listings <- read.csv("denver_listings.csv",stringsAsFactors=FALSE)
URL <- read.csv("URLs.csv",stringsAsFactors=FALSE)

install.packages('leaflet')
require('leaflet')
```
