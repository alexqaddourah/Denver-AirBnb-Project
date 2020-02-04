# Denver-AirBnb-Project
This project was started in the summer semester during my Masters in Business Analytics program at CU Boulder. It is intended to be an exploratory project into the basics of R.

## Data Cleaning
#### Let's start by uploading the necessary CSV's into R and install any necessary packages. For this project the only package used was leaflet.

``` r
listings <- read.csv("denver_listings.csv",stringsAsFactors=FALSE)
URL <- read.csv("URLs.csv",stringsAsFactors=FALSE)

install.packages('leaflet')
require('leaflet')
```
#### For starters, we need to do a little bit of cleaning with the price columns. The data set has information on both "price" which is what we assume is nightly vs "weekly price" which is what my group assumed was the price for renting this AirBNB for the entire week:

#### We need to take the price out of character form and change it into numeric so it is easy to find means and other stats. Let's remove the dollar sign and the quotes, and then change it into numeric for both price and weekly_price.
``` r
listings2$price <- sub(pattern="$",replacement="",listings2$price,fixed=TRUE)
listings2$price <- as.numeric(listings2$price)

listings2$weekly_price <- sub(pattern="$",replacement="",listings2$weekly_price,fixed=TRUE)
listings2$weekly_price <- as.numeric(listings2$weekly_price)
```

#### Filling in "NA's" with 0's.
``` r
listings2[is.na(listings2)] <- 0
```

#### Some of the weekly values in our data set do not have prices for the week. We could assign NA to them or throw them out, but lets try and give them a value using their corresponding price/night value. We can take that value and multiply to 7 to give us some estimation for the weekly price.
``` r
listings2$price_WEEK <- rep(0,times=nrow(listings2))

listings2[listings2$weekly_price == 0,'price_WEEK'] <- listings2[listings2$weekly_price == 0,'price']*7

listings2[listings2$weekly_price != 0,'price_WEEK'] <- listings2[listings2$weekly_price != 0,'weekly_price']
```

#### Now that we have cleaned both our price/night and price/week columns, let's see how they look!
``` r
head((listings2$price),25)
# [1]  56 140  61  42  70  95  76  65 110 111 300 179 120  75  70  60 125 411
# [19] 195  35  32  66 235 300  70
head((listings2$price_WEEK),25)
#  [1]  392  980  375  295  490  665  532  455  770  649 2100 1253  840  399
# [15]  455  364  900 2877 1365  900  350  950 1645 2100  450
```
