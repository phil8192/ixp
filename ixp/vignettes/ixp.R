## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ixp)

## ------------------------------------------------------------------------
lon2_smoothed <- smooth_and_clean(my_data = lon2$Bandwidth)

## ------------------------------------------------------------------------
peaks <- peak_finder(lon2_smoothed)
features <- feature_frame(lon2_smoothed)

## ------------------------------------------------------------------------
may_5_16 <- select_date(date = "2016-05-05")
features %>% may_5_16()
peaks %>% may_5_16()

## ------------------------------------------------------------------------
features %>% may_5_16() %>% time_2_increase()

## ------------------------------------------------------------------------
my_dates <- seq(as.Date("2016-01-01"), as.Date("2016-01-31"), by = 7)
jan_16 <- select_date(my_dates)
features %>% jan_16() %>% time_2_increase()


## ------------------------------------------------------------------------
features %>% jan_16() %>% time_2_increase %>% gradients()

## ------------------------------------------------------------------------
unlist(features %>% jan_16() %>% time_2_increase %>% gradients(), recursive = F)

## ------------------------------------------------------------------------
all_dates_16 <- select_date(date = seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = 1))
mat_16 <- do.call(rbind, unlist(features %>%
                              all_dates_16() %>%
                              time_2_increase() %>%
                              gradients(), recursive = FALSE, use.names = FALSE))
head(mat_16, n = 10)

