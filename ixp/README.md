# IXP Utilities

IXP data helper functions.

## Installing

Direct from github repo:

```
library(devtools)
install_github("datasciencecampus/tron/ixp", auth_token="your_secret_github_token")
```

Or from local:

```
install_local("ixp_dir")
```

## Building

Build requires `devtools` and `roxygen2`

```R
# R
install.packages("devtools")
install.packages("roxygen2")
```

Also, for pdf manual generation, need `texlive-fontsextra` (arch linux)

```
pacman -S texlive-fontsextra
```

Then

```
make
make install
```

manual can be built with:

```
make check
```

then have a look in `../ixp.Rcheck/ixp-manual.pdf`

## Using

```R
library(ixp)
```

## Pre-processed data

Note that `ixp` package contains pre-processed data for
`lon1` `lon2` `man1` `sco1` `nva1` exchanges. Just refer to dataset to load
it on the fly.

```
library(ixp)
library(xts)

> head(nva1)
                     Bandwidth Weekday MsM
2015-06-15 00:05:00 1065693898       2  65
2015-06-15 00:10:00 1010188471       2  70
2015-06-15 00:15:00 1004915417       2  75
2015-06-15 00:20:00 1052272150       2  80
2015-06-15 00:25:00 1052241056       2  85
2015-06-15 00:30:00 1000122004       2  90
```
