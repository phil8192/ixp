.onAttach <- function(libname, pkgname) {
  packageStartupMessage("-= IXP Utils =-")
}

.onLoad <- function(libname, pkgname) {
  options(digits.secs=3)
  options(scipen=999)
}
