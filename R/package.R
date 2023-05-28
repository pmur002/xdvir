
## Functions that define a "package" that can be added to an "engine"

package <- function(preamble=NULL,
                    prefix=NULL,
                    suffix=NULL,
                    special=NULL) {
    pkg <- list(preamble=preamble,
                prefix=prefix,
                suffic=suffix,
                special=special)
    class(pkg) <- "xdvirPackage"
    pkg
}

packageList <- function(...) {
    pkgList <- list(...)
    if (!all(sapply(pkgList, inherits, "xdvirPackage")))
        stop("Invalid package(s)")
    class(pkgList) <- "xdvirPackageList"
    pkgList
}

packagePreamble <- function(x) {
    UseMethod("packagePreamble")
}

packagePreamble.NULL <- function(x) {
    NULL
}

packagePreamble.xdvirPackage <- function(x) {
    package$preamble
}

packagePreamble.xdvirPackageList <- function(x) {
    unlist(lapply(x, packagePreamble))
}

packagePrefix <- function(x) {
    UseMethod("packagePrefix")
}

packagePrefix.NULL <- function(x) {
    NULL
}

packagePrefix.xdvirPackage <- function(x) {
    package$preamble
}

packagePrefix.xdvirPackageList <- function(x) {
    unlist(lapply(x, packagePrefix))
}

packageSuffix <- function(x) {
    UseMethod("packageSuffix")
}

packageSuffix.NULL <- function(x) {
    NULL
}

packageSuffix.xdvirPackage <- function(x) {
    package$preamble
}

packageSuffix.xdvirPackageList <- function(x) {
    unlist(lapply(x, packageSuffix))
}

