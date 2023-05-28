
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

packagePreamble.xdvirPackage <- function(x) {
    package$preamble
}

packagePreamble.xdvirPackageList <- function(x) {
    unlist(lapply(x, packagePreamble))
}
