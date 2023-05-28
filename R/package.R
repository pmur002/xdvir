
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

packagePreamble <- function(x) {
    UseMethod("packagePreamble")
}

packagePreamble.NULL <- function(x) {
    NULL
}

packagePreamble.xdvirPackage <- function(x) {
    x$preamble
}

packagePreamble.list <- function(x) {
    unlist(lapply(x, packagePreamble))
}

packagePrefix <- function(x) {
    UseMethod("packagePrefix")
}

packagePrefix.NULL <- function(x) {
    NULL
}

packagePrefix.xdvirPackage <- function(x) {
    x$prefix
}

packagePrefix.list <- function(x) {
    unlist(lapply(x, packagePrefix))
}

packageSuffix <- function(x) {
    UseMethod("packageSuffix")
}

packageSuffix.NULL <- function(x) {
    NULL
}

packageSuffix.xdvirPackage <- function(x) {
    x$suffix
}

packageSuffix.list <- function(x) {
    unlist(lapply(x, packageSuffix))
}

################################################################################
## Add convenience for specifying packages

set("packageRegister", NULL)

registerPackage <- function(pkg, alias) {
    register <- get("packageRegister")
    existing <- names(register)
    if (!inherits(pkg, "xdvirPackage"))
        stop("Invalid package")
    if (length(alias) > 1)
        stop("Please only register one package at a time")
    if (alias %in% existing)
        stop(sprtinf("Package alias %s already in use", alias))
    register[[alias]] <- pkg
    set("packageRegister", register)
}

resolvePackage <- function(x, ...) {
    UseMethod("resolvePackage")
}

## Package can be a character alias
resolvePackage.character <- function(x, ...) {
    register <- get("packageRegister")
    existing <- names(register)
    if (x %in% existing) {
        register[[x]]
    } else {
        stop(sprintf("Unknown package (%s)", x))
    }
}

resolvePackage.xdvirPackage <- function(x, ...) {
    x
}

resolvePackages <- function(x, ...) {
    UseMethod("resolvePackages")
}

resolvePackages.NULL <- function(x, ...) {
    NULL
}

## Packages can be specified as a character vector of aliases
resolvePackages.character <- function(x, ...) {
    resolvePackages(as.list(x))
}

## Package can be specified as a single "xdvirPackage"
resolvePackages.xdvirPackage <- function(x, ...) {
    list(x)
}

## Packages can be specified as a list, consisting of either
## character alias or "xdvirPackage"
resolvePackages.list <- function(x, ...) {
    lapply(x, resolvePackage)
}
