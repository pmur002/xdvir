
## Functions that define a "package" that can be added to an "engine"

LaTeXpackage <- function(name,
                         preamble=NULL,
                         prefix=NULL,
                         suffix=NULL,
                         special=NULL,
                         init=NULL,
                         final=NULL) {
    if (!is.character(name) && length(name) == 1) 
        stop("Invalid package name")
    pkg <- list(name=name,
                preamble=preamble,
                prefix=prefix,
                suffix=suffix,
                special=special,
                final=final)
    class(pkg) <- "LaTeXpackage"
    pkg
}

packagePreamble <- function(x) {
    UseMethod("packagePreamble")
}

packagePreamble.NULL <- function(x) {
    NULL
}

packagePreamble.LaTeXpackage <- function(x) {
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

packagePrefix.LaTeXpackage <- function(x) {
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

packageSuffix.LaTeXpackage <- function(x) {
    x$suffix
}

packageSuffix.list <- function(x) {
    unlist(lapply(x, packageSuffix))
}

################################################################################
## Package initialisation
packageInit <- function(pkgs, state) {
    lapply(pkgs,
           function(x) {
               if (!is.null(x$init))
                   x$init(state)
           })
}

################################################################################
## Call package specials
packageSpecial <- function(pkgs, special, state) {
    lapply(pkgs,
           function(x) {
               if (!is.null(x$special))
                   x$special(special, state)
           })
}

################################################################################
## Call package finals
packageFinal <- function(pkgs, state) {
    lapply(pkgs,
           function(x) {
               if (!is.null(x$final))
                   x$final(state)
           })
}

################################################################################
## Add convenience for specifying packages

set("packageRegister", NULL)

registerPackage <- function(package) {
    register <- get("packageRegister")
    alias <- package$name
    existing <- names(register)
    if (!inherits(package, "LaTeXpackage"))
        stop("Invalid package.")
    if (length(alias) > 1)
        stop("Please only register one package at a time.")
    if (alias %in% existing)
        warning(sprintf("Package %s already registered.", alias))
    register[[alias]] <- package
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

resolvePackage.LaTeXpackage <- function(x, ...) {
    ## If user has explicitly specified a package as a LaTeXpackage
    ## (rather than an alias), and if that package has not yet been
    ## registered, register it.
    ## This means that specifying a package via author() or typeset(),
    ## which embed package name in signature, will resolve package name
    ## when signature is extracted.
    register <- get("packageRegister")
    existing <- names(register)
    if (!x$name %in% existing) {
        registerPackage(x)
    }
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

## Package can be specified as a single "LaTeXpackage"
resolvePackages.LaTeXpackage <- function(x, ...) {
    list(x)
}

## Packages can be specified as a list, consisting of either
## character alias or "LaTeXpackage"
resolvePackages.list <- function(x, ...) {
    lapply(x, resolvePackage)
}

## Check for package conflict between explicit user-specification ('x')
## and packages from typeset DVI ('pkgNames'), if any
checkPackages <- function(x, pkgNames, ...) {
    UseMethod("checkPackages")
}

## User says NULL so take 'pkgNames', if any
checkPackages.NULL <- function(x, pkgNames, ...) {
    if (!is.null(pkgNames) && any(nchar(pkgNames))) {
        resolvePackages(pkgNames)
    } else {
        NULL
    }
}

checkPkgMatch <- function(x, pkgNames) {
    if (!identical(x, pkgNames)) {
        if (any(!x %in% pkgNames)) {
            warning(paste0("Explicit packages ",
                           "(", paste(x[!x %in% pkgNames], collapse=", "), ") ",
                           "not in DVI"))
        }
        if (any(!pkgNames %in% x)) {
            warning(paste0("DVI packages ",
                           "(", paste(pkgNames[!pkgNames %in% x],
                                      collapse=", "), ") ",
                           "not in explicit packages"))            
        }
    }
}

## User says something, so take 'x', but warn if that does not match 'pkgNames'
checkPackages.character <- function(x, pkgNames, ...) {
    checkPkgMatch(x, pkgNames)
    x
}

## User says one package so take that, but warn if does not match 'pkgNames'
checkPackages.LaTeXpackage <- function(x, pkgNames, ...) {
    x <- x$name
    checkPkgMatch(x, pkgNames)
    x
}

## User says multiple packages, so take those, but warn if does not match
checkPackages.list <- function(x, pkgNames, ...) {
    x <- resolvePackages(x)
    xNames <- sapply(x, function(y) y$name)
    checkPackages(xNames, pkgNames)
    x
}
