
set("engines", list())

## Create a TeX engine
TeXengine <- function(name,
                      version,
                      command,
                      isEngine,
                      options=NULL,
                      preamble="",
                      dviSuffix=".dvi") {
    engine <- list(name=name,
                   version=version,
                   command=command,
                   isEngine=isEngine,
                   options=options,
                   preamble=preamble,
                   dviSuffix=dviSuffix)
    class(engine) <- "TeXengine"
    engine
}

## Register an engine with {xdvir}
registerEngine <- function(engine) {
    if (!inherits(engine, "TeXengine"))
        stop("Invalid engine")
    engines <- get("engines")
    registered <- names(engines)
    if (length(registered) &&
        engine$name %in% registered) {
        warning("TeX engine already registered")
    } else {
        newEngine <- list(engine)
        names(newEngine) <- tolower(engine$name)
        set("engines",
            c(get("engines"), newEngine))
    }
}

## Check whether engine 'e1' (possibly NULL) matches engine 'e2'
engineCheck <- function(e1, e2) {
    if (is.null(e1)) {
        warning(paste0("Unknown engine; ",
                       "possible mismatch with ", e2$name))
    } else {
        if (e1$name != e2$name) {
            warning(paste0("TeX engine mismatch ",
                           "(", e1$name, " does not match ", e2$name, ")"))
        } else {
            if (e1$version != e2$version) {
                warning(paste0("TeX engine ", e1$name, " version mismatch ",
                               "(", e1$version, "does not match",
                               e2$version, ")"))
            }
        }
    }
}

## Given ...
## 1. object that might have engine information
## 2. user-specified engine (may be NULL)
## ... figure out what engine to use
## AND report any mismatches if both 1. and 2. are non-NULL
resolveEngine <- function(x, engine) {
    UseMethod("resolveEngine")
}

## Resolve for TeXdocument, which has an "engine" attribute.
## If no user engine, use "engine" attribute
## If user specifies engine, check for mismatches, but user engine wins.
resolveEngine.LaTeXdocument <- function(x, engine) {
    e1 <- authorEngine(x)
    if (is.null(engine)) {
        e1
    } else {
        engine <- getEngine(engine)
        engineCheck(e1, engine)
        engine
    }
}

## Resolve for character input, which is TeX code,
## which MAY include "engine" comment.
## If TeX code does not include "engine" comment, use non-NULL user engine.
## If neither provides an engine, default to "xdvir.engine" option.
## If TeX code does include "engine" comment, 
##   if no user engine, use "engine" attribute
##   if user specifies engine, check for mismatches, but user engine wins.
resolveEngine.character <- function(x, engine) {
    e1 <- authorEngine(x)
    if (is.null(e1)) {
        warning("Unknown author TeX engine; typeset TeX engine may not match")
        if (is.null(engine)) {
            getOption("xdvir.engine")
        } else {
            getEngine(engine)
        }
    } else {
        if (is.null(engine)) {
            e1
        } else {
            engine <- getEngine(engine)
            engineCheck(e1, engine)
            engine
        }
    }
}

## Resolve for DVI object, which MAY include "engine" comment in pre op.
## If pre op does not include "engine" comment, use non-NULL user engine.
## If neither provides an engine, default to "xdvir.engine" option.
## If DVI does include "engine" comment, 
##   if no user engine, use "engine" attribute
##   if user specifies engine, check for mismatches, but user engine wins.
resolveEngine.DVI <- function(x, engine) {
    e1 <- typesetEngine(x)
    if (is.null(e1)) {
        warning("Unknown typeset TeX engine; render TeX engine may not match")
        if (is.null(engine)) {
            getOption("xdvir.engine")
        } else {
            getEngine(engine)
        }
    } else {
        if (is.null(engine)) {
            e1
        } else {
            engine <- getEngine(engine)
            engineCheck(e1, engine)
            engine
        }
    }
}

## Get TeXengine from engine name (or TeX engine)
getEngine <- function(engine) {
    UseMethod("getEngine")
}

getEngine.TeXengine <- function(engine) {
    engine
}

getEngine.character <- function(engine) {
    if (length(engine) != 1)
        stop("Can only get exactly one engine")
    engine <- tolower(engine)
    engines <- get("engines")
    if (engine %in% names(engines)) {
        engines[[engine]]
    } else {
        stop(paste0("Engine ", engine, " not found"))
    }
}

print.TeXengine <- function(x, ...) {
    cat(paste0(x$name, " (", x$version, ")\n"))
}
