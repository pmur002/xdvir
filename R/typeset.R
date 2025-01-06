
## Generate DVI from TeX string


set("DVIcache", list())

typesetKey <- function(tex, engine, packages) {
    paste(paste(tex, collapse=""),
          engine$name,
          paste(packages, collapse=","),
          sep=":::")
}

addDVI <- function(tex, engine, packages, texFile, sig) {
    if (is.null(texFile)) {
        texFile <- tempfile(fileext=".tex")
    }
    texDir <- dirname(texFile)
    dviFile <- paste0(gsub("[.]tex", "", texFile), engine$dviSuffix)
    writeLines(tex, texFile)
    latex(texFile, texDir, engine, packages, dviFile, sig)
    dvi <- readDVI(dviFile)
    attr(dvi, "file") <- dviFile
    cache <- get("DVIcache")
    key <- typesetKey(tex, engine, packages)
    cache[[key]] <- dvi
    set("DVIcache", cache)
    dvi
}

getDVI <- function(tex, engine, packages, texFile, sig=TRUE) {
    key <- typesetKey(tex, engine, packages)
    cache <- get("DVIcache")
    dvi <- cache[[key]]
    if (is.null(dvi)) {
        addDVI(tex, engine, packages, texFile, sig)
    } else {
        dvi
    }
}

canTypeset <- function(engine=getOption("xdvir.engine")) {
    !is.null(engine$command)
}

typeset <- function(tex,
                    engine=getOption("xdvir.engine"),
                    ...) {
    UseMethod("typeset")
}

latex <- function(file, dir, engine, packages, dviFile, sig=TRUE) {
    engine <- getEngine(engine)
    if (!canTypeset(engine)) {
        stop(paste0("The ", engine$name,
                    " engine does not support typesetting"))
    }
    if (sig) {
        sig <- buildSignature(engine, packages)
        options <- c(engine$options,
                     paste0('--output-comment="', sig, '"'),
                     shQuote(paste0("--output-directory=", dir)))
    } else {
        options <- c(engine$options,
                     shQuote(paste0("--output-directory=", dir)))
    }
    options(tinytex.output_dir=dir)
    if (tinytexVersion() > "0.54") {
        ## A bit of jumping through hoops to use output-directory successfully
        ## 1.  Set options(tinytex.output) below
        ## 2.  Use shQuote() in 'options' above to match internal call
        ##     in latexmk()
        latexmk(file,
                engine=engine$command,
                engine_args=options)
    } else {
        ## Have to run within try() because tinytex::latexmk() will only
        ## produce .dvi without error if engine="latex" (hard coded)
        ## (it will still generate .dvi with other engines, it will
        ##  just error out because it cannot find .pdf file)
        try(latexmk(file,
                    engine=engine$command,
                    engine_args=options),
            silent=TRUE)
        ## Do my own check that LaTeX run worked
        if (!file.exists(dviFile)) {
            stop(paste0("Typesetting failed; check ", file,
                        " and associated .log file"))
        }
    }
}

## 'x' is a "LaTeXdocument" from author()
typeset.LaTeXdocument <- function(tex,
                                  engine=NULL,
                                  texFile=NULL,
                                  ...) {
    engine <- resolveEngine(tex, engine)
    packages <- authorPackages(tex)
    getDVI(tex, engine, packages, texFile)
}

## 'x' is LaTeX code
typeset.character <- function(tex,
                              engine=NULL,
                              texFile=NULL,
                              ## Did R generate the TeX file? (assume no)
                              sig=FALSE, 
                              ...) {
    if (length(tex) < 1)
        stop("No LaTeX fragment to typeset")
    engine <- resolveEngine(tex, engine)
    packages <- authorPackages(tex)
    getDVI(tex, engine, packages, texFile, sig)
}
                    
## What engine was used to typeset the TeX code?
typesetEngine <- function(x) {
    UseMethod("typesetEngine")
}

typesetEngine.DVI <- function(x) {
    commentStr <- commentString(x)
    commentLine <- commentLine(commentStr)
    if (length(commentLine)) {
        ## If latex::typeset() produced the DVI then engine should be
        ## in comment of DVI preamble
        engine <- signatureEngine(commentLine)
        getEngine(engine$name)
    } else {
        warning("Guessing typesetting engine from DVI pre op comment")
        ## Try to guess from DVI pre op comment
        engines <- get("engines")
        isEngine <- sapply(engines, function(y) y$isEngine(x))
        if (any(isEngine)) {
            if (sum(isEngine) > 1) {
                warning(paste0("More than one engine identified ",
                               "(", paste(sapply(engines[isEngine],
                                                 function(x) x$name),
                                          collapse=", "), "); ",
                               "using the first match ",
                               "(", engines[[which(isEngine)[1]]]$name, ")"))
            }
            engines[[which(isEngine)[1]]]
        } else {
            warning(paste0("Unable to identify engine from DVI pre op comment ",
                           "(", commentStr, "); ",
                           "falling back to null engine"))
            engines[["null"]]
        }
    }    
}

typesetPackages <- function(x) {
    UseMethod("typesetPackages")
}

typesetPackages.DVI <- function(x) {
    commentStr <- commentString(x)
    commentLine <- commentLine(commentStr)
    if (length(commentLine)) {
        ## If latex::typeset() produced the DVI then engine should be
        ## in comment of DVI preamble
        signaturePackages(commentLine)
    } else {
        ## FIXME: could guess from prefixes in DVI specials ?
        warning(paste("No packages in DVI;",
                      "possible package mismatch with rendering engine"))
        NULL
    }    
}
