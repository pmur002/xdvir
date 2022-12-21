
initTTX <- function() {
    ttx <- Sys.which("ttx")
    if (nchar(ttx) == 0)
        stop("Failed to find ttx; please install fonttools")
}

################################################################################
## TTX cache (to avoid loading TTX files over and over again)

getTTXcache <- function() {
    cache <- get("ttxCache")
    if (is.null(cache))
        set("ttxCache", list())
    cache
}

cacheTTX <- function(ttxFile, ttx) {
    cache <- getTTXcache()
    cache[[ttxFile]] <- ttx
    set("ttxCache", cache)
}

getTTX <- function(ttxFile) {
    cache <- getTTXcache()
    if (is.null(cache) ||
        is.null(cache[[ttxFile]])) {
        ttx <- read_xml(ttxFile)
        cacheTTX(ttxFile, ttx)        
    } else {
        ttx <- cache[[ttxFile]]
    }
    ttx
}

################################################################################
## Take copy of font file and place it in tmp directory for taking apart
## (and get font name and suffix)

ttxFontDir <- function() {
    fontDir <- "TTXfonts"
    tmpFontDir <- file.path(tempdir(), fontDir)
    if (!dir.exists(tmpFontDir)) {
        dir.create(tmpFontDir)
    }
    tmpFontDir
}

ttxFontFile <- function(fontpath) {
    tmpFontDir <- ttxFontDir()
    filename <- basename(fontpath)
    fontsuffix <- "[.](ttf|otf)$"
    if (!grepl(fontsuffix, filename))
        warning("Unrecognised font suffix")
    filesuffix <- gsub(paste0(".+", fontsuffix), "\\1", filename)
    filestub <- gsub(fontsuffix, "", filename)
    fontfile <- file.path(tmpFontDir, filename)
    if (!file.exists(fontfile)) {
        file.copy(fontpath, tmpFontDir)
    }
    list(file=fontfile, suffix=filesuffix)
}

################################################################################
## Get a single table
getTable <- function(table, fontfile, suffix, replace=table) {
    ttxfile <- gsub(paste0("[.]", suffix, "$"),
                    paste0("-", replace, ".ttx"), fontfile)
    if (!file.exists(ttxfile)) {
        system(paste0("ttx -t ", table, " -o ", ttxfile, " ", fontfile))
    }
    getTTX(ttxfile)
}

getHeadTable <- function(fontfile, suffix) {
    getTable("head", fontfile, suffix)
}

getHHeaTable <- function(fontfile, suffix) {
    getTable("hhea", fontfile, suffix)
}

getGlyphOrderTable <- function(fontfile, suffix) {
    getTable("GlyphOrder", fontfile, suffix)
}

getHmtxTable <- function(fontfile, suffix) {
    getTable("hmtx", fontfile, suffix)
}

getVmtxTable <- function(fontfile, suffix) {
    getTable("vmtx", fontfile, suffix)
}

getOS2Table <- function(fontfile, suffix) {
    getTable("OS/2", fontfile, suffix, "OS2")
}

getHHeaTable <- function(fontfile, suffix) {
    getTable("hhea", fontfile, suffix)
}

getGlyfTable <- function(fontfile, suffix) {
    getTable("glyf", fontfile, suffix)
}

getNameTable <- function(fontfile, suffix) {
    getTable("name", fontfile, suffix)
}

getCMapTable <- function(fontfile, suffix) {
    getTable("cmap", fontfile, suffix)
}

################################################################################
## TTX implementations of font-api.R functions

ttxFontFamily <- function(file) {
    font <- ttxFontFile(file)
    nameTable <- getNameTable(font$file, font$suffix)
    gsub("^[[:space:]]+|[[:space:]]+$", "",
         xml_text(xml_find_first(nameTable, "//namerecord[@nameID = '1']")))    
}

ttxFontWeight <- function(file) {
    font <- ttxFontFile(file)
    OS2 <- getOS2Table(font$file, font$suffix)
    weight <- xml_text(xml_find_first(OS2, "//usWeightClass/@value"))
    if (is.na(weight))
        400
    else
        as.numeric(weight)
}

ttxFontStyle <- function(file) {
    font <- ttxFontFile(file)
    hhea <- getHHeaTable(font$file, font$suffix)
    slopeRun <- xml_text(xml_find_first(hhea, "//caretSlopeRun/@value"))
    if (is.na(slopeRun) || slopeRun == "0")
        "normal"
    else
        "italic"
}

ttxGlyphIndex <- function(name, file) {
    font <- ttxFontFile(file)
    ## Find glyph index
    glyphs <- getGlyphOrderTable(font$file, font$suffix)
    ## Try more than one name (if there are multiple options)
    glyph <- NULL
    while (!length(glyph) && length(name)) {
        glyphPath <- paste0("//GlyphID[@name = '", name, "']")
        glyph <- xml_find_first(glyphs, glyphPath)
        name <- name[-1]
    }
    xml_attr(glyph, "id")
}

## Convert 4-digit hex code to glyph name via font Unicode mapping
ttxGlyphNameFromUNICODE <- function(code, file) {
    font <- ttxFontFile(file)
    cmap <- getCMapTable(font$file, font$suffix)
    ## cmap table with platformID="0" is UNICODE mapping
    ## NOTE: this may need relaxing to allow for other table formats
    ##       (e.g., cmap_format_6)
    unicodeMap <- paste0("//cmap_format_4[@platformID = '0']/map[@code = '",
                         code, "']")
    name <- xml_attr(xml_find_first(cmap, unicodeMap), "name")
    if (length(name)) {
        name
    } else {
        NULL
    }   
}

ttxGlyphWidth <- function(name, file) {
    font <- ttxFontFile(file)
    headTTX <- getHeadTable(font$file, font$suffix)
    unitsPerEm <- as.numeric(xml_text(xml_find_first(headTTX,
                                                     "//unitsPerEm/@value")))
    hmtx <- getHmtxTable(font$file, font$suffix)
    ## Try more than one name (if there are multiple options)
    glyph <- NULL
    while (!length(glyph) && length(name)) {
        glyphPath <- paste0("//mtx[@name = '", name[1], "']")
        glyph <- xml_find_first(hmtx, glyphPath)
        name <- name[-1]
    }
    width <- as.numeric(c(xml_attr(glyph, "width"),
                          xml_attr(glyph, "lsb")))
    ## round() to get whole number metrix (at 1000 scale)
    ## floor() to match what PDF_StrWidthUTF8() does
    fontsize <- 10
    cex <- 1
    widthPts <- floor(fontsize + .5)*cex*(round(width/(unitsPerEm/1000)))/1000
    xtoTeX(unit(widthPts, "bigpts"))
}

ttxGlyphHeight <- function(name, file) {
    font <- ttxFontFile(file)
    headTTX <- getHeadTable(font$file, font$suffix)
    unitsPerEm <- as.numeric(xml_text(xml_find_first(headTTX,
                                                     "//unitsPerEm/@value")))
    vmtx <- getVmtxTable(font$file, font$suffix)
    ## Try more than one name (if there are multiple options)
    glyph <- NULL
    while (!length(glyph) && length(name)) {
        glyphPath <- paste0("//mtx[@name = '", name[1], "']")
        glyph <- xml_find_first(vmtx, glyphPath)
        name <- name[-1]
    }
    height <- as.numeric(c(xml_attr(glyph, "height"),
                           xml_attr(glyph, "tsb")))
    ## round() to get whole number metrix (at 1000 scale)
    ## floor() to match what PDF_StrWidthUTF8() does
    fontsize <- 10
    cex <- 1
    heightPts <- floor(fontsize + .5)*cex*(round(height/(unitsPerEm/1000)))/1000
    xtoTeX(unit(heightPts, "bigpts"))
}

ttxGlyphMetrics <- function(name, file) {
    font <- ttxFontFile(file)
    head <- getHeadTable(font$file, font$suffix)
    unitsPerEm <- as.numeric(xml_text(xml_find_first(head,
                                                     "//unitsPerEm/@value")))
    glyf <- getGlyfTable(font$file, font$suffix)
    ## Try more than one name (if there are multiple options)
    glyph <- NULL
    while (!length(glyph) && length(name)) {
        glyphPath <- paste0("//TTGlyph[@name = '", name[1], "']")
        glyph <- xml_find_first(glyf, glyphPath)
        name <- name[-1]
    }
    if (is.na(glyph)) {
        ## e.g., no 'glyf' table, e.g., CFF font
        ## Just use font metrics
        warning("Glyph metric info not available;  using font metric info")
        bbox <- as.numeric(c(xml_attr(xml_find_first(head, "//xMin"), "value"),
                             xml_attr(xml_find_first(head, "//xMax"), "value"),
                             xml_attr(xml_find_first(head, "//yMin"), "value"),
                             xml_attr(xml_find_first(head, "//yMax"), "value")))
    } else {
        bbox <- as.numeric(c(xml_attr(glyph, "xMin"),
                             xml_attr(glyph, "xMax"),
                             xml_attr(glyph, "yMin"),
                             xml_attr(glyph, "yMax")))
    }
    ## round() to get whole number metrix (at 1000 scale)
    ## floor() to match what PDF_StrWidthUTF8() does
    fontsize <- 10
    cex <- 1
    bboxPts <- floor(fontsize + .5)*cex*(round(bbox/(unitsPerEm/1000)))/1000
    xtoTeX(unit(bboxPts, "bigpts"))
}
