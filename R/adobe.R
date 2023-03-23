
AdobeGlyphList <- read.table(system.file("adobe", "glyphlist.txt",
                                         package="xdvir"),
                             sep=";", col.names=c("name", "code"),
                             stringsAsFactors=FALSE)

## Convert 4-digit hex code to glyph name
AdobeName <- function(code) {
    AdobeGlyphList$name[AdobeGlyphList$code == code]    
}

