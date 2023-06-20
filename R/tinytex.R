
initTinyTeX <- function() {
    useTinyTeX <- getOption("xdvir.tinytex")
    haveTinyTeX <- nchar(system.file(package="tinytex"))
    if (haveTinyTeX) {
        message(paste0("  tinytex:  ", packageVersion("tinytex")))
    } else {
        message("  tinytex:  not installed")
    }
    if (is.null(useTinyTeX)) {
        if (haveTinyTeX) {
            options("xdvir.tinytex"=TRUE)
            ## Set luaOTFloadToolVersion
            if (.Platform$OS.type == "windows") {
                cmd <- paste(file.path(tinytex::tinytex_root(), "bin",
                                       "windows", "luaotfload-tool"),
                             "--version")
                loadToolText <- shell(cmd, intern=TRUE)
            } else {
                loadToolText <- system("luaotfload-tool --version", intern=TRUE)
            }
            loadToolVersLine <- grep("luaotfload-tool version", loadToolText)
            loadToolVers <- gsub('.+"', "",
                                 gsub('"$', "", loadToolText[loadToolVersLine]))
            set("luaOTFloadToolVersion", loadToolVers)
        } else {
            options("xdvir.tinytex"=FALSE)
        }
    }
}
