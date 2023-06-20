
initTinyTeX <- function() {
    if (nchar(system.file(package="tinytex"))) {
        message(paste0("tinytex:  ", packageVersion("tinytex")))
        options("xdvir.tinytex"=TRUE)
        ## Set luaOTFloadToolVersion
        if (.Platform$OS.type == "windows") {
            cmd <- paste(file.path(tinytex::tinytex_root(), "bin", "windows",
                                   "luaotfload-tool"),
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
        message("tinytex:  not installed")
        options("xdvir.tinytex"=FALSE)
    }
}
