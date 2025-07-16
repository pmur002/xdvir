
## Report on TeX installation and engine versions

TeXstatus <- function() {
    msg <- NULL
    width <- 15
    line <- function(prompt="", x="") {
        sprintf(paste0("%", width, "s:  %s"), prompt, x)
    }
    if (haveTeX()) {
        msg <- c(msg,
                 line("TeX", texVersion()))
        if (xetexAvailable()) {
            msg <- c(msg,
                     line("xetex", xetexVersion()))
        } else {
            msg <- c(msg,
                     line("xetex", "Not found"),
                     line("", "The XeTeX engine is NOT available."))
        }
        if (luatexAvailable()) {
            msg <- c(msg,
                     line("luatex", luatexVersion()))
            if (luaOTFloadToolAvailable()) {
                msg <- c(msg,
                         line("luaotfload-tool", luaOTFloadToolVersion()))
                if (!luaOTFloadToolSufficient()) {
                    msg <- c(msg,
                             line("",
                                  "The luaotfload-tool version is too low (< 3.15)."))
                    msg <- c(msg,
                             line("",
                                  "The LuaTeX engine is NOT available."))
                }
            } 
        } else {
            msg <- c(msg,
                     line("luatex", "Not found."),
                     line("", "The LuaTeX engine is NOT available."))
        }
    } else {
        msg <- c(msg,
                 line("TeX", "Not found."),
                 line("",
                      "No TeX installation detected (see ?tinytex::install_tinytex)."))
    }
    if (!(haveTeX() &&
          any(sapply(get("engines"), canTypeset)))) {
        msg <- c(msg,
                 line("", "Typesetting is NOT available."))
    }
    cat(msg, sep="\n")
}
