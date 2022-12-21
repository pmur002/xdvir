
initFC <- function() {
    fcmatch <- Sys.which("fc-match")
    if (nchar(fcmatch) == 0)
        stop("Failed to find fc-match; please install fontconfig")
}
