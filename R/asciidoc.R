#' Run `asciidoc` on a File
#'
#' This is the basic interface to `asciidoc`. Not more than a call to
#' \code{\link{system2}} and checks on `asciidoc` and `source-highlight`.
#'
#' @param file_name The file to run `asciidoc` on.
#' @param ... arguments passed to `asciidoc` via \code{\link{system2}}.
#' @return Invisibly \code{NULL}.
.asciidoc <- function(file_name, ...) {
    if (nchar(Sys.which("asciidoc")) == 0)
        stop("Can't find program `asciidoc`.")
    if (nchar(Sys.which("source-highlight")) == 0)
        ("Can't find program `source-highlight`.")
    system2("asciidoc", args = c(..., file_name))
    return(invisible(NULL))
}

#' Run `asciidoc` on a File
#' 
#' Wrapper to .asciidoc using html and slidy as output.
#'
#' @param file_name The file to run `asciidoc` on.
#' @param knit Knit the file first using \code{\link[knitr:knit]{knitr::knit}}?
#' If set to NA, the file's name is checked for the sequence `.R` and if found,
#' knit is set to TRUE. Set to anything apart from \code{NA} to avoid
#' auto-checking the file's name.
#' @param ... arguments passed to `.asciidoc`.
#' @return The output's file names.
#' @export
asciidoc <- function(file_name, ..., knit = NA) {
    basename <- sub("\\..*", "", file_name)
    out_files <- NULL
    if (is.na(knit)) {
        if (grepl("\\.R", file_name)) {
            knit <- TRUE
            warning("Setting option knit to TRUE based on the file name given!")
        }
    }
    if (isTRUE(knit)) {
        knit_out_file <- sub("\\.R(.*)", ".\\1", file_name)
        file_name <- knitr::knit(file_name, knit_out_file)

    }
    out_file <- paste0(basename, ".html")
    .asciidoc(file_name, paste("-o", out_file))
    out_files <- c(out_files, out_file)
    begin_no_slidy_pattern <- "//end_no_slide"
    if (any(grepl(begin_no_slidy_pattern, readLines(file_name)))) {
        sl <- document::get_lines_between_tags(file_name, keep_tagged_lines = TRUE,
                                               begin_pattern = begin_no_slidy_pattern, 
                                               end_pattern = "//begin_no_slide",
                                               from_first_line = TRUE, 
                                               to_last_line = TRUE)
        slide_file <- tempfile()
        sl <- sub("(:numbered:)", "// \\1", sl) 
        writeLines(sl, slide_file)
        out_file <- paste0(basename, "_slidy.html")
        .asciidoc(slide_file, "-b slidy", paste("-o", out_file))
        out_files <- c(out_files, out_file)
    }
    return(out_files)
}

