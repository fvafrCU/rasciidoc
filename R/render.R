is_installed <- function(program) {
    is_installed <- nchar(Sys.which(program)) > 0
    attr(is_installed, "names") <- NULL
    return(is_installed)
}

#' Render an `asciidoc` File
#'
#' This is the basic interface to `asciidoc`. Not more than a call to
#' \code{\link{system2}} and checks on `asciidoc` and `source-highlight`.
#' You should usually not call it directly, see
#' \code{\link{render}} and \code{\link{render_slides}} for wrappers.
#'
#' @param file_name The file to run `asciidoc` on.
#' @param ... arguments passed to `asciidoc` via \code{\link{system2}}.
#' @return \code{\link[base:invisible]{Invisibly}} `asciidoc`'s return value.
#' @export
rasciidoc <- function(file_name, ...) {
    if (! is_installed("asciidoc"))
        warning("Can't find program `asciidoc`. ",
                "Please install first (www.asciidoc.org).")
    if (! is_installed("source-highlight"))
        warning("Can't find program `source-highlight`.")
    status <- system2("asciidoc", args = c(..., file_name))
    return(invisible(status))
}

run_knitr <- function(file_name, knit = NA, adjust_hooks = TRUE,
                      envir = parent.frame()) {
    if (isTRUE(adjust_hooks)) adjust_asciidoc_hooks()
    if (is.na(knit)) {
        r_code_pattern <- "//begin.rcode"
        if (any(grepl(r_code_pattern, readLines(file_name)))) {
            knit <- TRUE
            warning("Setting option knit to TRUE based on the file contents!")
        }
    }
    if (is.na(knit)) {
        if (grepl("\\.R.*$", file_name)) {
            knit <- TRUE
            warning("Setting option knit to TRUE based on the file name given!")
        }
    }
    if (isTRUE(knit)) {
        knit_out_file <- sub("\\.R(.*)", ".\\1", file_name)
        file_name <- knitr::knit(file_name, knit_out_file, envir = envir)
        options(warn = 0) ## knitr changes the options?!
    }
    return(file_name)
}

#' Knit and Render an `asciidoc` File
#'
#' Knit (if required) and render an `asciidoc` file.
#' @inheritParams rasciidoc
#' @param knit Knit the file first using \code{\link[knitr:knit]{knitr::knit}}?
#' If set to \code{\link{NA}}, knitting is based on the file's contents or name.
#' Set to \code{\link{TRUE}}
#' to force knitting or to \code{\link{FALSE}} (anything apart from
#' \code{\link{TRUE}} or \code{\link{NA}}, really), to
#' disable knitting.
#' @param envir The frame in which to render.
#' @param adjust_hooks Adjust knitr's output hooks for `asciidoc` files using
#' the defaults of \code{\link{adjust_asciidoc_hooks}}?
#' @return The return value of \code{\link{rasciidoc}}.
#' @export
render <- function(file_name, knit = NA, adjust_hooks = TRUE,
                   envir = parent.frame(), ...) {
    adoc <- run_knitr(file_name, knit = knit, adjust_hooks = adjust_hooks,
                      envir = envir)
    status <- rasciidoc(adoc, ...)
    return(status)
}

#' Knit and Render an `asciidoc` File to html and slidy
#'
#' You can exclude parts of the file from the standard html or slidy output by
#' using lines starting with '//begin_only_slide' and '//end_only_slide' or
#' '//begin_no_slide' and '//end_no_slide', respectively. To exclude single
#' lines from standard html output, append a '//slide_only' comment to it (for
#' example to add slide titles for slidy to break longer sections of standard
#' html output.
#'
#' @inheritParams render
#' @return The output's file names.
#' @export
#' @examples
#' folder  <- system.file("runit_tests", "files", package = "rasciidoc")
#' file.copy(folder, tempdir(), recursive = TRUE)
#' files <- withr::with_dir(file.path(tempdir(), "files"),
#'                          rasciidoc::render_slides("slides.Rasciidoc"))
#' # files are in tempdir()/files/:
#' files <- file.path(tempdir(), "files", files)
#' print(files)
#' \dontrun{
#'     browseURL(files[1])
#'     browseURL(files[2])
#' }
render_slides <- function(file_name, knit = NA, adjust_hooks = TRUE,
                          envir = parent.frame()) {
    status <- NULL
    out_files <- NULL
    adoc <- run_knitr(file_name, knit = knit, adjust_hooks = adjust_hooks,
                      envir = envir)
    basename <- sub("\\..*", "", adoc)
    out_file <- paste0(basename, ".html")
    slide_only_pattern <- "//slide_only"
    begin_pattern <- "^//end_only_slide"
    if (any(grepl(begin_pattern, readLines(adoc))) ||
        any(grepl(slide_only_pattern, readLines(adoc)))) {
        glbt <- document::get_lines_between_tags
        excerpt <- glbt(adoc, keep_tagged_lines = TRUE,
                        begin_pattern = begin_pattern,
                        end_pattern = "^//begin_only_slide",
                        from_first_line = TRUE, to_last_line = TRUE)
        excerpt <- grep(slide_only_pattern, excerpt, invert = TRUE,
                        value = TRUE)
        # The asciidoc file has to be _here_ for include::-macros to work!
        excerpt_file <- file.path(dirname(file_name),
                                  basename(tempfile(fileext = ".asciidoc")))
        writeLines(excerpt, excerpt_file)
        status <- c(status, rasciidoc(excerpt_file, paste("-o", out_file)))
        file.remove(excerpt_file)
    } else {
        status <- c(status, rasciidoc(adoc, paste("-o", out_file)))
    }
    out_files <- c(out_files, out_file)
    begin_pattern <- "^//end_no_slide"
    if (any(grepl(begin_pattern, readLines(adoc)))) {
        glbt <- document::get_lines_between_tags
        excerpt <- glbt(adoc, keep_tagged_lines = TRUE,
                        begin_pattern = begin_pattern,
                        end_pattern = "^//begin_no_slide",
                        from_first_line = TRUE, to_last_line = TRUE)
        excerpt <- sub(paste0(slide_only_pattern, ".*"), "", excerpt)
        excerpt <- sub("(:numbered:)", "// \\1", excerpt)
        # The asciidoc file has to be _here_ for include::-macros to work!
        excerpt_file <- file.path(dirname(file_name),
                                  basename(tempfile(fileext = ".asciidoc")))
        writeLines(excerpt, excerpt_file)
        out_file <- paste0(basename, "_slidy.html")
        status <- c(status, rasciidoc(excerpt_file, "-b slidy",
                                      paste("-o", out_file)))
        file.remove(excerpt_file)
        out_files <- c(out_files, out_file)
    }
    return(out_files[status == 0])
}
