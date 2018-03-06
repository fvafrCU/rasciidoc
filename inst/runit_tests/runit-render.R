if (interactive()) devtools::load_all()
is_installed_asciidoc <- function() return(rasciidoc:::is_installed("asciidoc"))
remove_dates <- function(x) {
    grep(".*CET$", value = TRUE, invert = TRUE,
         grep(".*UTC$", value = TRUE, invert = TRUE, x)
         )
}
probably_me <- function() {
    me <- Sys.info()[["nodename"]] %in% c("h6") &&
        .Platform[["OS.type"]] == "unix"

}

test_render_simple <- function() {
    folder  <- system.file("runit_tests", "files", package = "rasciidoc")
    file.copy(folder, tempdir(), recursive = TRUE)
    on.exit(unlink(file.path(tempdir(), "files"), recursive = TRUE))
    #% render
    withr::with_dir(file.path(tempdir(), "files"),
                    result <- rasciidoc::render("simple.Rasciidoc"))
    if (! is_installed_asciidoc()) {
        RUnit::checkTrue(! identical(result, as.integer(0)))
    } else {
        expectation  <- as.integer(0)
        RUnit::checkIdentical(result, expectation)
        result <- remove_dates(readLines(file.path(tempdir(), "files",
                                                   "simple.html")))
        expectation <- remove_dates(readLines(file.path(tempdir(), "files",
                                                        "expected",
                                                        "simple.html")))
        if (probably_me()) {
            RUnit::checkIdentical(result, expectation)
        } else {
            RUnit::checkTrue(any(grepl("Dominik Cullmann", result)))
        }
        #% render slides
        withr::with_dir(file.path(tempdir(), "files"),
                        rasciidoc::render_slides("simple.Rasciidoc"))
        result <- remove_dates(readLines(file.path(tempdir(), "files",
                                                   "simple.html")))
        expectation <- remove_dates(readLines(file.path(tempdir(), "files",
                                                        "expected",
                                                        "simple.html")))
        if (probably_me()) {
            RUnit::checkIdentical(result, expectation)
        } else {
            RUnit::checkTrue(any(grepl("Dominik Cullmann", result)))
        }
        # file contains no R code
        withr::with_dir(file.path(tempdir(), "files"),
                        rasciidoc::render("fake.Radoc", knit = NA))
        result <- remove_dates(readLines(file.path(tempdir(), "files",
                                                   "fake.html")))
        expectation <- remove_dates(readLines(file.path(tempdir(), "files",
                                                        "expected",
                                                        "fake.html")))
        if (probably_me()) {
            RUnit::checkIdentical(result, expectation)
        } else {
            RUnit::checkTrue(any(grepl("Dominik Cullmann", result)))
        }
    }

}

test_render_slides <- function() {
    folder  <- system.file("runit_tests", "files", package = "rasciidoc")
    file.copy(folder, tempdir(), recursive = TRUE)
    on.exit(unlink(file.path(tempdir(), "files"), recursive = TRUE))
    withr::with_dir(file.path(tempdir(), "files"),
                    result <- rasciidoc::render_slides("slides.Rasciidoc"))
    if (! is_installed_asciidoc()) {
        RUnit::checkTrue(! identical(result, as.integer(0)))
    } else {
        result <- remove_dates(readLines(file.path(tempdir(), "files",
                                                   "slides.html")))
        expectation <- remove_dates(readLines(file.path(tempdir(), "files",
                                                        "expected",
                                                        "slides.html")))
        if (probably_me()) {
            RUnit::checkIdentical(result, expectation)
        } else {
            RUnit::checkTrue(any(grepl("Dominik Cullmann", result)))
        }
        result <- remove_dates(readLines(file.path(tempdir(), "files",
                                                   "slides_slidy.html")))
        expectation <- remove_dates(readLines(file.path(tempdir(), "files",
                                                        "expected",
                                                        "slides_slidy.html")))
        if (probably_me()) {
            RUnit::checkIdentical(result, expectation)
        } else {
            RUnit::checkTrue(any(grepl("Dominik Cullmann", result)))

        }
    }
}

test_knit_spin <- function() {
    if (is_installed_asciidoc()) {
        withr::with_dir(tempdir(), {
                        file.copy(list.files(system.file("files", "simple",
                                                         package = "rasciidoc"),
                                             full.names = TRUE
                                             ),
                                  ".", recursive = TRUE)
                        # lintr inevitably reads spin.R and crashes (I tried all
                        # kindes of exlusions...). I therefore moved spin.R to
                        # spin.R_nolint to make lintr not read the file.
                        # But I need it to end on R or r when deciding whether
                        # to knit or spin. So I rename here:
                        file.rename("spin.R_nolint", "spin.R")
                        rasciidoc::render("spin.R")
                        file.copy("spin.md", "foo.md")
                        rasciidoc::render("foo.md")
                        spin <- remove_dates(readLines("spin.html"))
                        ascii_md <- remove_dates(readLines("foo.html"))
                        RUnit::checkIdentical(spin, ascii_md)
                        rasciidoc::render("knit.Rasciidoc")
                        file.copy("knit.asciidoc", "bar.asciidoc")
                        rasciidoc::render("bar.asciidoc")
                        knit <- remove_dates(readLines("knit.html"))
                        ascii <- remove_dates(readLines("bar.html"))
                        RUnit::checkIdentical(knit, ascii)
                    })
    }
}

test_adjusting_hooks <- function() {
    on.exit( knitr::knit_hooks$restore())
    # covr infects functions, so we deparse an grep them first
    hs <- gsub(" ", "",
               deparse(function(x, options) {
                           x = paste(c(hilight_source(x, "asciidoc", options), ""),
                                     collapse = "\n")
                           sprintf("\n[source,%s]\n----\n%s----\n", tolower(options$engine),
                                   x) }))
    hm <- gsub(" ", "",
               deparse(function(x, options) {
                           sprintf("\n[NOTE]\n====\n.Message\n%s\n====\n",
                                   substring(x, comment_length(options$comment)))}))
    hw <- gsub(" ", "",
               deparse(function(x, options) {
                           sprintf("\n[WARNING]\n====\n.Warning\n%s\n====\n",
                                   gsub("^.*Warning: ", "", x))}))
    he <- gsub(" ", "",
               deparse(function(x, options) {
                           sprintf("\n[CAUTION]\n====\n.Error\n%s\n====\n",
                                   gsub("^.*Error: ", "", x))}))
    ho <- gsub(" ", "",
               deparse(function(x, options) sprintf("\n----\n%s----\n", x)))
    knitr::knit_hooks$restore()
    rasciidoc::adjust_asciidoc_hooks(replacement = NULL)
    cs <- gsub(" ", "",
               grep("covr:::count|   \\{|   \\}", invert = TRUE, value = TRUE,
                    deparse(knitr::knit_hooks$get("source")))
               )
    co <- gsub(" ", "",
               grep("covr:::count|   \\{|   \\}", invert = TRUE, value = TRUE,
                    deparse(knitr::knit_hooks$get("output")))
               )
    cm <- gsub(" ", "",
               grep("covr:::count|   \\{|   \\}", invert = TRUE, value = TRUE,
                    deparse(knitr::knit_hooks$get("message")))
               )
    cw <- gsub(" ", "",
               grep("covr:::count|   \\{|   \\}", invert = TRUE, value = TRUE,
                    deparse(knitr::knit_hooks$get("warning")))
               )
    ce <- gsub(" ", "",
               grep("covr:::count|   \\{|   \\}", invert = TRUE, value = TRUE,
                    deparse(knitr::knit_hooks$get("error")))
               )
    RUnit::checkEquals(hs, cs)
    RUnit::checkEquals(ho, co)
    RUnit::checkEquals(hm, cm)
    RUnit::checkEquals(hw, cw)
    RUnit::checkEquals(he, ce)
    rasciidoc::adjust_asciidoc_hooks(hooks = c("message", "warning", "error"),
                                     replacement = "source")
    cs <- gsub(" ", "",
               grep("covr:::count|   \\{|   \\}", invert = TRUE, value = TRUE,
                    deparse(knitr::knit_hooks$get("source")))
               )
    co <- gsub(" ", "",
               grep("covr:::count|   \\{|   \\}", invert = TRUE, value = TRUE,
                    deparse(knitr::knit_hooks$get("output")))
               )
    cm <- gsub(" ", "",
               grep("covr:::count|   \\{|   \\}", invert = TRUE, value = TRUE,
                    deparse(knitr::knit_hooks$get("message")))
               )
    cw <- gsub(" ", "",
               grep("covr:::count|   \\{|   \\}", invert = TRUE, value = TRUE,
                    deparse(knitr::knit_hooks$get("warning")))
               )
    ce <- gsub(" ", "",
               grep("covr:::count|   \\{|   \\}", invert = TRUE, value = TRUE,
                    deparse(knitr::knit_hooks$get("error")))
               )
    RUnit::checkEquals(hs, cs)
    RUnit::checkEquals(ho, co)
    RUnit::checkEquals(hs, cm)
    RUnit::checkEquals(hs, cw)
    RUnit::checkEquals(hs, ce)
    rasciidoc::adjust_asciidoc_hooks(hooks = c("source"), replacement = "error")
    cs <- gsub(" ", "",
               grep("covr:::count|   \\{|   \\}", invert = TRUE, value = TRUE,
                    deparse(knitr::knit_hooks$get("source")))
               )
    RUnit::checkEquals(he, cs)
}
