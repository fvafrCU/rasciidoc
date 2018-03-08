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
                        rasciidoc::rasciidoc("foo.md")
                        spin <- remove_dates(readLines("spin.html"))
                        ascii_md <- remove_dates(readLines("foo.html"))
                        RUnit::checkIdentical(spin, ascii_md)
                        rasciidoc::render("knit.Rasciidoc")
                        file.copy("knit.asciidoc", "bar.asciidoc")
                        rasciidoc::rasciidoc("bar.asciidoc")
                        knit <- remove_dates(readLines("knit.html"))
                        ascii <- remove_dates(readLines("bar.html"))
                        RUnit::checkIdentical(knit, ascii)
                    })
    }
}

test_adjusting_hooks <- function() {
    # if this fails, insert prints or messages,
    # run covr::package_coverage(path = ".", quiet = FALSE), read
    # the output, it will point you to something like
    # /tmp/RtmpXXX/R_LIBSXXX/rasciidoc/rasciidoc-tests/runit.Rout.fail
    on.exit( knitr::knit_hooks$restore())
    # covr infects functions, so we deparse an grep them first
    clean <- function(x) gsub(" ", "",
                              paste0(grep("covr:::count|   \\{$|   \\}$",
                                          invert = TRUE, value = TRUE,
                                          deparse(x)),
                                     collapse = "")
                              )
    # get a verbartim copy from adjust_knitr_hooks:
    hook.source <- function(x, options) {
        x <- paste(c(hilight_source(x, "asciidoc", options), ""),
                   collapse = "\n")
        sprintf("\n[source,%s]\n----\n%s----\n", tolower(options$engine),
                x)
    }
    hook.message <- function(x, options) {
        sprintf("\n[NOTE]\n====\n.Message\n%s\n====\n",
                substring(x, comment_length(options$comment)))
    }
    hook.warning <- function(x, options) {
        sprintf("\n[WARNING]\n====\n.Warning\n%s\n====\n",
                gsub("^.*Warning: ", "", x))
    }
    hook.error <- function(x, options) {
        sprintf("\n[CAUTION]\n====\n.Error\n%s\n====\n",
                gsub("^.*Error: ", "", x))
    }
    hook.output <- function(x, options) sprintf("\n----\n%s----\n", x)
    knitr::knit_hooks$restore()
    rasciidoc::adjust_asciidoc_hooks(replacement = NULL)
    cs <- knitr::knit_hooks$get("source")
    co <- knitr::knit_hooks$get("output")
    cm <- knitr::knit_hooks$get("message")
    cw <- knitr::knit_hooks$get("warning")
    ce <- knitr::knit_hooks$get("error")
    RUnit::checkEquals(clean(cs), clean(hook.source))
    RUnit::checkEquals(clean(co), clean(hook.output))
    RUnit::checkEquals(clean(cm), clean(hook.message))
    RUnit::checkEquals(clean(cw), clean(hook.warning))
    RUnit::checkEquals(clean(ce), clean(hook.error))
    rasciidoc::adjust_asciidoc_hooks(hooks = c("message", "warning", "error"),
                                     replacement = "source")
    cs <- knitr::knit_hooks$get("source")
    co <- knitr::knit_hooks$get("output")
    cm <- knitr::knit_hooks$get("message")
    cw <- knitr::knit_hooks$get("warning")
    ce <- knitr::knit_hooks$get("error")
    RUnit::checkEquals(clean(cs), clean(hook.source))
    RUnit::checkEquals(clean(co), clean(hook.output))
    RUnit::checkEquals(clean(cm), clean(hook.source))
    RUnit::checkEquals(clean(cw), clean(hook.source))
    RUnit::checkEquals(clean(ce), clean(hook.source))
    rasciidoc::adjust_asciidoc_hooks(hooks = c("source"), replacement = "error")
    cs <- knitr::knit_hooks$get("source")
    RUnit::checkEquals(clean(cs), clean(hook.error))
}
