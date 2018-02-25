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
        expectation  <- as.integer(127)
        RUnit::checkIdentical(result, expectation)
    } else {
        expectation  <- as.integer(0)
        RUnit::checkIdentical(result, expectation)
        result <- remove_dates(readLines(file.path(tempdir(), "files", 
                                                   "simple.html")))
        expectation <- remove_dates(readLines(file.path(tempdir(), "files", 
                                                        "expected", "simple.html")))
        if (probably_me()) {
            RUnit::checkIdentical(result, expectation)
        } else {
            RUnit::checkTrue(any(grepl("Dominik Cullmann", result)))
        }
    }
    #% render slides
    withr::with_dir(file.path(tempdir(), "files"), 
                    rasciidoc::render_slides("simple.Rasciidoc"))
    result <- remove_dates(readLines(file.path(tempdir(), "files", 
                                               "simple.html")))
    expectation <- remove_dates(readLines(file.path(tempdir(), "files", 
                                                    "expected", "simple.html")))
    if (probably_me()) {
        RUnit::checkIdentical(result, expectation)
    } else {
        RUnit::checkTrue(any(grepl("Dominik Cullmann", result)))
    }
    # file contains no R code
    withr::with_dir(file.path(tempdir(), "files"), 
                    rasciidoc::render("fake.Radoc", knit = NA))
    result <- remove_dates(readLines(file.path(tempdir(), "files", "fake.html")))
    expectation <- remove_dates(readLines(file.path(tempdir(), "files", 
                                                    "expected", "fake.html")))
    if (probably_me()) {
        RUnit::checkIdentical(result, expectation)
    } else {
        RUnit::checkTrue(any(grepl("Dominik Cullmann", result)))
    }

}
test_render_slides <- function() {
    folder  <- system.file("runit_tests", "files", package = "rasciidoc")
    file.copy(folder, tempdir(), recursive = TRUE)
    on.exit(unlink(file.path(tempdir(), "files"), recursive = TRUE))
    withr::with_dir(file.path(tempdir(), "files"), 
                    rasciidoc::render_slides("slides.Rasciidoc"))
    result <- remove_dates(readLines(file.path(tempdir(), "files", 
                                               "slides.html")))
    expectation <- remove_dates(readLines(file.path(tempdir(), "files", 
                                                    "expected", "slides.html")))
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
