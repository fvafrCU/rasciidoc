test_render_simple <- function() {
    folder  <- system.file("runit_tests", "files", package = "rasciidoc")
    file.copy(folder, tempdir(), recursive = TRUE)
    on.exit(unlink(file.path(tempdir(), "files"), recursive = TRUE))
    withr::with_dir(file.path(tempdir(), "files"), rasciidoc::render_slides("simple.Rasciidoc"))
    result <- grep(".*CET$", value = TRUE, invert = TRUE,
                   readLines(file.path(tempdir(), "files", "simple.html")))
    expectation  <- grep(".*CET$", value = TRUE, invert = TRUE,
                         readLines(file.path(tempdir(), "files", "expected", "simple.html")))
    RUnit::checkIdentical(result, expectation)
}
test_render_slides <- function() {
    folder  <- system.file("runit_tests", "files", package = "rasciidoc")
    file.copy(folder, tempdir(), recursive = TRUE)
    on.exit(unlink(file.path(tempdir(), "files"), recursive = TRUE))
    withr::with_dir(file.path(tempdir(), "files"), rasciidoc::render_slides("slides.Rasciidoc"))
    result <- grep(".*CET$", value = TRUE, invert = TRUE,
                   readLines(file.path(tempdir(), "files", "slides.html")))
    expectation  <- grep(".*CET$", value = TRUE, invert = TRUE,
                         readLines(file.path(tempdir(), "files", "expected", "slides.html")))
    RUnit::checkIdentical(result, expectation)
    result <- grep(".*CET$", value = TRUE, invert = TRUE,
                   readLines(file.path(tempdir(), "files", "slides_slidy.html")))
    expectation  <- grep(".*CET$", value = TRUE, invert = TRUE,
                         readLines(file.path(tempdir(), "files", "expected", "slides_slidy.html")))
    RUnit::checkIdentical(result, expectation)
}
