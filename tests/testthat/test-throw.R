testthat::context("Testing asciidoc:::throw()")
testthat::test_that("throw the asciidoc exception", {
                        error_message <- "hello, testthat"
                        string <- "hello, testthat"
                        testthat::expect_error(asciidoc:::throw(string),
                            error_message)
}
)
