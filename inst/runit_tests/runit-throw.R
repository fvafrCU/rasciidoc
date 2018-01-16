test_exception <- function() {
    RUnit::checkException(asciidoc:::throw("Hello, error!"))
}
