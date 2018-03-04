devtools::load_all(".")
withr::with_dir(file.path("inst", "files", "simple"),
                render("knit.Rasciidoc"))
withr::with_dir(file.path("inst", "files", "simple"),
                {
                    file.copy("spin.R_nolint", "spin.R")
                    render("spin.R")
                    unlink("spin.R")
                }
)
rasciidoc(system.file("files", "simple", "knit.asciidoc", package = "rasciidoc"))
rasciidoc(system.file("files", "simple", "spin.md", package = "rasciidoc"))

withr::with_dir(file.path("inst", "files", "simple"),
                knitr::knit(system.file("files", "simple", "knitr.Rmd", package = "rasciidoc"))
                )


# This changes knit.asciidoc, html is not produced:
render(system.file("files", "simple", "knit.Rasciidoc", package = "rasciidoc"))
