a  <- utils::person(given = "Andreas Dominik",
           family = "Cullmann",
           role = c("aut", "cre"),
           email = "adc-r@arcor.de")
packager::set_package_info(force = FALSE, path = ".", author_at_r = a, 
                 title = "Create Reports Using R and `asciidoc`",
                 description = "Inspired by Karl Broman's reader on using knitr with asciidoc (<http://kbroman.org/knitr_knutshell/pages/asciidoc.html>), this is a wrapper to and a slight modification of knitr.",
                 details = 'You will find the details in\\cr\\code{vignette("An_Introduction_to_asciidoc", package = "rasciidoc")}')
devtools::load_all(".")
withr::with_dir(file.path("inst", "files", "simple"),
                rasciidoc::render("knit.Rasciidoc"))
withr::with_dir(file.path("inst", "files", "simple"),
                {
                    file.copy("spin.R_nolint", "spin.R")
                    rasciidoc::render("spin.R")
                    unlink("spin.R")
                }
)
# This will not change the html output!
rasciidoc::rasciidoc(system.file("files", "simple", "knit.asciidoc", package = "rasciidoc"))
rasciidoc::rasciidoc(system.file("files", "simple", "spin.md", package = "rasciidoc"))


# Knitr does not change to file's path!
withr::with_dir(file.path("inst", "files", "simple"),
                knitr::knit(system.file("files", "simple", "knitr.Rmd", package = "rasciidoc"))
                )

# Nor does spin!
withr::with_dir(file.path("inst", "files", "simple"),
                knitr::spin(system.file("files", "simple", "spin.R_nolint", package = "rasciidoc"), knit = TRUE, report = FALSE)
                )

file_name <- system.file("files", "simple", "knit.Rasciidoc", package = "rasciidoc")
rasciidoc::render(file_name)




