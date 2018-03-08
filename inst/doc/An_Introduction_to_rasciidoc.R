file_name <- system.file("files", "minimal", "knit.Rasciidoc",
                         package = "rasciidoc")
cat(readLines(file_name), sep = "\n")
withr::with_dir(tempdir(), {
                    file.copy(file_name, ".")
                    rasciidoc::render(basename(file_name))
})
if (interactive()) browseURL(file.path(tempdir(),
                                       paste0(sub("\\..*$", "",
                                                  basename(file_name)),
                                              ".html")))
file_name <- system.file("files", "simple", "knit.Rasciidoc",
                         package = "rasciidoc")
cat(readLines(file_name), sep = "\n")
withr::with_dir(tempdir(), {
                    file.copy(file_name, ".")
                    file.copy(file.path(dirname(file_name), "src"), ".",
                              recursive = TRUE)
                    rasciidoc::render(basename(file_name))
})
if (interactive()) browseURL(file.path(tempdir(),
                                       paste0(sub("\\..*$", "",
                                                  basename(file_name)),
                                              ".html")))
file_name <- system.file("files", "simple", "spin.R_nolint",
                         package = "rasciidoc")
cat(readLines(file_name), sep = "\n")
withr::with_dir(tempdir(), {
                    file.copy(file_name, ".", overwrite = TRUE)
                    file.copy(file.path(dirname(file_name), "src"), ".",
                              recursive = TRUE)
                    rasciidoc::render(basename(file_name))
})
if (interactive()) browseURL(file.path(tempdir(),
                                       paste0(basename(file_name),
                                              ".html")))
rasciidoc::render(file.path(tempdir(), basename(file_name)), hooks = NULL)
if (interactive()) browseURL(file.path(tempdir(),
                                       paste0(basename(file_name),
                                              ".html")))
