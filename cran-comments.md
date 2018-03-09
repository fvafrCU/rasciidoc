Dear CRAN Team,
this is the initial commit of package 'rasciidoc'.

Ever since I read Karl Broman`s reader on using knitr with asciidoc (http://kbroman.org/knitr_knutshell/pages/asciidoc.html), I've been using a bunch of functions to change knitr's default output hooks for asciidoc, run knitr on the input file (possibly taking care of options for slidy output) and run asciidoc from within R. I have eliminated bugs I encountered while using them for over a year now and I hope they might be of use to others.

Please consider uploading it to CRAN.
Best, Dominik

# Package rasciidoc 1.0.0
## Test  environments 
- R Under development (unstable) (2017-08-15 r73096)
  Platform: x86_64-pc-linux-gnu (64-bit)
  Running under: Debian GNU/Linux 9 (stretch)
- R version 3.4.2 (2017-01-27)
  Platform: x86_64-pc-linux-gnu (64-bit)
  Running under: Ubuntu 14.04.5 LTS
- win-builder (devel)

## Local Test Coverage
rasciidoc Coverage: 92.00%
R/knitr_internals.R: 55.56%
R/knitr_internals_mod.R: 78.18%
R/highr_internals.R: 100.00%
R/render.R: 100.00%
R/throw.R: 100.00%

## R CMD check results
Status: OK
