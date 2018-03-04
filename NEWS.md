# rasciidoc 0.7.0

* render() now uses a working directory that defaults to the input file's
  directory, allowing for the default file to source code.
# rasciidoc 0.6.0

* render() now works for R files with markup in roxygen comments that are
  parsed trough knitr::spin().

# rasciidoc 0.5.0

* Passed the parent.frame() down to knitr to always be in .GlobalEnv.

# rasciidoc 0.4.0

* Add `//[begin|end]\_only\_slide`-blocks and `//slide\_only`-comments to allow 
  for content for slidy only.
* Fix broken code inclusions for slidy.

# rasciidoc 0.3.0

* Made adjusting knitr's hooks (see rasciidoc 0.2.0) the default behaviour.

# rasciidoc 0.2.0

* Added function to adjust knitr's hooks for asciidoc files, providing a work
  around for not using asciidoc's [MESSAGE|WARNING|ERROR] when knitting produces
  a message|warning|error.

# rasciidoc 0.1.0

* Added core functionality.
