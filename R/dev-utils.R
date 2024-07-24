# This unexported function adds a custom item to `usethis::use_release_issue()`
release_bullets <- function() {
  c(
    "Run `goodpractice::gp()`",
    "Review [WORDLIST](https://docs.cran.dev/spelling#wordlist)",
    "Check if `# nolint` comments are still needed with recent lintr releases",
    "All contributors to this release are acknowledged in some way"
  )
}
