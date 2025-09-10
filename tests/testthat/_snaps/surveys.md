# missing surveys can't be cited

    Code
      get_citation("bogus")
    Condition
      Error in `download_survey()`:
      ! `survey` is not a DOI or URL.

# multiple DOI's cannot be loaded

    Code
      suppressMessages(suppressWarnings(get_survey(c("10.5281/zenodo.1095664",
        "10.5281/zenodo.1127693"))))
    Condition
      Error in `download_survey()`:
      ! `survey` must be a character of length 1.

