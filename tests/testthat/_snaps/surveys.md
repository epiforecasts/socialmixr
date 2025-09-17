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

# list_survey() gives deprecated warning

    `list_surveys()` was deprecated in socialmixr 0.5.0.
    i Please use `contactsurveys::list_surveys()` instead.

# get_citation() gives deprecated warning

    `get_citation()` was deprecated in socialmixr 0.5.0.
    i Please use `contactsurveys::get_citation()` instead.

