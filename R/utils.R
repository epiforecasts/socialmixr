flexible_countrycode <- function(countries){
    coding_scheme <- ifelse(
        test = all(nchar(countries) == 2),
        yes = "iso2c",
        no = "country.name"
    )
        corrected_countries <- suppressWarnings(
            countrycode(
                sourcevar = countries,
                origin = coding_scheme,
                destination = "country.name"
                )
        )
}
