flexible_countrycode <- function(countries){
    if (is.null(countries) || length(countries) == 0) {
        return(character(0))
    }
    
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
    
    return(corrected_countries)
}
