##' Generate a contact matrix from diary survey data
##'
##' Samples a contact survey using a bootstrap
##'
##' @param survey a \code{\link{survey}} object
##' @param countries limit to one or more countries; if not given, will use all countries in the survey; these can be given as country names or 2-letter (ISO Alpha-2) country codes
##' @param survey.pop survey population -- either a data frame with columns 'lower.age.limit' and 'population', or a character vector giving the name(s) of a country or countries from the list that can be obtained via \code{wpp_countries}; if not given, will use the country populations from the chosen countries, or all countries in the survey if \code{countries} is not given
##' @param age.limits lower limits of the age groups over which to construct the matrix
##' @param filter any filters to apply to the data, given as list of the form (column=filter_value) - only contacts that have 'filter_value' in 'column' will be considered
##' @param n number of matrices to sample
##' @param bootstrap whether to sample participants and contacts randomly using a bootstrap; by default, will use bootstrap if n > 1
##' @param counts whether to return counts (instead of means)
##' @param symmetric whether to make matrix symmetric
##' @param split whether to split the number of contacts and assortativity
##' @param missing.participant.age if set to "remove", participants without age information are removed; if set to "keep", participants with missing age are kept and treated as a separate age group
##' @param missing.contact.age if set to "sample", contacts without age information are sampled from all the contacts of participants of the same age group; if set to "remove", participants that that have contacts without age information are removed; if set to "keep", contacts with missing age are kept and treated as a separate age group
##' @param weights columns that contain weights
##' @param weigh.dayofweek whether to weigh the day of the week (weight 5 for weekdays ans 2 for weekends)
##' @param quiet if set to TRUE, output is reduced
##' @param ... further arguments to pass to, \code{\link{check}} and \code{\link{pop_age}} (especially column names)
##' @return a list of sampled contact matrices, and the underlying demography of the surveyed population
##' @importFrom stats xtabs runif median
##' @importFrom utils data globalVariables
##' @importFrom countrycode countrycode
##' @import data.table
##' @export
##' @inheritParams get_survey
##' @inheritParams pop_age
##' @examples
##' data(polymod)
##' contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))
##' @author Sebastian Funk
contact_matrix <- function(survey, countries=c(), survey.pop, age.limits, filter, n = 1, bootstrap, counts = FALSE, symmetric = FALSE, split = FALSE, missing.participant.age = c("remove", "keep"), missing.contact.age = c("sample", "remove", "keep"), weights = c(), weigh.dayofweek = FALSE, quiet = FALSE, ...)
{
    ## circumvent R CMD CHECK errors by defining global variables
    lower.age.limit <- NULL
    N <- NULL
    population <- NULL
    upper.age.limit <- NULL
    agegroup <- NULL
    weight <- NULL
    dayofweek <- NULL
    cnt.agegroup <- NULL
    proportion <- NULL

    ## record if 'missing.participant.age' and 'missing.contact.age' are set, for later
    missing.participant.age.set <- !missing(missing.participant.age)
    missing.contact.age.set <- !missing(missing.contact.age)

    ## read arguments
    missing.participant.age <- match.arg(missing.participant.age)
    missing.contact.age <- match.arg(missing.contact.age)

    ## get the survey
    survey <- get_survey(survey, quiet)
    ## check and get columns
    columns <- check(survey, columns=TRUE, quiet=TRUE, ...)

    ## copy data
    participants <- copy(survey$participants)
    contacts <- copy(survey$contacts)

    ## if bootstrap not asked for
    if (missing(bootstrap)) bootstrap <- (n > 1)

    ## check if specific countries are requested (if a survey contains data from multiple countries)
    if (length(countries) > 0 && columns[["country"]] %in% colnames(participants))
    {
        if (all(nchar(countries) == 2))
        {
            suppressWarnings(corrected_countries <-
                                     countrycode(countries, "iso2c", "country.name"))
        } else
        {
            suppressWarnings(corrected_countries <-
                                 countrycode(countries, "country.name", "country.name"))
        }
        present_countries <- unique(as.character(participants[[columns[["country"]]]]))
        missing_countries <- countries[which(is.na(corrected_countries))]
        if (length(missing_countries) > 0)
        {
            stop("Survey data not found for ", paste(missing_countries, sep=", "), ".")
        }
        countries <- corrected_countries
        participants <- participants[get(columns[["country"]]) %in% countries]
        if (nrow(participants) == 0)
        {
            stop("No participants left after selecting countries.")
        }
    }

    ## check maximum age in the data
    max.age <- max(participants[, get(columns[["participant.age"]])], na.rm = TRUE) + 1
    if (missing(age.limits)) age.limits <- c(0, seq_len(max.age-1))

    ## check if any filters have been requested
    if (nrow(contacts) > 0 && !missing(filter)) {
        missing_columns <- setdiff(names(filter), colnames(contacts))
        if (length(missing_columns) > 0) {
            warning("filter column(s) ", paste(missing_columns), " not found")
            filter <- filter[names(filter) %in% colnames(contacts)]
        }
        ## filter contact data
        for (column in names(filter)) {
            contacts <- contacts[get(column) == filter[[column]]]
        }
    }

    if (missing.participant.age == "remove" &&
        nrow(participants[is.na(get(columns[["participant.age"]]))]) > 0)
    {
        if (!quiet && !missing.participant.age.set)
        {
            message("Removing participants without age information. ",
                    "To change this behaviour, set the 'missing.participant.age' option")
        }
        participants <- participants[!is.na(get(columns[["participant.age"]]))]
    }

    if (missing.contact.age == "remove" &&
        nrow(contacts[is.na(get(columns[["contact.age"]]))]) > 0)
    {
        missing.age.id <- contacts[is.na(get(columns[["contact.age"]])), get(columns[["id"]])]
        participants <- participants[!(get(columns[["id"]]) %in% missing.age.id)]
    }

    ## if split or symmetric requested, get demographic data (survey population)
    need.survey.pop <- split || symmetric
    if (need.survey.pop)
    {
        ## check if survey population is either not given or given as a vector of countries
        if (missing(survey.pop) || is.character(survey.pop))
        {
            survey.representative=FALSE
            if (!missing(survey.pop))
            {
                ## survey population is given as vector of countries
                survey.countries <- survey.pop
            } else if (!missing(countries))
            {
                ## survey population not given but countries requested from
                ## survey - get population data from those countries
                survey.countries <- countries
            } else
            {
                ## neither survey population nor country names given - try to
                ## guess country or countries surveyed from participant data
                if (columns[["country"]] %in% names(participants))
                {
                    survey.countries <- unique(participants[, get(columns[["country"]])])
                } else
                {
                    warning("No 'survey.pop' or 'countries' given, and no '", columns[["country"]],
                            "' column found in the data. ",
                            "I don't know which population this is from. ",
                            "Assuming the survey is representative")
                    survey.representative=TRUE
                }
            }

            if (!survey.representative) {
                ## get population data for countries from 'wpp' package
                country.pop <- data.table(wpp_age(survey.countries))

                ## check if survey data are from a specific year - in that case
                ## use demographic data from that year, otherwise latest
                if (columns[["year"]] %in% colnames(participants))
                {
                    survey.year <- participants[, median(get(columns[["year"]]), na.rm=TRUE)]
                } else
                {
                    survey.year <- country.pop[, max(year, na.rm=TRUE)]
                    warning("No '", columns[["year"]], "' column found in the data. Will use ",
                            survey.year, " population data.")
                }

                ## check if any survey countries are not in wpp
                missing.countries <- setdiff(survey.countries, unique(country.pop$country))
                if (length(missing.countries) > 0)
                {
                    warning("Could not find population data for ",
                            paste(missing.countries, collapse = ", "), ". ",
                            " Use wpp_countries() to get a list of country names.")
                }

                if (length(missing.countries) == length(survey.countries)) {
                    warning("No survey data available for any of the requested data. ",
                            "I don't know which population this is from. ",
                            "Assuming the survey is representative")
                    survey.representative <- TRUE
                } else {
                    ## get demographic data closest to survey year
                    country.pop.year <- unique(country.pop[, year])
                    survey.year <-
                        min(country.pop.year[which.min(abs(survey.year - country.pop.year))])
                    survey.pop <-
                        country.pop[year == survey.year][, list(population = sum(population)),
                                                         by = "lower.age.limit"]
                }
            }

            if (survey.representative) {
                survey.pop <-
                    participants[, lower.age.limit :=
                                       reduce_agegroups(get(columns[["participant.age"]]), age.limits)]
                survey.pop <- survey.pop[, list(population=.N), by=lower.age.limit]
                survey.pop <- survey.pop[!is.na(lower.age.limit)]
                if (columns[["year"]] %in% colnames(participants))
                {
                    survey.year <- participants[, median(get(columns[["year"]]), na.rm=TRUE)]
                }
                survey.year <- participants[, median(get(columns[["year"]]), na.rm=TRUE)]
            }
        }
        ## adjust age groups by interpolating, in case they don't match between
        ## demographic and survey data
        survey.pop <- data.table(pop_age(survey.pop, age.limits, ...))

        if (nrow(survey.pop) == 0)
        {
            warning("Could not construct survey population data.")
            survey.pop <- data.table(lower.age.limit=age.limits, population=NA_integer_)
        }

        ## possibly adjust age groups according to maximum age (so as not to have empty age groups)
        survey.pop[, lower.age.limit := reduce_agegroups(lower.age.limit, age.limits)]
        survey.pop <- survey.pop[, list(population = sum(population)), by=lower.age.limit]
        setkey(survey.pop, lower.age.limit)
        ## re-assign lower age limits in participants
        participants[, lower.age.limit :=
                           reduce_agegroups(get(columns[["participant.age"]]),
                                            survey.pop$lower.age.limit)]
        present.lower.age.limits <- unique(survey.pop$lower.age.limit)
        present.lower.age.limits <-
            present.lower.age.limits[order(present.lower.age.limits)]

        ## set upper age limits
        survey.pop[, upper.age.limit := c(survey.pop$lower.age.limit[-1], max.age)]

        lower.upper.age.limits <-
            data.table(lower.age.limit = present.lower.age.limits,
                       upper.age.limit = c(present.lower.age.limits[-1], max.age))
        ## set upper age limits and construct age groups
        participants <- merge(participants, lower.upper.age.limits, by="lower.age.limit")
        if (all(is.na(survey.pop$population))) survey.pop[, population := NULL]
    }

    participants[, lower.age.limit := reduce_agegroups(get(columns[["participant.age"]]),
                                                       age.limits[age.limits < max.age])]
    participants[, agegroup := cut(participants[, get(columns[["participant.age"]])],
                                          breaks = union(age.limits, max.age),
                                          right = FALSE)]
    agegroups <- participants[, levels(agegroup)]
    agegroups[length(agegroups)] <- paste0(max(participants$lower.age.limit), "+")
    participants[, agegroup := factor(agegroup, levels=levels(agegroup), labels=agegroups)]

    participants[, weight := 1]
    ## assign weights to participants, to account for weekend/weekday variation
    if (weigh.dayofweek) {
        if ("dayofweek" %in% colnames(participants))
        {
            participants[dayofweek %in% 1:5,
                         weight := 5 / nrow(participants[dayofweek %in% 1:5])]
            participants[!(dayofweek %in% 1:5),
                         weight := 2 / nrow(participants[!(dayofweek %in% 1:5)])]
        } else
        {
            warning("'weigh.dayofweek' is TRUE, but no 'dayofweek' column in the data. ",
                    "Will ignore.")
        }
    }

    if (n > 1)
    {
        if (!bootstrap)
        {
            warning("n > 1 does not make sense if not bootstrapping. Will return just one sample.")
            n <- 1
        }
    }

    ret <- list()
    for (i in seq_len(n))
    {
        if (bootstrap)
        {
            ## take a bootstrap sample from the participants
            part.sample <- participants[sample(nrow(participants), replace = T)]
        } else
        {
            ## just use all participants
            part.sample <- copy(participants)
        }

        ## gather contacts for sampled participants
        contacts.sample <-
            data.table(merge(contacts, part.sample, by = columns[["id"]], all = F,
                             allow.cartesian = T))

        ## sample contacts
        if (missing.contact.age == "sample" &&
            nrow(contacts.sample[is.na(get(columns[["contact.age"]]))]) > 0)
        {
            if (!quiet && n == 1 && !missing.contact.age.set)
            {
                message("Sampling the age of contacts with missing age from other ",
                        "participants of the same age group.\n  To change this behaviour, set the ",
                        "'missing.contact.age' option")
            }
            for (this.agegroup in
                 unique(contacts.sample[is.na(get(columns[["contact.age"]])), agegroup]))
            {
                ## first, deal with missing age
                if (nrow(contacts.sample[!is.na(get(columns[["contact.age"]])) &
                                         agegroup == this.agegroup]) > 0)
                {
                    ## some contacts in the age group have an age, sample from these
                    contacts.sample[is.na(get(columns[["contact.age"]])) &
                                    agegroup == this.agegroup,
                                    paste(columns[["contact.age"]]) :=
                                        sample(contacts.sample[!is.na(get(columns[["contact.age"]])) &
                                                               agegroup == this.agegroup,
                                                               get(columns[["contact.age"]])],
                                               size = .N,
                                               replace = TRUE)]
                } else {
                    ## no contacts in the age group have an age, sample uniformly between limits
                  min.contact.age <-
                    contacts.sample[, min(get(columns[["contact.age"]]), na.rm=TRUE)]
                  max.contact.age <-
                    contacts.sample[, max(get(columns[["contact.age"]]), na.rm=TRUE)]
                  contacts.sample[is.na(get(columns[["contact.age"]])) &
                                    agegroup == this.agegroup,
                                    paste(columns[["contact.age"]]) :=
                                      as.integer(floor(runif(.N, min = min.contact.age,
                                                             max = max.contact.age + 1)))]
                }
            }
        }
        ## set contact age groups
        max.contact.age <- contacts.sample[, max(get(columns[["contact.age"]]), na.rm = TRUE) + 1]
        contacts.sample[, cnt.agegroup :=
                              cut(get(columns[["contact.age"]]),
                                  breaks = union(age.limits, max.contact.age),
                                  labels = agegroups,
                                  right = FALSE)]

        ## further weigh contacts if columns are specified
        if (length(weights) > 0) {
            for (i in 1:length(weights)) {
                contacts.sample[, weight := weight * get(weights[i])]
            }
        }

        ## calculate weighted contact matrix
        weighted.matrix <- xtabs(data = contacts.sample,
                                 formula = weight ~ agegroup + cnt.agegroup,
                                 addNA = TRUE)

        if (!counts) { ## normalise to give mean number of contacts
            ## calculate normalisation vector
            norm.vector <- xtabs(data = part.sample, formula = weight ~ agegroup, addNA = TRUE)

            ## normalise contact matrix
            weighted.matrix <- apply(weighted.matrix, 2, function(x) x/norm.vector)
            ## set non-existent data to NA
            weighted.matrix[is.nan(weighted.matrix)] <- NA_real_
        }


        ## construct a warning in case there are NAs
        na.headers <- any(is.na(colnames(weighted.matrix))) ||
            any(is.na(rownames(weighted.matrix)))
        na.content <- any(is.na(weighted.matrix))
        na.present <- na.headers || na.content

        if (na.present)
        {
            warning.suggestion <- "  Consider "
            if (na.headers)
            {
                warning.suggestion <- paste0(warning.suggestion, "setting ")
                suggested.options <- c()
                if (any(is.na(rownames(weighted.matrix))))
                    suggested.options <- c(suggested.options, "'missing.participant.age'")
                if (any(is.na(colnames(weighted.matrix))))
                    suggested.options <- c(suggested.options, "'missing.contact.age'")
                warning.suggestion <-
                    paste0(warning.suggestion, paste(suggested.options, collapse = " and "))
                if (na.content)
                {
                    warning.suggestion <- paste0(warning.suggestion, ", and ")
                } else
                {
                    warning.suggestion <- paste0(warning.suggestion, ".")
                }

            }
            if (na.content)
            {
                warning.suggestion <- paste0(warning.suggestion, "adjusting the age limits.")
            }
        }

        if (symmetric & prod(dim(as.matrix(weighted.matrix))) > 1) {
            if (counts) {
                warning("'symmetric=TRUE' does not make sense with 'counts=TRUE'; ",
                        "will not make matrix symmetric.")
            } else if (na.present)
            {
                warning("'symmetric=TRUE' does not work with missing data; ",
                        "will not make matrix symmetric\n",
                        warning.suggestion)
            } else
            {
                ## set c_{ij} N_j and c_{ji} N_i (which should both be equal) to
                ## 0.5 * their sum; then c_{ij} is that sum / N_i
                normalised.weighted.matrix <- diag(survey.pop$population) %*% weighted.matrix
                weighted.matrix <- 0.5 * diag(1/survey.pop$population) %*%
                    (normalised.weighted.matrix + t(normalised.weighted.matrix))
            }
        }

        ret[[i]] <- list()

        if (split)
        {
            if (counts) {
                warning("'split=TRUE' does not make sense with 'counts=TRUE'; ",
                        "will not make matrix symmetric.")
            } else if (na.present)
            {
                warning("'split=TRUE' does not work with missing data; ",
                        "will not make matrix symmetric\n",
                        warning.suggestion)
            } else
            {
                ## get rid of name but preserve row and column names
                rows <- rownames(weighted.matrix)
                cols <- colnames(weighted.matrix)
                weighted.matrix <- unname(weighted.matrix)

                if (counts) {
                    warning("'split=TRUE' does not make sense with 'counts=TRUE'; ",
                            "will not make matrix symmetric.")
                } else
                {
                    nb.contacts <- apply(weighted.matrix, 1, sum)
                    spectrum.matrix <- weighted.matrix
                    spectrum.matrix[is.na(spectrum.matrix)] <- 0
                    spectrum <- as.numeric(eigen(spectrum.matrix, only.values = TRUE)$values[1])
                    ret[[i]][["normalisation"]] <- spectrum

                    age.proportions <- survey.pop$population / sum(survey.pop$population)
                    weighted.matrix <-
                        diag(1 / nb.contacts) %*% weighted.matrix %*% diag(1 / age.proportions)
                    nb.contacts <- nb.contacts / spectrum
                    ret[[i]][["contacts"]] <- nb.contacts
                }
                rownames(weighted.matrix) <- rows
                colnames(weighted.matrix) <- cols
            }
        }

        ret[[i]][["matrix"]] <- weighted.matrix
    }

    if (exists("survey.year")) {
        survey.pop[, year := survey.year]
        survey.pop <- survey.pop[, list(lower.age.limit, population,
                                        proportion=population/sum(population), year)]
    }

    ## get number of participants in each age group
    if (any(is.na(levels(part.sample$agegroup)))) {
        useNA <- "always"
    } else {
        useNA <- "no"
    }
    part.pop <- data.table(table(participants[, agegroup], useNA = useNA))
    setnames(part.pop, c("lower.age.limit", "participants"))
    part.pop[, proportion := participants / sum(participants)]

    if (length(ret) > 1)
        return_value <- list(matrices = ret)
    else if (length(ret) == 1)
        return_value <- ret[[1]]
    else return_value <- NULL

    if (!is.null(return_value)) {
        if (need.survey.pop) return_value[["demography"]] <- survey.pop[]
        return_value[["participants"]] <- part.pop[]
    }

    return(return_value)
}

