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
##' @param estimated.contact.age if set to "mean" (default), people whose ages are given as a range (in columns named "..._est_min" and "..._est_max") but not exactly (in a column named "..._exact") will have their age set to the mid-point of the range; if set to "sample", the age will be sampled from the range; if set to "missing", age ranges will be treated as missing
##' @param missing.participant.age if set to "remove" (default), participants without age information are removed; if set to "keep", participants with missing age are kept and treated as a separate age group
##' @param missing.contact.age if set to "remove" (default), participants that that have contacts without age information are removed; if set to "sample", contacts without age information are sampled from all the contacts of participants of the same age group; if set to "keep", contacts with missing age are kept and treated as a separate age group
##' @param weights columns that contain weights
##' @param weigh.dayofweek whether to weigh the day of the week (weight 5 for weekdays ans 2 for weekends)
##' @param quiet if set to TRUE, output is reduced
##' @param ... further arguments to pass to \code{\link{get_survey}}, \code{\link{check}} and \code{\link{pop_age}} (especially column names)
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
contact_matrix <- function(survey, countries=c(), survey.pop, age.limits, filter, n = 1, bootstrap, counts = FALSE, symmetric = FALSE, split = FALSE, estimated.contact.age=c("mean", "sample", "missing"), missing.participant.age = c("remove", "keep"), missing.contact.age = c("remove", "sample", "keep"), weights = c(), weigh.dayofweek = FALSE, quiet = FALSE, ...)
{
    ## circumvent R CMD CHECK errors by defining global variables
    lower.age.limit <- NULL
    N <- NULL
    population <- NULL
    upper.age.limit <- NULL
    age.group <- NULL
    weight <- NULL
    dayofweek <- NULL
    contact.age.group <- NULL
    proportion <- NULL
    participants <- NULL

    surveys <- c("participants", "contacts")

    dot.args <- list(...)
    unknown.args <- setdiff(names(dot.args), union(names(formals(check.survey)), names(formals(pop_age))))
    if (length(unknown.args) > 0)
    {
        stop("Unknown argument(s): ", paste(unknown.args, sep=", "), ".")
    }

    ## record if 'missing.participant.age' and 'missing.contact.age' are set, for later
    missing.participant.age.set <- !missing(missing.participant.age)
    missing.contact.age.set <- !missing(missing.contact.age)

    ## read arguments
    estimated.contact.age <- match.arg(estimated.contact.age)
    missing.participant.age <- match.arg(missing.participant.age)
    missing.contact.age <- match.arg(missing.contact.age)

    ## get the survey
    survey <- get_survey(survey, quiet)
    ## check and get columns
    columns <- check(survey, columns=TRUE, quiet=TRUE, ...)

    ## if bootstrap not asked for
    if (missing(bootstrap)) bootstrap <- (n > 1)

    ## check if specific countries are requested (if a survey contains data from multiple countries)
    if (length(countries) > 0 && columns[["country"]] %in% colnames(survey$participants))
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
        present_countries <- unique(as.character(survey$participants[[columns[["country"]]]]))
        missing_countries <- countries[which(is.na(corrected_countries))]
        if (length(missing_countries) > 0)
        {
            stop("Survey data not found for ", paste(missing_countries, sep=", "), ".")
        }
        countries <- corrected_countries
        survey$participants <- survey$participants[get(columns[["country"]]) %in% countries]
        if (nrow(survey$participants) == 0)
        {
            stop("No participants left after selecting countries.")
        }
    }

    ## check maximum participant age in the data
    max.age <- max(survey$participants[, get(columns[["participant.age"]])], na.rm = TRUE) + 1
    if (missing(age.limits))
    {
        age.limits <- c(0, seq_len(max.age-1))
    }
    age.limits <- as.integer(age.limits)
    if (any(is.na(age.limits)) || any(diff(age.limits) <= 0))
    {
        stop("'age.limits' must be an increasing integer vector of lower age limits.")
    }

    ## check if any filters have been requested
    if (!missing(filter)) {
        missing_columns <- list()
        for (table in surveys)
        {
            if (nrow(survey[[table]]) > 0)
            {
                missing_columns <-
                    c(missing_columns, list(setdiff(names(filter), colnames(survey[[table]]))))
                ## filter contact data
                for (column in names(filter))
                {
                    if (column %in% colnames(survey[[table]]))
                    {
                        survey[[table]] <- survey[[table]][get(column) == filter[[column]]]
                    }
                }
            }
        }
        missing_all <- do.call(intersect, missing_columns)
        if (length(missing_all) > 0) {
            warning("filter column(s) ", paste(missing_all), " not found")
        }
    }

    if (missing.participant.age == "remove" &&
        nrow(survey$participants[is.na(get(columns[["participant.age"]]))]) > 0)
    {
        if (!quiet && !missing.participant.age.set)
        {
            message("Removing participants without age information. ",
                    "To change this behaviour, set the 'missing.participant.age' option")
        }
        survey$participants <-
            survey$participants[!is.na(get(columns[["participant.age"]])) &
                                get(columns[["participant.age"]]) >=  min(age.limits)]
    }

    ## set contact age if it's not in the data
    if (!(columns[["contact.age"]] %in% colnames(survey$contacts)))
    {
        survey$contacts[, paste(columns[["contact.age"]]) := NA_integer_]

        exact.column <- paste(columns[["contact.age"]], "exact", sep="_")
        min.column <- paste(columns[["contact.age"]], "est_min", sep="_")
        max.column <- paste(columns[["contact.age"]], "est_max", sep="_")

        if (exact.column %in% colnames(survey$contacts))
        {
            survey$contacts[, paste(columns[["contact.age"]]) := get(exact.column)]
        }
        if (min.column %in% colnames(survey$contacts) &&
            max.column %in% colnames(survey$contacts) &&
            estimated.contact.age != "missing")
        {
            survey$contacts[is.na(get(columns[["contact.age"]])) & !is.na(get(min.column)) &
                            !is.na(get(max.column)),
                            paste(columns[["contact.age"]]) := as.integer(rowMeans(.SD)),
                            .SDcols=c(min.column, max.column)]
        }
    }

    if (missing.contact.age == "remove" &&
        nrow(survey$contacts[is.na(get(columns[["contact.age"]]))]) > 0)
    {
        missing.age.id <-
            survey$contacts[is.na(get(columns[["contact.age"]])) |
                            get(columns[["contact.age"]]) < min(age.limits),
                            get(columns[["id"]])]
        survey$participants <- survey$participants[!(get(columns[["id"]]) %in% missing.age.id)]
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
                if (columns[["country"]] %in% colnames(survey$participants))
                {
                    survey.countries <- unique(survey$participants[, get(columns[["country"]])])
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
                if (columns[["year"]] %in% colnames(survey$participants))
                {
                    survey.year <-
                        survey$participants[, median(get(columns[["year"]]), na.rm=TRUE)]
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
                    survey$participants[, lower.age.limit :=
                                              reduce_agegroups(get(columns[["participant.age"]]),
                                                               age.limits)]
                survey.pop <- survey.pop[, list(population=.N), by=lower.age.limit]
                survey.pop <- survey.pop[!is.na(lower.age.limit)]
                if (columns[["year"]] %in% colnames(survey$participants))
                {
                    survey.year <-
                        survey$participants[, median(get(columns[["year"]]), na.rm=TRUE)]
                }
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
        survey$participants[, lower.age.limit :=
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
        survey$participants <-
            merge(survey$participants, lower.upper.age.limits, by="lower.age.limit")
        if (all(is.na(survey.pop$population))) survey.pop[, population := NULL]
    }

    survey$participants[, lower.age.limit := reduce_agegroups(get(columns[["participant.age"]]),
                                                              age.limits[age.limits < max.age])]
    survey$participants[, age.group :=
                              cut(survey$participants[, get(columns[["participant.age"]])],
                                  breaks = union(age.limits, max.age),
                                  right = FALSE)]
    age.groups <- survey$participants[, levels(age.group)]
    age.groups[length(age.groups)] <-
        paste0(max(survey$participants$lower.age.limit, na.rm=TRUE), "+")
    survey$participants[, age.group :=
                              factor(age.group, levels=levels(age.group), labels=age.groups)]

    survey$participants[, weight := 1]
    survey$contacts[, weight := 1]
    ## assign weights to participants, to account for weekend/weekday variation
    if (weigh.dayofweek) {
        found.dayofweek <- FALSE
        for (table in surveys)
        {
            if ("dayofweek" %in% colnames(survey[[table]]))
            {
                survey[[table]][dayofweek %in% 1:5, weight := 5]
                survey[[table]][!(dayofweek %in% 1:5), weight := 2]
                found.dayofweek <- TRUE
            }
        }
        if (!found.dayofweek)
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
            part.sample <- survey$participants[sample(.N, replace = T)]
        } else
        {
            ## just use all participants
            part.sample <- survey$participants
        }

        ## sample estimated contact ages
        if (estimated.contact.age == "sample")
        {
            survey$contacts[!is.na(get(min.column)) & !is.na(get(max.column)),
                            paste(columns[["contact.age"]]) :=
                                as.integer(runif(.N,
                                                 as.integer(min(get(min.column),
                                                                get(max.column))),
                                                 as.integer(max(get(min.column),
                                                                get(max.column)))))]
        }

        ## gather contacts for sampled participants
        contacts.sample <-
            data.table(merge(survey$contacts, part.sample, by = columns[["id"]], all = F,
                             allow.cartesian = T, suffixes=c(".cont", ".part")))
        contacts.sample[, weight := weight.cont * weight.part]

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
            for (this.age.group in
                 unique(contacts.sample[is.na(get(columns[["contact.age"]])), age.group]))
            {
                ## first, deal with missing age
                if (nrow(contacts.sample[!is.na(get(columns[["contact.age"]])) &
                                         age.group == this.age.group]) > 0)
                {
                    ## some contacts in the age group have an age, sample from these
                    contacts.sample[is.na(get(columns[["contact.age"]])) &
                                    age.group == this.age.group,
                                    paste(columns[["contact.age"]]) :=
                                        sample(contacts.sample[!is.na(get(columns[["contact.age"]])) &
                                                               age.group == this.age.group,
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
                                    age.group == this.age.group,
                                    paste(columns[["contact.age"]]) :=
                                      as.integer(floor(runif(.N, min = min.contact.age,
                                                             max = max.contact.age + 1)))]
                }
            }
        }
        ## set contact age groups
        max.contact.age <- contacts.sample[, max(get(columns[["contact.age"]]), na.rm = TRUE) + 1]
        contacts.sample[, contact.age.group :=
                              cut(get(columns[["contact.age"]]),
                                  breaks = union(age.limits, max.contact.age),
                                  labels = age.groups,
                                  right = FALSE)]

        ## further weigh contacts if columns are specified
        if (length(weights) > 0) {
            for (i in 1:length(weights)) {
                contacts.sample[, weight := weight * get(weights[i])]
            }
        }
        ## normalise weights
        contacts.sample[, weight := weight / sum(weight) * nrow(contacts.sample)]
        part.sample[, weight := weight / sum(weight) * nrow(part.sample)]


        ## normalise weights

        ## calculate weighted contact matrix
        weighted.matrix <- xtabs(data = contacts.sample,
                                 formula = weight ~ age.group + contact.age.group,
                                 addNA = TRUE)

        dim.names <- dimnames(weighted.matrix)

        if (!counts) { ## normalise to give mean number of contacts
            ## calculate normalisation vector
            norm.vector <- xtabs(data = part.sample, formula = weight ~ age.group, addNA = TRUE)

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
                ## set c_{ij} N_i and c_{ji} N_j (which should both be equal) to
                ## 0.5 * their sum; then c_{ij} is that sum / N_i
                normalised.weighted.matrix <- diag(survey.pop$population) %*% weighted.matrix
                weighted.matrix <- 0.5 * diag(1/survey.pop$population) %*%
                    (normalised.weighted.matrix + t(normalised.weighted.matrix))

            }
        }

        ret[[i]] <- list()

        if (split)
        {
            if (counts)
            {
                warning("'split=TRUE' does not make sense with 'counts=TRUE'; ",
                        "will not split the contact matrix.")
            } else if (na.present)
            {
                warning("'split=TRUE' does not work with missing data; ",
                        "will not split contact.matrix.\n",
                        warning.suggestion)
                ret[[i]][["mean.contacts"]] <- NA
                ret[[i]][["normalisation"]] <- NA
                ret[[i]][["contacts"]] <- rep(NA, nrow(weighted.matrix))
            } else
            {
                ## get rid of name but preserve row and column names
                weighted.matrix <- unname(weighted.matrix)

                norm.vector <- xtabs(data = part.sample, formula = weight ~ age.group, addNA = TRUE)
                nb.contacts <- apply(weighted.matrix, 1, sum)
                mean.contacts <- sum(norm.vector*nb.contacts)/sum(norm.vector)
                spectrum.matrix <- weighted.matrix
                spectrum.matrix[is.na(spectrum.matrix)] <- 0
                spectrum <- as.numeric(eigen(spectrum.matrix, only.values = TRUE)$values[1])
                ret[[i]][["mean.contacts"]] <- mean.contacts
                ret[[i]][["normalisation"]] <- spectrum/mean.contacts

                age.proportions <- survey.pop$population / sum(survey.pop$population)
                weighted.matrix <-
                    diag(1 / nb.contacts) %*% weighted.matrix %*% diag(1 / age.proportions)
                nb.contacts <- nb.contacts / spectrum
                ret[[i]][["contacts"]] <- nb.contacts
            }
        }

        dimnames(weighted.matrix) <- dim.names
        ret[[i]][["matrix"]] <- weighted.matrix
    }

    if (exists("survey.year")) {
        survey.pop[, year := survey.year]
        survey.pop <- survey.pop[, list(lower.age.limit, population,
                                        proportion=population/sum(population), year)]
    }

    ## get number of participants in each age group
    if (any(is.na(part.sample$age.group))) {
        useNA <- "always"
    } else {
        useNA <- "no"
    }
    part.pop <- data.table(table(survey$participants[, age.group], useNA = useNA))
    setnames(part.pop, c("lower.age.limit", "participants"))
    part.pop[, proportion := participants / sum(participants)]

    if (length(ret) > 1) return_value <- list(matrices = ret)
    else if (length(ret) == 1) return_value <- ret[[1]]
    else return_value <- NULL

    if (!is.null(return_value)) {
        if (need.survey.pop) return_value[["demography"]] <- survey.pop[]
        return_value[["participants"]] <- part.pop[]
    }

    return(return_value)
}
