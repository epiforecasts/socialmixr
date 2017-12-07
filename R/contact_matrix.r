##' Generate a contact matrix from diary survey data
##'
##' Samples a contact survey using a bootstrap
##'
##' @param countries limit to one or more countries; if not given, will use all countries in the survey
##' @param survey.pop survey population -- either a data frame with columns 'lower.age.limit' and 'population', or a character vector giving the name(s) of a country or countries from the list that can be obtained via \code{wpp_countries}; if not given, will use the country populations from the chosen countries, or all countries in the survey if \code{countries} is not given
##' @param filter any filters to apply to the data, given as list of the form (column=filter_value) - only contacts that have 'filter_value' in 'column' will be considered
##' @param n number of matrices to sample
##' @param bootstrap whether to sample using a bootstrap; by default, will use bootstrap if n > 1
##' @param counts whether to return counts (instead of means)
##' @param symmetric whether to make matrix symmetric
##' @param split whether to split the number of contacts and assortativity
##' @param weigh.dayofweek whether to weigh the day of the week (weight 5 for weekdays ans 2 for weekends)
##' @param weights columns that contain weights
##' @return a list of sampled contact matrices, and the underlying demography of the surveyed population
##' @importFrom stats xtabs runif median
##' @importFrom utils data
##' @importFrom data.table data.table setnames copy
##' @export
##' @inheritParams get_survey
##' @inheritParams pop_age
##' @inheritParams check_survey
##' @examples
##' m <- contact_matrix()
##' m <- contact_matrix(n = 5)
##' m <- contact_matrix(split = TRUE)
##' m <- contact_matrix(survey = "POLYMOD", countries = "United Kingdom", age.limits = c(0, 1, 5, 15))
##' @author Sebastian Funk
contact_matrix <- function(survey="polymod", countries=c(), survey.pop, age.limits, filter, n = 1, bootstrap, counts = FALSE, symmetric = FALSE, split = FALSE, weigh.dayofweek = FALSE, weights = c(), quiet = FALSE)
{
    ## get the survey
    survey <- get_survey(survey, quiet)

    ## copy data
    survey <- lapply(survey, function(x) {copy(data.table(x))})

    ## if bootstrap not asked for
    if (missing(bootstrap)) bootstrap <- (n > 1)

    ## check if specific countries are requested (if a survey contains data from multiple countries)
    if (length(countries) > 0 & "country" %in% colnames(survey$participants))
    {
        missing_countries <- setdiff(countries, survey$participants$country)
        if (length(missing_countries) > 0) {
            stop("Survey data not found for ", paste(missing_countries, sep=","), ".")
        }
        survey$participants <- survey$participants[country %in% countries]
    }

    ## check maximum age in the data
    max.age <- max(survey$participants[, part_age], na.rm = TRUE) + 1
    if (missing(age.limits)) age.limits <- c(0, seq_len(max.age))

    ## check if any filters have been requested
    if (nrow(survey$contacts) > 0 && !missing(filter)) {
        missing_columns <- setdiff(names(filter), colnames(survey$contacts))
        if (length(missing_columns) > 0) {
            warning("filter column(s) ", paste(missing_columns), " not found")
            filter <- filter[names(filter) %in% colnames(survey$contacts)]
        }
        ## filter contact data
        for (column in names(filter)) {
            survey$contacts <- survey$contacts[get(column) == filter[[column]]]
        }
    }

    if (nrow(survey$participants[is.na(part_age)]) > 0) {
        warning("removing participants with no age recorded")
        survey$participants <- survey$participants[!is.na(part_age)]
    }

    ## adjust age groups according to what is in the data
    ## possibly adjust age groups according to maximum age (so as not to have empty age groups)
    survey$participants[, lower.age.limit := reduce_agegroups(part_age, age.limits[age.limits < max.age])]
    present.lower.age.limits <-
        survey$participants[, .N, by = lower.age.limit][N > 1]$lower.age.limit
    present.lower.age.limits <-
        present.lower.age.limits[order(present.lower.age.limits)]

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
                ## survey population not given but countries requested from survey - get population data from those countries
                survey.countries <- countries
            } else
            {
                ## neither survey population nor country names given - try to guess country or countries surveyed from participant data
                if ("country" %in% names(survey$participants))
                {
                    survey.countries <- unique(survey$participants[, country])
                } else
                {
                    warning("No 'survey.pop' or 'countries' given, and no 'country' column found in the data. I don't know which population this is from. Assuming the survey is representative")
                    survey.representative=TRUE
                }
            }

            if (!survey.representative) {
                ## get population data for countries from 'wpp' package
                country.pop <- data.table(wpp_age(survey.countries))

                ## check if survey data are from a specific year - in that case use demographic data from that year, otherwise latest
                if ("year" %in% colnames(survey$participants))
                {
                    survey.year <- survey$participants[, median(year, na.rm=TRUE)]
                } else
                {
                    survey.year <- country.pop[, max(year, na.rm=TRUE)]
                    warning("No 'year' column found in the data. Will use ", survey.year, " population data.")
                }

                ## check if any survey countries are not in wpp
                missing.countries <- setdiff(survey.countries, unique(country.pop$country))
                if (length(missing.countries) > 0)
                {
                    warning("Could not find population data for ", paste(missing.countries, collapse = ", "), ". ",
                            " Use wpp_countries() to get a list of country names.")
                }

                if (length(missing.countries) == length(survey.countries)) {
                    warning("No survey data available for any of the requested data. I don't know which population this is from. Assuming the survey is representative")
                    survey.representative <- TRUE
                } else {
                    ## get demographic data closest to survey year
                    country.pop.year <- unique(country.pop[, year])
                    survey.year <- min(country.pop.year[which.min(abs(survey.year - country.pop.year))])
                    survey.pop <- country.pop[year == survey.year][, list(population = sum(population)), by = "lower.age.limit"]
                }
            }

            if (survey.representative) {
                survey.pop <- survey$participants[, lower.age.limit := reduce_agegroups(part_age, age.limits)]
                survey.pop <- survey.pop[, list(population=.N), by=lower.age.limit]
                survey.pop <- survey.pop[!is.na(lower.age.limit)]
                if ("year" %in% colnames(survey$participants))
                {
                    survey.year <- survey$participants[, median(year, na.rm=TRUE)]
                }
                survey.year <- survey$participants[, median(year, na.rm=TRUE)]
            }
        }
        ## adjust age groups by interpolating, in case they don't match between demographic and survey data
        survey.pop <- data.table(pop_age(survey.pop, age.limits))

        if (nrow(survey.pop) == 0)
        {
            warning("Could not construct survey population data.")
            survey.pop <- data.table(lower.age.limit=age.limits, population=NA_integer_)
        }

        ## possibly adjust age groups according to maximum age (so as not to have empty age groups)
        survey.pop[, lower.age.limit := reduce_agegroups(lower.age.limit, present.lower.age.limits)]
        setkey(survey.pop, lower.age.limit)
        ## re-assign lower age limits in participants
        survey$participants[, lower.age.limit :=
                                  reduce_agegroups(part_age, survey.pop$lower.age.limit)]
        present.lower.age.limits <- unique(survey.pop$lower.age.limit)
        present.lower.age.limits <-
            present.lower.age.limits[order(present.lower.age.limits)]

        ## set upper age limits
        survey.pop[, upper.age.limit := c(survey.pop$lower.age.limit[-1], max.age)]

        lower.upper.age.limits <-
            data.table(lower.age.limit = present.lower.age.limits,
                       upper.age.limit = c(present.lower.age.limits[-1], max.age))
        ## set upper age limits and construct age groups
        survey$participants <- merge(survey$participants, lower.upper.age.limits)
        if (all(is.na(survey.pop$population))) survey.pop[, population := NULL]
    }

    survey$participants[, agegroup := cut(survey$participants[, part_age],
                                          breaks = union(survey$participants$lower.age.limit, max.age),
                                          right = FALSE)]

    ## get number of participants in each age group
    part.pop <- data.table(table(survey$participants[, lower.age.limit]))
    setnames(part.pop, c("lower.age.limit", "participants"))
    part.pop[, proportion := participants / sum(participants)]

    survey$participants[, weight := 1]
    ## assign weights to participants, to account for weekend/weekday variation
    if (weigh.dayofweek) {
        if (dayofweek.column %in% colnames(survey$participants))
        {
            survey$participants[dayofweek %in% 1:5, weight := 5 / nrow(survey$participants[dayofweek %in% 1:5])]
            survey$participants[!(dayofweek %in% 1:5), weight := 2 / nrow(survey$participants[!(dayofweek %in% 1:5)])]
        } else
        {
            warning("'weigh.dayofweek' is TRUE, but no day of week column in the data. Will ignore.")
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
            part.sample <- survey$participants[sample(nrow(survey$participants), replace = T)]
        } else
        {
            ## just use all participants
            part.sample <- copy(survey$participants)
        }

        ## gather contacts for sampled participants
        contacts.sample <-
            data.table(merge(survey$contacts, part.sample, by = "global_id", all = F, allow.cartesian = T))

        ## sample contacts
        for (this.agegroup in unique(contacts.sample[is.na(cnt_age_mean), agegroup]))
        {
            ## first, deal with missing age
            if (nrow(contacts.sample[!is.na(cnt_age_mean) & agegroup == this.agegroup]) > 0)
            {
                ## some contacts in the age group have an age, sample from these
                contacts.sample[is.na(cnt_age_mean) & agegroup == this.agegroup,
                                cnt_age_mean :=
                                    sample(contacts.sample[!is.na(cnt_age_mean) & agegroup == this.agegroup, cnt_age_mean], size = .N, replace = TRUE)]
            } else {
                ## no contacts in the age group have an age, sample uniformly between limits
                contacts.sample[is.na(cnt_age_mean) & agegroup == this.agegroup,
                                cnt_age_mean := runif(.N, min = lower.age.limit, max = upper.age.limit - 1)]
            }
        }
        ## set contact age groups
        contacts.sample[, cnt.agegroup := cut(cnt_age_mean,
                                              breaks = union(present.lower.age.limits, max.age),
                                              right = FALSE)]

        ## further weigh contacts if columns are specified
        if (length(weights) > 0) {
            for (i in 1:length(weights)) {
                contacts.sample[, weight := weight * get(weights[i])]
            }
        }

        ## calculate weighted contact matrix
        weighted.matrix <- xtabs(data = contacts.sample,
                                 formula = weight ~ agegroup + cnt.agegroup)

        if (symmetric & prod(dim(as.matrix(weighted.matrix))) > 1) {
            weighted.matrix <- 0.5 * (weighted.matrix + t(weighted.matrix))
        }

         if (!counts) { ## normalise to give mean number of contacts
            ## calculate normalisation vector
            norm.vector <- xtabs(data = part.sample, formula = weight ~ agegroup)

            ## normalise contact matrix
            weighted.matrix <- apply(weighted.matrix, 2, function(x) {x/norm.vector})
        }

        ## get rid of name but preserve row and column names
        cols <- rownames(weighted.matrix)
        weighted.matrix <- unname(weighted.matrix)

        ret[[i]] <- list()


        if (split)
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

        rownames(weighted.matrix) <- cols
        colnames(weighted.matrix) <- cols

        ret[[i]][["matrix"]] <- weighted.matrix
    }

    if (exists("survey.year")) {
        survey.pop[, year := survey.year]
        survey.pop <- survey.pop[, list(lower.age.limit, population,
                                        proportion=population/sum(population), year)]
    }
    if (length(ret) > 1)
        return_value[["matrices"]] <- list(matrices = ret)
    else if (length(ret) == 1)
        return_value <- ret[[1]]
    else return_value <- NULL

    if (need.survey.pop) return_value[["demography"]] <- survey.pop[]
    return_value[["participants"]] <- part.pop[]

    return(return_value)
}

