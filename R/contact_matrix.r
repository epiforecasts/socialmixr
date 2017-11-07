##' Generate a contact matrix from diary survey data
##'
##' Samples a contact survey using a bootstrap
##'
##' @param countries limit to one or more countries; if not given, will use all countries in the survey
##' @param survey.pop survey population -- either a data frame with columns 'lower.age.limit' and 'population', or a character vector giving the name(s) of a country or countries from the list that can be obtained via \code{wpp_countries}; if not given, will use the country populations from the chosen countries, or all countries in the survey if \code{countries} is not given
##' @param filter any filters to apply to the data, given as list of the form (column=filter_value) - only contacts that have 'filter_value' in 'column' will be considered
##' @param n number of matrices to sample
##' @param bootstrap whether to sample using a bootstrap; will be set to TRUE if n > 1
##' @param symmetric whether to make matrix symmetric
##' @param normalise whether to normalise to eigenvalue 1
##' @param split whether to split the number of contacts and assortativity
##' @param weigh.dayofweek whether to weigh the day of the week (weight 5 for weekdays ans 2 for weekends)
##' @param weights columns that contain weights
##' @param part.age.column column indicating age in participant data
##' @param contact.age.column column indicating age in contact data
##' @param id.column column to match participants with contacts
##' @param dayofweek.column column indicating the day of the week
##' @param country.column column indicating the country
##' @param year.column column indicating the year
##' @param ... further parameters for 'pop_age'
##' @return a list of sampled contact matrices, and the underlying demography of the surveyed population
##' @importFrom stats xtabs runif median
##' @importFrom utils data
##' @importFrom data.table data.table setnames copy
##' @export
##' @inheritParams get_survey
##' @inheritParams pop_age
##' @examples
##' m <- contact_matrix()
##' m <- contact_matrix(n = 5)
##' m <- contact_matrix(normalise = TRUE)
##' m <- contact_matrix(normalise = TRUE, split = TRUE)
##' m <- contact_matrix(survey = "POLYMOD", countries = "United Kingdom", age.limits = c(0, 1, 5, 15))
##' @author Sebastian Funk
contact_matrix <- function(survey = "POLYMOD", countries=c(), survey.pop, age.limits, filter, n = 1, bootstrap = FALSE,  symmetric = TRUE, normalise = FALSE, split = FALSE, weigh.dayofweek = FALSE, weights = c(), part.age.column = "participant_age", contact.age.column = "cnt_age_mean", id.column = "global_id", dayofweek.column = "day_of_week", country.column = "country", year.column = "year", quiet = FALSE, ...)
{
    ## get the survey
    survey_data <- get_survey(survey, quiet)
    ## get data
    participants <- copy(data.table(survey_data[["participants"]]))
    contacts <- copy(data.table(survey_data[["contacts"]]))

    ## check maximum age in the data
    max.age <- min(max(participants[, get(part.age.column)], na.rm = TRUE),
                   max(contacts[, get(contact.age.column)], na.rm = TRUE)) + 1
    if (missing(age.limits)) age.limits <- c(0, seq_len(max.age))

    ## check if specific countries are requested (if a survey contains data from multiple countries)
    if (length(countries) > 0 & country.column %in% names(participants))
    {
        participants <- participants[get(country.column) %in% countries]
    }

    ## are population data needed?
    pop_needed <- split || symmetric

    if (pop_needed) {
        ## now, get demographic data (survey population)
        ## check if survey population is either not given or given as a vector of countries
        if (missing(survey.pop) || is.character(survey.pop))
        {
            survey_representative=FALSE
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
                if (country.column %in% names(participants))
                {
                    survey.countries <- unique(participants[[country.column]])
                } else
                {
                    warning("No 'survey.pop' or 'countries' given, and no country column found in the data. I don't know which population this is from. Assuming the survey is representative")
                    survey_representative=TRUE
                }
            }

            if (!survey_representative) {
                ## get population data for countries from 'wpp' package
                country.pop <- data.table(wpp_age(survey.countries))

                ## check if survey data are from a specific year - in that case use demographic data from that year, otherwise latest
                if (year.column %in% names(participants))
                {
                    survey.year <- median(participants[[year.column]], na.rm = TRUE)
                } else if (missing(year.column))
                {
                    survey.year <- country.pop[, max(year, na.rm=TRUE)]
                    warning("No year column found in the data. Will use ", survey.year, " population data.")
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
                    survey_representative <- TRUE
                } else {
                    ## get demographic data closest to survey year
                    country.pop.year <- unique(country.pop[, year])
                    survey.year <- min(country.pop.year[which.min(abs(survey.year - country.pop.year))])
                    survey.pop <- country.pop[year == survey.year][, list(population = sum(population)), by = "lower.age.limit"]
                }
            }

            if (survey_representative) {
                survey.pop <- participants[, lower.age.limit := reduce_agegroups(get(part.age.column), age.limits)]
                survey.pop <- survey.pop[, list(population=.N), by=lower.age.limit]
                survey.pop <- survey.pop[!is.na(lower.age.limit)]
            }
        }

        ## adjust age groups by interpolating, in case they don't match between demographic and survey data
        survey.pop <- data.table(pop_age(survey.pop, age.limits))

        if (nrow(survey.pop) == 0)
        {
            stop("Could not construct survey population data.")
        }
    }

    ret <- list()

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

    if (nrow(participants[is.na(get(part.age.column))]) > 0) {
        warning("removing participants with no age recorded")
        participants <- participants[!is.na(get(part.age.column))]
    }

    ## adjust age groups according to what is in the data
    ## possibly adjust age groups according to maximum age (so as not to have empty age groups)
    participants[, lower.age.limit := reduce_agegroups(get(part.age.column), age.limits[age.limits <= max.age])]
    present.lower.age.limits <-
        participants[, .N, by = lower.age.limit][N > 1]$lower.age.limit
    present.lower.age.limits <-
        present.lower.age.limits[order(present.lower.age.limits)]

    if (pop_needed) {
        ## possibly adjust age groups according to maximum age (so as not to have empty age groups)
        survey.pop[, lower.age.limit := reduce_agegroups(lower.age.limit, present.lower.age.limits)]
        survey.pop <- survey.pop[, list(population = sum(population)), by = lower.age.limit]

        ## re-assign lower age limits in participants
        participants[, lower.age.limit :=
                           reduce_agegroups(get(part.age.column), survey.pop$lower.age.limit)]
        present.lower.age.limits <- unique(survey.pop$lower.age.limit)
        present.lower.age.limits <-
            present.lower.age.limits[order(present.lower.age.limits)]

        ## set upper age limits
        survey.pop[, upper.age.limit := c(survey.pop$lower.age.limit[-1], max.age)]
    }

    lower.upper.age.limits <-
        data.table(lower.age.limit = present.lower.age.limits,
                   upper.age.limit = c(present.lower.age.limits[-1], max.age))
    ## set upper age limits and construct age groups
    participants <- merge(participants, lower.upper.age.limits)
    participants[, agegroup := cut(participants[, get(part.age.column)],
                                   breaks = union(present.lower.age.limits, max.age),
                                   right = FALSE)]

    participants[, weight := 1]
    ## assign weights to participants, to account for weekend/weekday variation
    if (weigh.dayofweek) {
        if (dayofweek.column %in% colnames(participants))
        {
            participants[get(dayofweek.column) %in% 1:5, weight := 5 / nrow(participants[get(dayofweek.column) %in% 1:5])]
            participants[!(get(dayofweek.column) %in% 1:5),
                         weight := 2 / nrow(participants[!(get(dayofweek.column) %in% 1:5)])]
        } else
        {
            warning("'weigh.dayofweek' is TRUE, but no day of week column in the data. Will ignore.")
        }
    }

    ## get number of participants in each age group
    participants.age <- unname(table(participants[, lower.age.limit]))

    if (n > 1)
    {
        if (missing(bootstrap))
        {
            bootstrap <- TRUE
        } else if (!bootstrap)
        {
            warning("n > 1 does not make sense if not bootstrapping. Will return just one sample.")
            n <- 1
        }
    }

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
        contacts.sample <- data.table(merge(contacts, part.sample, by = id.column,
                                            all = F, allow.cartesian = T))

        ## sample contacts
        for (this.agegroup in unique(contacts.sample[is.na(get(contact.age.column)), agegroup]))
        {
            if (nrow(contacts.sample[!is.na(get(contact.age.column)) & agegroup == this.agegroup]) > 0)
            {
                contacts.sample[is.na(get(contact.age.column)) & agegroup == this.agegroup,
                                paste(contact.age.column) :=
                                    sample(contacts.sample[!is.na(get(contact.age.column)) & agegroup == this.agegroup,
                                                           get(contact.age.column)], size = .N, replace = TRUE)]
            } else {
                contacts.sample[is.na(get(contact.age.column)) & agegroup == this.agegroup,
                                paste(contact.age.column) := runif(.N, min = lower.age.limit, max = upper.age.limit - 1)]
            }
        }
        ## set contact age groups
        contacts.sample[, cnt.agegroup := cut(get(contact.age.column),
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
                                 formula = weight ~ cnt.agegroup + agegroup)
        ## calculate normalisation vector
        norm.vector <- xtabs(data = part.sample, formula = weight ~ agegroup)

        ## normalise contact matrix
        weighted.matrix <- t(apply(weighted.matrix, 1, function(x) { x / norm.vector} ))
        ## get rid of name but preserve row and column names
        cols <- rownames(weighted.matrix)
        weighted.matrix <- unname(weighted.matrix)

        if (symmetric & prod(dim(as.matrix(weighted.matrix))) > 1) {
            ## set C_{ij} N_j and C_{ji} N_i (which should both be equal) to
            ## 0.5 * their sum; then C_{ij} is that sum / N_j
            normalised.weighted.matrix <- weighted.matrix %*% diag(survey.pop$population)
            weighted.matrix <- t(apply(0.5 * (normalised.weighted.matrix +
                                              t(normalised.weighted.matrix)),
                                       1, function(x) { x / survey.pop$population }))
        }

        ret[[i]] <- list()

        if (normalise)
        {
            if (!any(is.na(weighted.matrix)))
            {
                spectrum <- eigen(weighted.matrix, only.values = TRUE)$values[1]
                weighted.matrix <- weighted.matrix / spectrum
                ret[[i]][["normalisation"]] <- spectrum
            } else
            {
                ret[[i]][["normalisation"]] <- NA_real_
            }
        }

        if (split)
        {
            nb_contacts <- apply(weighted.matrix, 2, sum)
            age_proportions <- survey.pop$population / sum(survey.pop$population)
            weighted.matrix <-
                diag(1 / age_proportions) %*% weighted.matrix %*% diag(1 / nb_contacts)
            ret[[i]][["contacts"]] <- nb_contacts
        }

        rownames(weighted.matrix) <- cols
        colnames(weighted.matrix) <- cols

        ret[[i]][["matrix"]] <- weighted.matrix
    }

    if (exists("survey.year")) {
        survey.pop[, year := survey.year]
    }
    if (length(ret) > 1)
        return_value <- list(matrices = ret)
    else if (length(ret) == 1)
        return_value <- ret[[1]]
    else return_value <- NULL

    if (!is.null(return_value) && pop_needed) return_value[["demography"]] <- survey.pop

    return(return_value)
}

