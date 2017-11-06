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
##' @param weights columns that contain weights
##' @param part.age.column column indicating age in participant data
##' @param contact.age.column column indicating age in contact data
##' @param id.column column to match participants with contacts
##' @param dayofweek.column column indicating the day of the week
##' @param country.column column indicating the country
##' @param year.column column indicating the year
##' @param quiet if TRUE, suppress messages
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
contact_matrix <- function(survey = "POLYMOD", countries, survey.pop, age.limits, filter, n = 1, bootstrap = FALSE,  symmetric = TRUE, normalise = FALSE, split = FALSE, weights = c(), part.age.column = "participant_age", contact.age.column = "cnt_age_mean", id.column = "global_id", dayofweek.column = "day_of_week", country.column = "country", year.column = "year", quiet = FALSE, ...)
{
    survey_data <- get_survey(survey)

    ## check if specific countries are requested (if a survey contains data from multiple countries)
    if (!missing(countries) & country.column %in% names(survey_data[["participants"]]))
    {
        survey_data[["participants"]] <- survey_data[["participants"]][get(country.column) %in% countries]
    }

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
            if (country.column %in% names(survey_data[["participants"]]))
            {
                survey.countries <- unique(survey_data[["participants"]][[country.column]])
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
            if (year.column %in% names(survey_data[["participants"]]))
            {
                survey.year <- median(survey_data[["participants"]][[year.column]], na.rm = TRUE)
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
            if (missing(age.limits)) {
                stop("Without population data, 'age.limits' must be given.")
            }
            survey.pop <- survey_data[["participants"]][, lower.age.limit := reduce_agegroups(get(part.age.column), age.limits)]
            survey.pop <- survey.pop[, list(population=.N), by=lower.age.limit]
            survey.pop <- survey.pop[!is.na(lower.age.limit)]
        }
    }

    ## adjust age groups by interpolating, in case they don't match between demographic and survey data
    if (missing(age.limits)) age.limits <- NA_integer_
    survey.pop <- data.table(pop_age(survey.pop, age.limits, ...))

    if (nrow(survey.pop) == 0)
    {
        stop("Could not construct survey population data.")
    }

    ret <- list()

    if (n > 0)
    {
        ## get data
        participants <- copy(data.table(survey_data[["participants"]]))
        contacts <- copy(data.table(survey_data[["contacts"]]))

        ## check if any filters have been requested
        if (nrow(contacts) > 0 && !missing(filter)) {
            missing_columns <- setdiff(names(filter), colnames(contacts))
            if (length(missing_columns) > 0) {
                warning("filter column(s) ", paste(missing_columns),
                        " not found")
                filter <- filter[names(filter) %in% colnames(contacts)]
            }
            ## filter contact data
            for (column in names(filter)) {
                contacts <- contacts[get(column) == filter[[column]]]
            }
        }

        if (nrow(participants[is.na(get(part.age.column))]) > 0) {
            warning("removing participants with no age recorded")
        }
        participants <- participants[!is.na(get(part.age.column))]

        ## check maximum age in the data
        max.age <- min(max(participants[, get(part.age.column)]),
                       max(contacts[, get(contact.age.column)], na.rm = TRUE)) + 1

        ## possibly adjust age groups according to maximum age (so as not to have empty age groups)
        survey.pop[, lower.age.limit := reduce_agegroups(lower.age.limit, lower.age.limit[lower.age.limit < max.age])]
        survey.pop <- survey.pop[, list(population = sum(population)), by = lower.age.limit]

        ## assign age group to participants
        participants[, lower.age.limit := reduce_agegroups(get(part.age.column), survey.pop$lower.age.limit)]
        present.lower.age.limits <-
            participants[, .N, by = lower.age.limit][N > 1]$lower.age.limit
        present.lower.age.limits <-
            present.lower.age.limits[order(present.lower.age.limits)]

        ## reduce to all lower limits (of age groups) that exist in the data
        survey.pop[, lower.age.limit := reduce_agegroups(lower.age.limit, present.lower.age.limits)]
        survey.pop <- survey.pop[, list(population = sum(population)), by = lower.age.limit]

        ## set upper age limits
        survey.pop[, upper.age.limit := c(survey.pop$lower.age.limit[-1], max.age)]
        participants <-
            merge(participants,
                  survey.pop[, list(lower.age.limit, upper.age.limit)],
                  by="lower.age.limit")

        participants[, agegroup := cut(participants[, get(part.age.column)],
                                       breaks = union(present.lower.age.limits, max.age),
                                       right = FALSE)]

        ## assign weights to participants, to account for weekend/weekday variation
        if (dayofweek.column %in% colnames(participants))
        {
            participants[get(dayofweek.column) %in% 1:5, weight := 5 / nrow(participants[get(dayofweek.column) %in% 1:5])]
            participants[!(get(dayofweek.column) %in% 1:5),
                         weight := 2 / nrow(participants[!(get(dayofweek.column) %in% 1:5)])]
        } else
        {
            participants[, weight := 1]
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
                weighted.matrix <- diag(1 / age_proportions) %*% weighted.matrix %*% diag(1 / nb_contacts)
                ret[[i]][["contacts"]] <- nb_contacts
            }

            rownames(weighted.matrix) <- cols
            colnames(weighted.matrix) <- cols

            ret[[i]][["matrix"]] <- weighted.matrix
        }
    }

    if (exists("survey.year")) {
        survey.pop[, year := survey.year]
    }

    if (length(ret) > 1)
        return(list(matrices = ret, survey = survey.pop))
    else if (length(ret) == 1)
        return(c(ret[[1]], list(demography = survey.pop)))
    else
        return(list(demography = survey.pop))
}

