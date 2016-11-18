##' Samples polymod using weights following Baguelin et al. (2013), but
##' using a bootstrap; first, contacts and ages are sampled
##'
##' @param n number of matrices to sample
##' @param age.limits Lower limits of the age groups; if not given, will use 5 year age limits as in the population data
##' @param survey either a survey ("POLYMOD") or a list of 'participants' and 'contacts' (both data frames) to sample from
##' @param countries limit to one or more countries; if not given, will use all countries in the survey
##' @param survey.pop survey population -- either a data frame with columns lower.age.limit and population, or a character vector giving the name(s) to use with the 2013 WHO population; if not given, will use the country populations from the desired countries, or all countries in the survey if \code{countries} is not given
##' @param bootstrap whether to sample using a bootstrap; will be set to TRUE if n > 1
##' @param symmetric whether to make matrix symmetric
##' @param normalise whether to normalise to eigenvalue 1
##' @param split whether to split the number of contacts and assortativity
##' @param part.age.column column indicating age in participant data
##' @param contact.age.column column indicating age in contact data
##' @param id.column column to match participants with contacts
##' @param dayofweek.column column indicating the day of the week
##' @param country.column column indicating the country
##' @param add.weights additional weight columns (e.g., minutes etc
##' @param symmetry make contact matrix symmetric
##' @return a list of sampled contact matrices, and the underlying demography
##' @import wpp2015
##' @importFrom reshape2 melt
##' @importFrom data.table data.table setnames
##' @export
##' @author Sebastian Funk
contact_matrix <- function(n = 1, age.limits, survey = "polymod", countries, survey.pop, mixing.pop, bootstrap = FALSE,  symmetric = TRUE, normalise = FALSE, split = FALSE, add.weights = c(), part.age.column = "participant_age", contact.age.column = "cnt_age_mean", id.column = "global_id", dayofweek.column = "day_of_week", country.column = "country", year.column = "year")
{
    ## load population data if necessary
    if ((missing(survey.pop) || is.character(survey.pop)) &&
        (missing(mixing.pop) || is.character(mixing.pop)))
    {
        data(popM, package = "wpp2015")
        data(popF, package = "wpp2015")

        popM <- data.table(popM)
        popF <- data.table(popF)

        popM <- popM[, sex := "male"]
        popF <- popF[, sex := "female"]

        pop <- rbind(popM, popF)

        pop <- melt(pop, id.vars = c("country", "country_code", "age", "sex"), variable.name = "year")
        pop <- data.table(dcast(pop, country + country_code + age + year ~ sex, value.var = "value"))
        pop <- pop[, year := as.integer(as.character(year))]
        pop <- pop[, lower.age.limit := as.integer(sub("[-+].*$", "", age))]
        pop <- pop[, list(country, lower.age.limit, year, population = female + male)]
    }

    ## check if survey is given as character
    if (is.character(survey))
    {
        if (tolower(survey) == "polymod")
        {
            survey_data <- list(participants = polymod$participants,
                                contacts = polymod$contacts)
        } else
        {
            stop("Unknown survey, '", survey, "''")
        }
    } else
    {
        if (!is.list(survey) || is.null(names(survey)) || !(all(names(survey) %in% c("participants", "contacts"))))
        {
            stop("'survey' must be either a character string or a named list with elements named 'participants' and 'contacts'")
        }
        survey_data <- survey
    }
    survey_data <- lapply(survey_data, data.table)

    if (!missing(countries))
    {
        survey_data[["participants"]] <- survey_data[["participants"]][get(country.column) %in% countries]
    }

    if (missing(survey.pop))
    {
        if (missing(countries))
        {
            if (country.column %in% names(survey_data[["participants"]]))
            {
                survey.countries <- unique(survey_data[["participants"]][[country.column]])
            } else
            {
                stop("No 'survey.pop' and or 'countries' given, and no country column found in the data. I don't know which population this is from.")
            }
        } else
        {
            survey.countries <- countries
        }

        if (year.column %in% names(survey_data[["participants"]]))
        {
            survey.year <- round(mean(survey_data[["participants"]][[year.column]], na.rm = TRUE) / 5) * 5
        } else if (missing(year.column))
        {
            survey.year <- pop[, max(year)]
            warning("No year column found in the data. Will use ", survey.year, " data.")
        }

        missing.countries <- setdiff(survey.countries, survey_data[["participants"]][[country.column]])
        if (length(missing.countries) > 0)
        {
            warning("Could not find population data for ", paste(missing.countries, collapse = ", "), ". ",
                    " Use wpp_countries() to get a list of country names.")
        }

        survey.pop <- pop[country %in% survey.countries & year == survey.year][, list(population = sum(population) * 1000), by = "lower.age.limit"]
    }

    ages <- survey.pop

    if (missing(age.limits)) {
        age.limits <- unique(survey.pop$lower.age.limits)
    } else {
        ages[, lower.age.limit := reduce.agegroups(lower.age.limit, age.limits)]
        ages <- ages[, list(population = sum(population)), by = lower.age.limit]
    }
    setkey(ages, lower.age.limit)

    ret <- list()

    ## clean participant data of age NAs
    participants <- copy(data.table(survey_data[["participants"]]))
    contacts <- copy(data.table(survey_data[["contacts"]]))
    if (nrow(participants[is.na(get(part.age.column))]) > 0) {
        warning("removing participants with no age recorded")
    }
    participants <- participants[!is.na(get(part.age.column))]

    ## check maximum age in the data
    max.age <- min(max(participants[, get(part.age.column)]),
                   max(contacts[, get(contact.age.column)], na.rm = TRUE)) + 1

    ## possibly adjust age groups according to maximum age (so as not to have empty age groups)
    ages[, lower.age.limit := reduce.agegroups(lower.age.limit, lower.age.limit[lower.age.limit < max.age])]
    ages <- ages[, list(population = sum(population)), by = lower.age.limit]

    ## assign age group to participants
    participants[, lower.age.limit := reduce.agegroups(get(part.age.column), ages$lower.age.limit)]
    present.lower.age.limits <-
        participants[, .N, by = lower.age.limit][N > 1]$lower.age.limit
    present.lower.age.limits <-
        present.lower.age.limits[order(present.lower.age.limits)]

    ## reduce to all lower limits that exist in the data
    ages[, lower.age.limit := reduce.agegroups(lower.age.limit, present.lower.age.limits)]
    ages <- ages[, list(population = sum(population)), by = lower.age.limit]

    ## set upper age limits
    ages[, upper.age.limit := c(ages$lower.age.limit[-1], max.age)]

    participants[, agegroup := cut(participants[, get(part.age.column)],
                                   breaks = union(present.lower.age.limits, max.age),
                                   right = FALSE)]

    ## assign weights to participants, to account for weekend/weekday
    ## variation
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
        ## age groups
        contacts.sample[, cnt.agegroup := cut(get(contact.age.column),
                                              breaks = union(present.lower.age.limits, max.age),
                                              right = FALSE)]

        ## further weigh contacts if columns are specified
        if (length(add.weights) > 0) {
            for (i in 1:length(add.weights)) {
                contacts.sample[, weight := weight * get(add.weights)]
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
            normalised.weighted.matrix <- t(apply(weighted.matrix, 1,
                                                  function(x) { x * ages$population }))
            weighted.matrix <- t(apply(0.5 * (normalised.weighted.matrix +
                                              t(normalised.weighted.matrix)),
                                       1, function(x) { x / ages$population }))
        }

        rownames(weighted.matrix) <- cols
        colnames(weighted.matrix) <- cols

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
            contacts <- apply(weighted.matrix, 2, sum)
            age_proportions <- ages$population / sum(ages$population)
            weighted.matrix <- t(t(weighted.matrix / contacts) / age_proportions)
            ret[[i]][["contacts"]] <- contacts
        }

        ret[[i]][["matrix"]] <- weighted.matrix
    }

    if (length(ret) > 1)
        return(list(matrices = ret, demography = ages))
    else if (length(ret) == 1)
        return(c(ret[[1]], list(demography = ages)))
    else
        stop("No matrix.")
}

