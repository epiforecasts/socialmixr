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
##' @param symmetric whether to make matrix symmetric, such that c_{ij}N_i = c{ji}N_j.
##' @param split whether to split the number of contacts and assortativity
##' @param estimated.participant.age if set to "mean" (default), people whose ages are given as a range (in columns named "..._est_min" and "..._est_max") but not exactly (in a column named "..._exact") will have their age set to the mid-point of the range; if set to "sample", the age will be sampled from the range; if set to "missing", age ranges will be treated as missing
##' @param estimated.contact.age if set to "mean" (default), contacts whose ages are given as a range (in columns named "..._est_min" and "..._est_max") but not exactly (in a column named "..._exact") will have their age set to the mid-point of the range; if set to "sample", the age will be sampled from the range; if set to "missing", age ranges will be treated as missing
##' @param missing.participant.age if set to "remove" (default), participants without age information are removed; if set to "keep", participants with missing age are kept and treated as a separate age group
##' @param missing.contact.age if set to "remove" (default), participants that have contacts without age information are removed; if set to "sample", contacts without age information are sampled from all the contacts of participants of the same age group; if set to "keep", contacts with missing age are kept and treated as a separate age group; if set to "ignore", contact with missing age are ignored in the contact analysis
##' @param weights columns that contain weights
##' @param weigh.dayofweek whether to weigh the day of the week (weight (5/7 / N_week/N) for weekdays and (2/7 / N_weekend/N) for weekends)
##' @param weigh.age whether to weigh by the age of the participants (vs. the populations' age distribution)
##' @param weight.threshold threshold value for the standardized weights before running an additional standardisation (default 'NA' = no cutoff)
##' @param sample.all.age.groups what to do if bootstrapping fails to sample participants from one or more age groups; if FALSE (default), corresponding rows will be set to NA, if TRUE the sample will be discarded and a new one taken instead
##' @param quiet if set to TRUE, output is reduced
##' @param return.demography boolean to explicitly return demography data that corresponds to the survey data (default 'NA' = if demography data is requested by other function parameters)
##' @param return.part.weights boolean to return the participant weights
##' @param per.capita wheter to return a matrix with contact rates per capita (default is FALSE and not possible if 'counts=TRUE' or 'split=TRUE')
##' @param ... further arguments to pass to \code{\link{get_survey}}, \code{\link{check}} and \code{\link{pop_age}} (especially column names)
##' @return a list of sampled contact matrices, and the underlying demography of the surveyed population
##' @importFrom stats xtabs runif median
##' @importFrom utils data globalVariables
##' @importFrom countrycode countrycode
##' @import data.table
##' @export
##' @examples
##' data(polymod)
##' contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))
##' @author Sebastian Funk
contact_matrix <- function(survey, countries=c(), survey.pop, age.limits, filter, n = 1, bootstrap, counts = FALSE, symmetric = FALSE, split = FALSE, estimated.participant.age=c("mean", "sample", "missing"), estimated.contact.age=c("mean", "sample", "missing"), missing.participant.age = c("remove", "keep"), missing.contact.age = c("remove", "sample", "keep", "ignore"), weights = c(), weigh.dayofweek = FALSE, weigh.age = FALSE, weight.threshold = NA, sample.all.age.groups = FALSE, quiet = FALSE, return.part.weights = FALSE, return.demography = NA, per.capita = FALSE, ...)
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
    weight.cont <- NULL
    weight.part <- NULL
    id <- NULL
    sampled.weight <- NULL
    bootstrap.weight <- NULL
    participants <- NULL
    sum_weight <- NULL

    age.count <- NULL
    age.proportion <- NULL
    population.proportion <- NULL
    population.count <- NULL
    weight.age <- NULL
    participant.age <- NULL
    age.count <- NULL
    age.proportion <- NULL
    is.weekday <- NULL

    # study.year <- NULL
    # survey.year <- NULL
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
    estimated.participant.age <- match.arg(estimated.participant.age)
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
        survey$participants[, paste(columns[["country"]]) :=
                                countrycode(get(columns[["country"]]),
                                            "country.name", "country.name")]
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
    part_min.column <- paste(columns[["participant.age"]], "est_min", sep="_")
    part_max.column <- paste(columns[["participant.age"]], "est_max", sep="_")

    if (!(columns[["participant.age"]] %in% colnames(survey$participants)))
    {
        survey$participants[, paste(columns[["participant.age"]]) := NA_integer_]
    }

    if (!(part_max.column %in% colnames(survey$participants)) &
        (columns[["participant.age"]] %in% colnames(survey$participants))
    )
    {
        max.age <- max(survey$participants[, get(columns[["participant.age"]])], na.rm = TRUE) +  1

    } else if (part_max.column %in% colnames(survey$participants) &
               (columns[["participant.age"]] %in% colnames(survey$participants)))
    {
        max.age <- max(
            c(
                survey$participants[, get(columns[["participant.age"]])],
                survey$participants[, get(part_max.column)]
            ),
            na.rm = TRUE
        ) + 1
    } else if(part_max.column %in% colnames(survey$participants)){
        max.age <- max(survey$participants[, get(columns[["participant.age"]])], na.rm = TRUE) +  1

    }

    if (missing(age.limits))
    {
        all.ages <-
            unique(as.integer(survey$participants[, get(columns[["participant.age"]])]))
        all.ages <- all.ages[!is.na(all.ages)]
        all.ages <- all.ages[order(all.ages)]
        age.limits <- union(0, all.ages)
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

    ## sample estimated participant ages
    if (part_min.column %in% colnames(survey$participants) &&
        part_max.column %in% colnames(survey$participants))
    {
        if (estimated.participant.age == "mean")
        {
            survey$participants[is.na(get(columns[["participant.age"]])) &
                                    !is.na(get(part_min.column)) & !is.na(get(part_max.column)),
                                paste(columns[["participant.age"]]) :=
                                    as.integer(rowMeans(.SD)),
                                .SDcols=c(part_min.column, part_max.column)]
        } else if (estimated.participant.age == "sample")
        {
            survey$participants[is.na(get(columns[["participant.age"]])) &
                                    !is.na(get(part_min.column)) & !is.na(get(part_max.column)) &
                                    get(part_min.column) <= get(part_max.column),
                                paste(columns[["participant.age"]]) :=
                                    as.integer(runif(.N,
                                                     get(part_min.column),
                                                     get(part_max.column)))]
        }
    }


    if (missing.participant.age == "remove" &&
        nrow(survey$participants[is.na(get(columns[["participant.age"]])) |
                                 get(columns[["participant.age"]]) <  min(age.limits)]) > 0)
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

    exact.column <- paste(columns[["contact.age"]], "exact", sep="_")
    min.column <- paste(columns[["contact.age"]], "est_min", sep="_")
    max.column <- paste(columns[["contact.age"]], "est_max", sep="_")

    ## set contact age if it's not in the data
    if (!(columns[["contact.age"]] %in% colnames(survey$contacts)))
    {
        survey$contacts[, paste(columns[["contact.age"]]) := NA_integer_]

        if (exact.column %in% colnames(survey$contacts))
        {
            survey$contacts[!is.na(get(exact.column)),
                            paste(columns[["contact.age"]]) := get(exact.column)]
        }
    }

    ## convert factors to integers
    for (age_column in
         c(columns[["contact.age"]], min.column, max.column, exact.column)) {
        if (age_column %in% colnames(survey$contacts) &&
            class(survey$contacts[[age_column]]) == "factor")
        {
            survey$contacts[, paste(age_column) :=
                                as.integer(levels(get(age_column)))[get(age_column)]]
        }
    }

    ## sample estimated contact ages
    if (min.column %in% colnames(survey$contacts) &&
        max.column %in% colnames(survey$contacts))
    {
        if (estimated.contact.age == "mean")
        {
            survey$contacts[is.na(get(columns[["contact.age"]])) &
                                !is.na(get(min.column)) & !is.na(get(max.column)),
                            paste(columns[["contact.age"]]) :=
                                as.integer(rowMeans(.SD)),
                            .SDcols=c(min.column, max.column)]
        } else if (estimated.contact.age == "sample")
        {
            survey$contacts[is.na(get(columns[["contact.age"]])) &
                                !is.na(get(min.column)) & !is.na(get(max.column)) &
                                get(min.column) <= get(max.column),
                            paste(columns[["contact.age"]]) :=
                                as.integer(runif(.N,
                                                 get(min.column),
                                                 get(max.column)))]
        }
    }

    if (missing.contact.age == "remove" &&
        nrow(survey$contacts[is.na(get(columns[["contact.age"]])) |
                             get(columns[["contact.age"]]) < min(age.limits)]) > 0)
    {
        if (!quiet && n == 1 && !missing.contact.age.set)
        {
            message("Removing participants that have contacts without age information. ",
                    "To change this behaviour, set the 'missing.contact.age' option")
        }
        missing.age.id <-
            survey$contacts[is.na(get(columns[["contact.age"]])) |
                                get(columns[["contact.age"]]) < min(age.limits),
                            get(columns[["id"]])]
        survey$participants <- survey$participants[!(get(columns[["id"]]) %in% missing.age.id)]
    }


    if (missing.contact.age == "ignore" &&
        nrow(survey$contacts[is.na(get(columns[["contact.age"]])) |
                             get(columns[["contact.age"]]) < min(age.limits)]) > 0)
    {
        if (!quiet && n == 1 && !missing.contact.age.set)
        {
            message("Ignore contacts without age information. ",
                    "To change this behaviour, set the 'missing.contact.age' option")
        }
        survey$contacts <- survey$contacts[!is.na(get(columns[["contact.age"]])) &
                                               get(columns[["contact.age"]]) >= min(age.limits),]
    }

    # adjust age.group.brakes to the lower and upper ages in the survey
    survey$participants[, lower.age.limit := reduce_agegroups(get(columns[["participant.age"]]),
                                                              age.limits[age.limits < max.age])]
    part.age.group.breaks  <- c(age.limits[age.limits < max.age], max.age)
    part.age.group.present <- age.limits[age.limits < max.age]
    survey$participants[, age.group :=
                            cut(survey$participants[, get(columns[["participant.age"]])],
                                breaks = part.age.group.breaks,
                                right = FALSE)]
    age.groups <- survey$participants[, levels(age.group)]
    age.groups[length(age.groups)] <-
        sub("\\[([0-9]+),.*$", "\\1+", age.groups[length(age.groups)])
    survey$participants[, age.group :=
                            factor(age.group, levels=levels(age.group), labels=age.groups)]

    ## add upper age limits
    lower.upper.age.limits <- data.table(lower.age.limit = part.age.group.present,
                                         upper.age.limit = part.age.group.breaks[-1])
    survey$participants <-
        merge(survey$participants, lower.upper.age.limits, by="lower.age.limit", all.x=TRUE)

    ## if split, symmetric or age weights are requested, get demographic data (survey population)
    need.survey.pop <- split || symmetric || weigh.age || 
        (!is.na(return.demography) && return.demography) || per.capita
    if (need.survey.pop)
    {
        ## check if survey population is either not given or given as a vector of countries
        if (missing(survey.pop) || is.character(survey.pop))
        {
            survey.representative <- FALSE
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
                    survey.representative <- TRUE
                }
            }

            if (!survey.representative) {
                ## get population data for countries from 'wpp' package
                country.pop <- data.table(wpp_age(survey.countries))

                # !! warning: spelling can differ between wpp_age and wpp_countries (e.g. Viet Nam vs Vietnam)
                # fix: rename countries using the same approach as in clean(survey,...)
                country.pop$country <- suppressWarnings(countrycode(country.pop$country, "country.name", "country.name"))

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
                    stop("Could not find population data for ",
                         paste(missing.countries, collapse = ", "), ". ",
                         " Use wpp_countries() to get a list of country names.")
                }

                ## get demographic data closest to survey year
                country.pop.year <- unique(country.pop[, year])
                survey.year <-
                    min(country.pop.year[which.min(abs(survey.year - country.pop.year))])
                survey.pop <-
                    country.pop[year == survey.year][, list(population = sum(population)),
                                                     by = "lower.age.limit"]
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
        } else{
            # if survey.pop is a data frame with columns 'lower.age.limit' and 'population'
            survey.pop <- data.table(survey.pop)
            # make sure the maximum survey.pop age exceeds the participant age group breaks
            if(max(survey.pop$lower.age.limit) < max(part.age.group.present)){
                survey.pop <- rbind(survey.pop,
                                    list(max(part.age.group.present+1),0))
            }
            
            # add dummy survey.year
            survey.year <- NA
        }
        
        # add upper.age.limit after sorting the survey.pop ages (and add maximum age > given ages)
        survey.pop <- survey.pop[order(lower.age.limit),]
        survey.pop$upper.age.limit <- unlist(c(survey.pop[-1,'lower.age.limit'],
                                               1+max(survey.pop$lower.age.limit,
                                                   part.age.group.present)))
        
        if (weigh.age) {
            ## keep reference of survey.pop
            survey.pop.full <-
                data.table(pop_age(survey.pop,
                                   seq(min(survey.pop$lower.age.limit),
                                       max(survey.pop$upper.age.limit)), ...))
        }

        ## adjust age groups by interpolating, in case they don't match between
        ## demographic and survey data
        survey.pop.max <- max(survey.pop$upper.age.limit)
        survey.pop <- data.table(pop_age(survey.pop, part.age.group.present, ...))

        ## set upper age limits
        survey.pop[, upper.age.limit := c(part.age.group.present[-1], survey.pop.max)]
    }

    ## weights
    survey$participants[, weight := 1]
    survey$contacts[, weight := 1]

    ## assign weights to participants, to account for weekend/weekday variation
    if (weigh.dayofweek) {
        found.dayofweek <- FALSE
        for (table in surveys)
        {
            if ("dayofweek" %in% colnames(survey[[table]]))
            {
                ## Add column sum_weight: Number of entries on weekdays / weekends
                survey[[table]][,  sum_weight := nrow(.SD),
                                by = (dayofweek %in% 1:5), ]

                ## The sum of the weights on weekdays is 5
                survey[[table]][dayofweek %in% 1:5, weight := 5/sum_weight]
                ## The sum of the weights on weekend is 2
                survey[[table]][!(dayofweek %in% 1:5), weight := 2/sum_weight]

                survey[[table]][,  sum_weight := NULL]
                found.dayofweek <- TRUE

                # add boolean for "weekday"
                survey[[table]][,  is.weekday := dayofweek %in% 1:5]
            }
        }
        if (!found.dayofweek)
        {
            warning("'weigh.dayofweek' is TRUE, but no 'dayofweek' column in the data. ",
                    "Will ignore.")
        }
    }

    ## assign weights to participants, to account for age variation
    if (weigh.age) {
        # for (table in surveys)
        for (table in surveys[1]) #TODO: option to change survey[[table]] into survey$participant
        {
            if ("age.group" %in% colnames(survey[[table]]))
            {
                # get number and proportion of participants by age
                survey[[table]][,age.count:= .N, by= eval(columns[["participant.age"]])]
                survey[[table]][,age.proportion:= age.count / .N]

                # get reference population by age (absolute and proportional)
                part.age.all      <- range(unique(survey[[table]][,get(columns[["participant.age"]])]))
                survey.pop.detail <- data.table(pop_age(survey.pop.full, seq(part.age.all[1],part.age.all[2]+1)))
                names(survey.pop.detail) <- c(columns[["participant.age"]],'population.count')
                survey.pop.detail[,population.proportion := population.count / sum(population.count)]

                # merge reference and survey population data
                survey[[table]] <- merge(survey[[table]],survey.pop.detail,by=eval(columns[["participant.age"]]))

                # calculate age-specific weights
                survey[[table]][, weight.age := population.proportion/age.proportion]

                # merge 'weight.age' into 'weight'
                survey[[table]][, weight := weight * weight.age]

                ## Remove the additional columns
                survey[[table]][,  age.count := NULL]
                survey[[table]][,  age.proportion := NULL]
                survey[[table]][,  population.count := NULL]
                survey[[table]][,  population.proportion := NULL]
                survey[[table]][,  weight.age := NULL]
            }
        }
    }

    ## further weigh if columns are specified
    if (length(weights) > 0) {
        for (i in 1:length(weights)) {
            for (table in surveys) {
                if (weights[i] %in% colnames(survey[[table]]))
                {
                    ## Number of entry per level of weight[i]
                    survey[[table]][,  sum_weight := nrow(.SD),
                                    by = get(weights[i])]
                    ## Compute the individual weight
                    survey[[table]][,  weight := weight * get(weights[i]) / sum_weight]
                    ## Remove the column "sum_weight"
                    survey[[table]][,  sum_weight := NULL]
                }
            }
        }
    }

    # post-stratification weight standardisation: by age.group
    survey$participants[, weight := weight / sum(weight) * .N,
                        by = age.group]
    survey$contacts[, weight := weight / sum(weight) * .N,
                    by = age.group]

    # option to truncate weights (if not NULL or NA)
    if(!is.null(weight.threshold) && !is.na(weight.threshold)){
        for (table in surveys) {
            survey[[table]][weight > weight.threshold, weight := weight.threshold]
            survey[[table]][, weight := weight / sum(weight) * .N,
                            by = age.group]
        }
    }

    ## merge participants and contacts into a single data table
    setkeyv(survey$participants, columns[["id"]])
    participant_ids <- unique(survey$participants[[columns[["id"]]]])

    survey$contacts <-
        merge(survey$contacts, survey$participants, by = columns[["id"]], all = F,
              allow.cartesian = T, suffixes=c(".cont", ".part"))
    survey$contacts[, weight := weight.cont * weight.part]

    setkeyv(survey$contacts, columns[["id"]])

    ## sample contacts
    if (missing.contact.age == "sample" &&
        nrow(survey$contacts[is.na(get(columns[["contact.age"]]))]) > 0)
    {
        for (this.age.group in
             unique(survey$contacts[is.na(get(columns[["contact.age"]])), age.group]))
        {
            ## first, deal with missing age
            if (nrow(survey$contacts[!is.na(get(columns[["contact.age"]])) &
                                     age.group == this.age.group]) > 0)
            {
                ## some contacts in the age group have an age, sample from these
                survey$contacts[is.na(get(columns[["contact.age"]])) &
                                    age.group == this.age.group,
                                paste(columns[["contact.age"]]) :=
                                    sample(survey$contacts[!is.na(get(columns[["contact.age"]])) &
                                                               age.group == this.age.group,
                                                           get(columns[["contact.age"]])],
                                           size = .N,
                                           replace = TRUE)]
            } else {
                ## no contacts in the age group have an age, sample uniformly between limits
                min.contact.age <-
                    survey$contacts[, min(get(columns[["contact.age"]]), na.rm=TRUE)]
                max.contact.age <-
                    survey$contacts[, max(get(columns[["contact.age"]]), na.rm=TRUE)]
                survey$contacts[is.na(get(columns[["contact.age"]])) &
                                    age.group == this.age.group,
                                paste(columns[["contact.age"]]) :=
                                    as.integer(floor(runif(.N, min = min.contact.age,
                                                           max = max.contact.age + 1)))]
            }
        }
    }

    ## set contact age groups
    max.contact.age <-
        survey$contacts[, max(get(columns[["contact.age"]]), na.rm = TRUE) + 1]

    contact.age.group.breaks <- part.age.group.breaks
    if (max.contact.age > max(contact.age.group.breaks)) {
        contact.age.group.breaks[length(contact.age.group.breaks)] <- max.contact.age
    }
    survey$contacts[, contact.age.group :=
                        cut(get(columns[["contact.age"]]),
                            breaks = contact.age.group.breaks,
                            labels = age.groups,
                            right = FALSE)]

    ## Bootstrap
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
            good.sample <- FALSE
            while (!good.sample) {
                ## take a bootstrap sample from the participants
                part.sample <- sample(participant_ids, replace=T)
                part.age.limits <-
                    unique(survey$participants[get(columns[["id"]]) %in% part.sample,
                                               lower.age.limit])
                good.sample <- !sample.all.age.groups ||
                    (length(setdiff(age.limits, part.age.limits)) == 0)

                sample.table <-
                    data.table(id=part.sample, weight=1)
                sample.table <-
                    sample.table[, list(bootstrap.weight=sum(weight)), by=id]
                setnames(sample.table, "id", columns[["id"]])
                setkeyv(sample.table, columns[["id"]])

                sampled.contacts <- merge(survey$contacts, sample.table)
                sampled.contacts[, sampled.weight := weight * bootstrap.weight]

                sampled.participants <-
                    merge(survey$participants, sample.table)
                sampled.participants[, sampled.weight := weight * bootstrap.weight]
            }
        } else
        {
            ## just use all participants
            sampled.contacts <- survey$contacts
            sampled.contacts[, sampled.weight := weight]
            sampled.participants <- survey$participants
            sampled.participants[, sampled.weight := weight]
        }

        ## calculate weighted contact matrix
        weighted.matrix <-
            xtabs(data = sampled.contacts,
                  formula = sampled.weight ~ age.group + contact.age.group,
                  addNA = TRUE)

        dims <- dim(weighted.matrix)
        dim.names <- dimnames(weighted.matrix)

        if (!counts) { ## normalise to give mean number of contacts
            ## calculate normalisation vector
            norm.vector <-
                xtabs(data = sampled.participants,
                      formula = sampled.weight ~ age.group, addNA = TRUE)

            ## normalise contact matrix
            weighted.matrix <-
                array(apply(weighted.matrix, 2, function(x) x/norm.vector), dim=dims,
                      dimnames=dim.names)
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

                nb.contacts <- apply(weighted.matrix, 1, sum)
                mean.contacts <- sum(survey.pop$population*nb.contacts)/
                    sum(survey.pop$population)
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

        ret[[i]][["matrix"]] <- weighted.matrix
        
        # option to add matrix per capita, i.e. the contact rate of age i with one individual of age j in the population.
        if(per.capita){
            if (counts) {
                warning("'per.capita=TRUE' does not make sense with 'counts=TRUE'; ",
                        "will not return the contact matrix per capita.")
            } else if (split) {
                warning("'per.capita=TRUE' does not make sense with 'split=TRUE'; ",
                        "will not return the contact matrix per capita.")
            } else {
            survey.pop$population
            weighted.matrix.per.capita <- weighted.matrix / matrix(rep(survey.pop$population,nrow(survey.pop)),ncol=nrow(survey.pop),byrow = TRUE)
            weighted.matrix.per.capita
            ret[[i]][["matrix.per.capita"]] <- weighted.matrix.per.capita
            }
        }
        
    }

    if (exists("survey.year")) {
        survey.pop[, year := survey.year]
        survey.pop <-
            merge(survey.pop,
                  unique(survey$participants[, list(lower.age.limit, age.group)]))
        survey.pop <- survey.pop[, list(age.group, population,
                                        proportion=population/sum(population), year)]
    }

    ## get number of participants in each age group
    if (any(is.na(survey$participants$age.group))) {
        useNA <- "always"
    } else {
        useNA <- "no"
    }

    part.pop <- data.table(table(survey$participants[, age.group], useNA = useNA))
    setnames(part.pop, c("age.group", "participants"))
    part.pop[, proportion := participants / sum(participants)]

    # set function output
    if (length(ret) > 1) {
        return_value <- list(matrices = ret)
    } else {
        return_value <- ret[[1]]
    }

    if (!is.null(return_value)) {
        if (need.survey.pop && (is.na(return.demography) || return.demography)) {
            # change survey.pop$age.group factors into characters (cfr. part.pop)
            survey.pop[,age.group:= as.character(age.group)]
            return_value[["demography"]] <- survey.pop[]
            }
        return_value[["participants"]] <- part.pop[]
    }

    # option to return participant weights
    if(return.part.weights){

        # default
        part.weights <- survey$participants[, .N, by = list(age.group, weight)]
        part.weights <- part.weights[order(age.group,weight),]

        # add age and/or dayofweek info
        if(weigh.age && weigh.dayofweek){
            part.weights <- survey$participants[, .N, by = list(age.group,participant.age=get(columns[["participant.age"]]),is.weekday,weight)]
        } else if(weigh.age){
            part.weights <- survey$participants[, .N, by = list(age.group,participant.age=get(columns[["participant.age"]]),weight)]
        } else if(weigh.dayofweek){
            part.weights <- survey$participants[, .N, by = list(age.group,is.weekday,weight)]
        }

        # order (from left to right)
        part.weights <- part.weights[order(part.weights),]

        # set name of last column
        names(part.weights)[ncol(part.weights)] <- "participants"

        # add proportion and add to return_value
        part.weights[, proportion := participants / sum(participants)]
        return_value[["participants.weights"]] <- part.weights[]
    }

    return(return_value)
}
