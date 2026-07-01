# Weigh survey participants

`weigh()` multiplies participant weights by values looked up from a
`target`. The existing `weight` column is multiplied in place, so
multiple calls compose; if no `weight` column is present, one is created
with value 1.

`weigh_by_dayofweek()` and `weigh_by_age()` are thin convenience
wrappers around the two most common recipes — the weekly weekday/weekend
split and age post-stratification against a reference population. See
the dedicated sections below for what they compute exactly.

## Usage

``` r
weigh(survey, by, target = NULL, groups = NULL, ...)

weigh_by_dayofweek(survey)

weigh_by_age(survey, pop, ...)
```

## Arguments

- survey:

  a [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object

- by:

  column name in the participant data to join on

- target:

  see *Target shapes accepted by `weigh()`*.

- groups:

  a list of value sets mapping column values to groups (used with an
  unnamed numeric `target` vector); must be the same length as `target`.

- ...:

  further arguments passed on for age interpolation.

- pop:

  a data frame with columns `lower.age.limit` and `population` (used by
  `weigh_by_age()`).

## Value

the survey object with updated participant weights

## Target shapes accepted by `weigh()`

- `target = NULL` (the default) — multiply the numeric column `by`
  directly into `weight`. Useful when participants already carry a
  precomputed weight column.

- a two-column data frame whose key column is named `by` — pure discrete
  join: multiply the value column into `weight` where the key matches.
  Unmatched values get `NA` (with a warning).

- an unnamed numeric vector together with `groups` — each element of
  `target` is the *total* weight assigned across participants matching
  the corresponding entry in `groups`. The per-participant factor is
  `target[g] / n_in_group`.

- a named numeric vector — same as above but `names(target)` are matched
  against values of the `by` column.

A data frame target that does *not* have a column named `by` but does
have `lower.age.limit` and `population` triggers a deprecation warning
and falls back to the old hidden age post-stratification path; use
`weigh_by_age()` instead.

## `weigh_by_dayofweek()`

Rescales weights so that weekday participants together carry a total
weight of 5 and weekend participants a total weight of 2 — the weekly
5/2 split that corrects for the typical over-representation of weekdays
in diary surveys. Concretely, each weekday participant gets
`5 / n_weekday` and each weekend participant `2 / n_weekend`;
participants with `NA` day-of-week get the neutral average `7 / N`. The
`dayofweek` column is taken to use 0 = Sunday through 6 = Saturday (the
POLYMOD convention).

Equivalent to:
`weigh(survey, "dayofweek", target = c(5, 2), groups = list(1:5, c(0, 6)))`

## `weigh_by_age()`

Convenience wrapper for age post-stratification. The main thing it adds
over a raw `weigh()` call is **interpolation**: the reference `pop` is
expanded to single-year ages internally, so it can be supplied at any
age resolution (e.g. 5-year bands).

For each single-year age \\a\\ the weight then becomes

\$\$w_a = \frac{P_a / P}{N_a / N},\$\$

where \\P_a\\ is the target population at age \\a\\, \\P\\ the total,
and \\N_a\\, \\N\\ the corresponding sample counts.

`survey` must already have been processed by
[`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md)
so that a `part_age` column is available for the join.

## Examples

``` r
data(polymod)
uk <- polymod[country == "United Kingdom"] |>
  assign_age_groups(age_limits = c(0, 5, 15))

# ── target = NULL ────────────────────────────────────────────────
# Multiply an existing numeric column directly into the weight:
uk |> weigh("hh_size")
#> $participants
#> Key: <lower.age.limit>
#>       lower.age.limit      hh_id part_id part_gender part_occupation
#>                 <num>     <char>   <int>      <char>           <int>
#>    1:               0 Mo08HH4520    4520           F               5
#>    2:               0 Mo08HH4521    4521           M               5
#>    3:               0 Mo08HH4522    4522           M               5
#>    4:               0 Mo08HH4525    4525           F               5
#>    5:               0 Mo08HH4526    4526           M               6
#>   ---                                                               
#> 1007:              15 Mo08HH5518    5518           M              NA
#> 1008:              15 Mo08HH5519    5519           F              NA
#> 1009:              15 Mo08HH5520    5520           F              NA
#> 1010:              15 Mo08HH5521    5521           F               4
#> 1011:              15 Mo08HH5522    5522           M              NA
#>       part_occupation_detail part_education part_education_length
#>                        <int>          <int>                 <int>
#>    1:                      4              4                    13
#>    2:                      3              4                    13
#>    3:                      1              4                    13
#>    4:                     NA              4                    13
#>    5:                     NA              4                    13
#>   ---                                                            
#> 1007:                     NA              4                    13
#> 1008:                     NA              4                    13
#> 1009:                     NA              4                    13
#> 1010:                     NA              5                    16
#> 1011:                     NA              4                    13
#>       participant_school_year participant_nationality child_care
#>                         <int>                  <char>     <char>
#>    1:                      NA                      UK          Y
#>    2:                      NA                      UK          Y
#>    3:                      NA                      UK          Y
#>    4:                      NA                      UK          Y
#>    5:                      NA                      UK          N
#>   ---                                                           
#> 1007:                      NA                      OT           
#> 1008:                      NA                      OT           
#> 1009:                      NA                      OT           
#> 1010:                      NA                      UK           
#> 1011:                      NA                      OT           
#>       child_care_detail child_relationship child_nationality problems diary_how
#>                   <int>              <int>            <char>   <char>     <int>
#>    1:                NA                  1                UK                 NA
#>    2:                NA                  2                UK                 NA
#>    3:                NA                  1                UK                 NA
#>    4:                NA                  1                UK                 NA
#>    5:                NA                  1                UK                 NA
#>   ---                                                                          
#> 1007:                NA                 NA                                   NA
#> 1008:                NA                 NA                                   NA
#> 1009:                NA                 NA                                   NA
#> 1010:                NA                 NA                                   NA
#> 1011:                NA                 NA                                   NA
#>       diary_missed_unsp diary_missed_skin diary_missed_noskin  sday_id  type
#>                   <int>             <int>               <int>    <int> <int>
#>    1:                 1                 1                   1 20060422     3
#>    2:                 1                 1                   1 20060422     3
#>    3:                 1                 1                   1 20060422     3
#>    4:                 1                 1                   1 20060421     3
#>    5:                 1                 1                   1 20060421     3
#>   ---                                                                       
#> 1007:                 1                 1                   1 20060512     1
#> 1008:                 1                 1                   1 20060512     1
#> 1009:                 1                 1                   1 20060512     1
#> 1010:                 1                 1                   1 20060512     1
#> 1011:                 1                 1                   1 20060512     1
#>         day month  year dayofweek hh_age_1 hh_age_2 hh_age_3 hh_age_4 hh_age_5
#>       <int> <int> <int>     <int>    <int>    <int>    <int>    <int>    <int>
#>    1:    22     4  2006         6        3       33       NA       NA       NA
#>    2:    22     4  2006         6        1        2       25       30       NA
#>    3:    22     4  2006         6        4       31       31       NA       NA
#>    4:    21     4  2006         5        4        7       33       NA       NA
#>    5:    21     4  2006         5        2       18       46       48       NA
#>   ---                                                                         
#> 1007:    12     5  2006         5       43       50       NA       NA       NA
#> 1008:    12     5  2006         5       57       64       NA       NA       NA
#> 1009:    12     5  2006         5       19       27       52       55       NA
#> 1010:    12     5  2006         5        0        3       34       40       NA
#> 1011:    12     5  2006         5       35       39       NA       NA       NA
#>       hh_age_6 hh_age_7 hh_age_8 hh_age_9 hh_age_10 hh_age_11 hh_age_12
#>          <int>    <int>    <int>    <int>     <int>     <int>     <int>
#>    1:       NA       NA       NA       NA        NA        NA        NA
#>    2:       NA       NA       NA       NA        NA        NA        NA
#>    3:       NA       NA       NA       NA        NA        NA        NA
#>    4:       NA       NA       NA       NA        NA        NA        NA
#>    5:       NA       NA       NA       NA        NA        NA        NA
#>   ---                                                                  
#> 1007:       NA       NA       NA       NA        NA        NA        NA
#> 1008:       NA       NA       NA       NA        NA        NA        NA
#> 1009:       NA       NA       NA       NA        NA        NA        NA
#> 1010:       NA       NA       NA       NA        NA        NA        NA
#> 1011:       NA       NA       NA       NA        NA        NA        NA
#>       hh_age_13 hh_age_14 hh_age_15 hh_age_16 hh_age_17 hh_age_18 hh_age_19
#>           <int>     <int>     <int>    <lgcl>    <lgcl>    <lgcl>    <lgcl>
#>    1:        NA        NA        NA        NA        NA        NA        NA
#>    2:        NA        NA        NA        NA        NA        NA        NA
#>    3:        NA        NA        NA        NA        NA        NA        NA
#>    4:        NA        NA        NA        NA        NA        NA        NA
#>    5:        NA        NA        NA        NA        NA        NA        NA
#>   ---                                                                      
#> 1007:        NA        NA        NA        NA        NA        NA        NA
#> 1008:        NA        NA        NA        NA        NA        NA        NA
#> 1009:        NA        NA        NA        NA        NA        NA        NA
#> 1010:        NA        NA        NA        NA        NA        NA        NA
#> 1011:        NA        NA        NA        NA        NA        NA        NA
#>       hh_age_20 class_size        country hh_size part_age_exact part_age
#>          <lgcl>      <int>         <fctr>   <int>          <int>    <int>
#>    1:        NA         NA United Kingdom       2              3        3
#>    2:        NA         NA United Kingdom       4              2        2
#>    3:        NA         NA United Kingdom       3              4        4
#>    4:        NA         NA United Kingdom       3              4        4
#>    5:        NA         NA United Kingdom       4              2        2
#>   ---                                                                    
#> 1007:        NA         NA United Kingdom       2             50       50
#> 1008:        NA         NA United Kingdom       2             57       57
#> 1009:        NA         NA United Kingdom       4             52       52
#> 1010:        NA         NA United Kingdom       4             34       34
#> 1011:        NA         NA United Kingdom       2             39       39
#>       age.group upper.age.limit weight
#>          <fctr>           <num>  <num>
#>    1:     [0,5)               5      2
#>    2:     [0,5)               5      4
#>    3:     [0,5)               5      3
#>    4:     [0,5)               5      3
#>    5:     [0,5)               5      4
#>   ---                                 
#> 1007:  [15,Inf)              80      2
#> 1008:  [15,Inf)              80      2
#> 1009:  [15,Inf)              80      4
#> 1010:  [15,Inf)              80      4
#> 1011:  [15,Inf)              80      2
#> 
#> $contacts
#>        cont_id part_id cnt_age_exact cnt_age_est_min cnt_age_est_max cnt_gender
#>          <int>   <int>         <int>           <int>           <int>     <char>
#>     1:   66023    4517             4              NA              NA          M
#>     2:   66024    4517            40              NA              NA          F
#>     3:   66025    4517            31              NA              NA          F
#>     4:   66026    4517            NA              50              55          F
#>     5:   66027    4517            29              NA              NA          M
#>    ---                                                                         
#> 11872:   77894    5522            NA              10              20          F
#> 11873:   77895    5522            35              NA              NA          F
#> 11874:   77896    5522            50              NA              NA          M
#> 11875:   77897    5522            NA              30              40          M
#> 11876:   77898    5522            NA              40              50          M
#>        cnt_home cnt_work cnt_school cnt_transport cnt_leisure cnt_otherplace
#>           <int>    <int>      <int>         <int>       <int>          <int>
#>     1:        0        0          1             0           0              0
#>     2:        0        0          0             0           0              1
#>     3:        1        0          0             0           0              0
#>     4:        0        0          0             0           0              1
#>     5:        1        0          0             0           0              0
#>    ---                                                                      
#> 11872:        1        0          0             0           0              0
#> 11873:        1        0          0             0           0              0
#> 11874:        0        1          0             0           0              0
#> 11875:        0        1          0             0           0              0
#> 11876:        0        1          0             0           0              0
#>        frequency_multi phys_contact duration_multi cnt_age contact.age.group
#>                  <int>        <int>          <int>   <int>            <fctr>
#>     1:               1            1              4       4             [0,5)
#>     2:               2            2              2      40          [15,Inf)
#>     3:               1            1              5      31          [15,Inf)
#>     4:               2            2              2      52          [15,Inf)
#>     5:               1            1              4      29          [15,Inf)
#>    ---                                                                      
#> 11872:               1            2              1      15          [15,Inf)
#> 11873:               1            1              5      35          [15,Inf)
#> 11874:               2            1              3      50          [15,Inf)
#> 11875:               2            2              3      35          [15,Inf)
#> 11876:               1            1              4      45          [15,Inf)
#> 
#> $reference
#> $reference$title
#> [1] "POLYMOD social contact data"
#> 
#> $reference$bibtype
#> [1] "Misc"
#> 
#> $reference$author
#>  [1] "Joël Mossong"               "Niel Hens"                 
#>  [3] "Mark Jit"                   "Philippe Beutels"          
#>  [5] "Kari Auranen"               "Rafael Mikolajczyk"        
#>  [7] "Marco Massari"              "Stefania Salmaso"          
#>  [9] "Gianpaolo Scalia Tomba"     "Jacco Wallinga"            
#> [11] "Janneke Heijne"             "Malgorzata Sadkowska-Todys"
#> [13] "Magdalena Rosinska"         "W. John Edmunds"           
#> 
#> $reference$year
#> [1] 2017
#> 
#> $reference$note
#> [1] "Version 1.1"
#> 
#> $reference$doi
#> [1] "10.5281/zenodo.1157934"
#> 
#> 
#> attr(,"class")
#> [1] "contact_survey"

# ── data-frame target (discrete join) ────────────────────────────
# The key column of `target` must match `by`. Each participant
# has its weight multiplied by the matching value column.
age_target <- data.frame(
  age.group = c("[0,5)", "[5,15)", "[15,Inf)"),
  p = c(0.06, 0.12, 0.82)
)
uk |> weigh("age.group", target = age_target)
#> $participants
#> Key: <lower.age.limit>
#>       lower.age.limit      hh_id part_id part_gender part_occupation
#>                 <num>     <char>   <int>      <char>           <int>
#>    1:               0 Mo08HH4520    4520           F               5
#>    2:               0 Mo08HH4521    4521           M               5
#>    3:               0 Mo08HH4522    4522           M               5
#>    4:               0 Mo08HH4525    4525           F               5
#>    5:               0 Mo08HH4526    4526           M               6
#>   ---                                                               
#> 1007:              15 Mo08HH5518    5518           M              NA
#> 1008:              15 Mo08HH5519    5519           F              NA
#> 1009:              15 Mo08HH5520    5520           F              NA
#> 1010:              15 Mo08HH5521    5521           F               4
#> 1011:              15 Mo08HH5522    5522           M              NA
#>       part_occupation_detail part_education part_education_length
#>                        <int>          <int>                 <int>
#>    1:                      4              4                    13
#>    2:                      3              4                    13
#>    3:                      1              4                    13
#>    4:                     NA              4                    13
#>    5:                     NA              4                    13
#>   ---                                                            
#> 1007:                     NA              4                    13
#> 1008:                     NA              4                    13
#> 1009:                     NA              4                    13
#> 1010:                     NA              5                    16
#> 1011:                     NA              4                    13
#>       participant_school_year participant_nationality child_care
#>                         <int>                  <char>     <char>
#>    1:                      NA                      UK          Y
#>    2:                      NA                      UK          Y
#>    3:                      NA                      UK          Y
#>    4:                      NA                      UK          Y
#>    5:                      NA                      UK          N
#>   ---                                                           
#> 1007:                      NA                      OT           
#> 1008:                      NA                      OT           
#> 1009:                      NA                      OT           
#> 1010:                      NA                      UK           
#> 1011:                      NA                      OT           
#>       child_care_detail child_relationship child_nationality problems diary_how
#>                   <int>              <int>            <char>   <char>     <int>
#>    1:                NA                  1                UK                 NA
#>    2:                NA                  2                UK                 NA
#>    3:                NA                  1                UK                 NA
#>    4:                NA                  1                UK                 NA
#>    5:                NA                  1                UK                 NA
#>   ---                                                                          
#> 1007:                NA                 NA                                   NA
#> 1008:                NA                 NA                                   NA
#> 1009:                NA                 NA                                   NA
#> 1010:                NA                 NA                                   NA
#> 1011:                NA                 NA                                   NA
#>       diary_missed_unsp diary_missed_skin diary_missed_noskin  sday_id  type
#>                   <int>             <int>               <int>    <int> <int>
#>    1:                 1                 1                   1 20060422     3
#>    2:                 1                 1                   1 20060422     3
#>    3:                 1                 1                   1 20060422     3
#>    4:                 1                 1                   1 20060421     3
#>    5:                 1                 1                   1 20060421     3
#>   ---                                                                       
#> 1007:                 1                 1                   1 20060512     1
#> 1008:                 1                 1                   1 20060512     1
#> 1009:                 1                 1                   1 20060512     1
#> 1010:                 1                 1                   1 20060512     1
#> 1011:                 1                 1                   1 20060512     1
#>         day month  year dayofweek hh_age_1 hh_age_2 hh_age_3 hh_age_4 hh_age_5
#>       <int> <int> <int>     <int>    <int>    <int>    <int>    <int>    <int>
#>    1:    22     4  2006         6        3       33       NA       NA       NA
#>    2:    22     4  2006         6        1        2       25       30       NA
#>    3:    22     4  2006         6        4       31       31       NA       NA
#>    4:    21     4  2006         5        4        7       33       NA       NA
#>    5:    21     4  2006         5        2       18       46       48       NA
#>   ---                                                                         
#> 1007:    12     5  2006         5       43       50       NA       NA       NA
#> 1008:    12     5  2006         5       57       64       NA       NA       NA
#> 1009:    12     5  2006         5       19       27       52       55       NA
#> 1010:    12     5  2006         5        0        3       34       40       NA
#> 1011:    12     5  2006         5       35       39       NA       NA       NA
#>       hh_age_6 hh_age_7 hh_age_8 hh_age_9 hh_age_10 hh_age_11 hh_age_12
#>          <int>    <int>    <int>    <int>     <int>     <int>     <int>
#>    1:       NA       NA       NA       NA        NA        NA        NA
#>    2:       NA       NA       NA       NA        NA        NA        NA
#>    3:       NA       NA       NA       NA        NA        NA        NA
#>    4:       NA       NA       NA       NA        NA        NA        NA
#>    5:       NA       NA       NA       NA        NA        NA        NA
#>   ---                                                                  
#> 1007:       NA       NA       NA       NA        NA        NA        NA
#> 1008:       NA       NA       NA       NA        NA        NA        NA
#> 1009:       NA       NA       NA       NA        NA        NA        NA
#> 1010:       NA       NA       NA       NA        NA        NA        NA
#> 1011:       NA       NA       NA       NA        NA        NA        NA
#>       hh_age_13 hh_age_14 hh_age_15 hh_age_16 hh_age_17 hh_age_18 hh_age_19
#>           <int>     <int>     <int>    <lgcl>    <lgcl>    <lgcl>    <lgcl>
#>    1:        NA        NA        NA        NA        NA        NA        NA
#>    2:        NA        NA        NA        NA        NA        NA        NA
#>    3:        NA        NA        NA        NA        NA        NA        NA
#>    4:        NA        NA        NA        NA        NA        NA        NA
#>    5:        NA        NA        NA        NA        NA        NA        NA
#>   ---                                                                      
#> 1007:        NA        NA        NA        NA        NA        NA        NA
#> 1008:        NA        NA        NA        NA        NA        NA        NA
#> 1009:        NA        NA        NA        NA        NA        NA        NA
#> 1010:        NA        NA        NA        NA        NA        NA        NA
#> 1011:        NA        NA        NA        NA        NA        NA        NA
#>       hh_age_20 class_size        country hh_size part_age_exact part_age
#>          <lgcl>      <int>         <fctr>   <int>          <int>    <int>
#>    1:        NA         NA United Kingdom       2              3        3
#>    2:        NA         NA United Kingdom       4              2        2
#>    3:        NA         NA United Kingdom       3              4        4
#>    4:        NA         NA United Kingdom       3              4        4
#>    5:        NA         NA United Kingdom       4              2        2
#>   ---                                                                    
#> 1007:        NA         NA United Kingdom       2             50       50
#> 1008:        NA         NA United Kingdom       2             57       57
#> 1009:        NA         NA United Kingdom       4             52       52
#> 1010:        NA         NA United Kingdom       4             34       34
#> 1011:        NA         NA United Kingdom       2             39       39
#>       age.group upper.age.limit weight
#>          <fctr>           <num>  <num>
#>    1:     [0,5)               5   0.06
#>    2:     [0,5)               5   0.06
#>    3:     [0,5)               5   0.06
#>    4:     [0,5)               5   0.06
#>    5:     [0,5)               5   0.06
#>   ---                                 
#> 1007:  [15,Inf)              80   0.82
#> 1008:  [15,Inf)              80   0.82
#> 1009:  [15,Inf)              80   0.82
#> 1010:  [15,Inf)              80   0.82
#> 1011:  [15,Inf)              80   0.82
#> 
#> $contacts
#>        cont_id part_id cnt_age_exact cnt_age_est_min cnt_age_est_max cnt_gender
#>          <int>   <int>         <int>           <int>           <int>     <char>
#>     1:   66023    4517             4              NA              NA          M
#>     2:   66024    4517            40              NA              NA          F
#>     3:   66025    4517            31              NA              NA          F
#>     4:   66026    4517            NA              50              55          F
#>     5:   66027    4517            29              NA              NA          M
#>    ---                                                                         
#> 11872:   77894    5522            NA              10              20          F
#> 11873:   77895    5522            35              NA              NA          F
#> 11874:   77896    5522            50              NA              NA          M
#> 11875:   77897    5522            NA              30              40          M
#> 11876:   77898    5522            NA              40              50          M
#>        cnt_home cnt_work cnt_school cnt_transport cnt_leisure cnt_otherplace
#>           <int>    <int>      <int>         <int>       <int>          <int>
#>     1:        0        0          1             0           0              0
#>     2:        0        0          0             0           0              1
#>     3:        1        0          0             0           0              0
#>     4:        0        0          0             0           0              1
#>     5:        1        0          0             0           0              0
#>    ---                                                                      
#> 11872:        1        0          0             0           0              0
#> 11873:        1        0          0             0           0              0
#> 11874:        0        1          0             0           0              0
#> 11875:        0        1          0             0           0              0
#> 11876:        0        1          0             0           0              0
#>        frequency_multi phys_contact duration_multi cnt_age contact.age.group
#>                  <int>        <int>          <int>   <int>            <fctr>
#>     1:               1            1              4       4             [0,5)
#>     2:               2            2              2      40          [15,Inf)
#>     3:               1            1              5      31          [15,Inf)
#>     4:               2            2              2      52          [15,Inf)
#>     5:               1            1              4      29          [15,Inf)
#>    ---                                                                      
#> 11872:               1            2              1      15          [15,Inf)
#> 11873:               1            1              5      35          [15,Inf)
#> 11874:               2            1              3      50          [15,Inf)
#> 11875:               2            2              3      35          [15,Inf)
#> 11876:               1            1              4      45          [15,Inf)
#> 
#> $reference
#> $reference$title
#> [1] "POLYMOD social contact data"
#> 
#> $reference$bibtype
#> [1] "Misc"
#> 
#> $reference$author
#>  [1] "Joël Mossong"               "Niel Hens"                 
#>  [3] "Mark Jit"                   "Philippe Beutels"          
#>  [5] "Kari Auranen"               "Rafael Mikolajczyk"        
#>  [7] "Marco Massari"              "Stefania Salmaso"          
#>  [9] "Gianpaolo Scalia Tomba"     "Jacco Wallinga"            
#> [11] "Janneke Heijne"             "Malgorzata Sadkowska-Todys"
#> [13] "Magdalena Rosinska"         "W. John Edmunds"           
#> 
#> $reference$year
#> [1] 2017
#> 
#> $reference$note
#> [1] "Version 1.1"
#> 
#> $reference$doi
#> [1] "10.5281/zenodo.1157934"
#> 
#> 
#> attr(,"class")
#> [1] "contact_survey"

# Same idea, joining on `country` to pool participants across studies
# by a target population share:
country_target <- data.frame(
  country = c("United Kingdom", "Germany", "Italy"),
  p = c(0.3, 0.4, 0.3)
)
polymod |>
  assign_age_groups(age_limits = c(0, 5, 15)) |>
  weigh("country", target = country_target)
#> Warning: 5 values in column "country" not found in `target` ("Luxembourg",
#> "Netherlands", "Poland", "Finland", and "Belgium"); their weights will be set
#> to "NA".
#> $participants
#> Key: <lower.age.limit>
#>       lower.age.limit      hh_id part_id part_gender part_occupation
#>                 <num>     <char>   <int>      <char>           <int>
#>    1:               0   Mo08HH10      10           F               6
#>    2:               0  Mo08HH101     101           F               5
#>    3:               0  Mo08HH102     102           M               6
#>    4:               0  Mo08HH103     103           M               5
#>    5:               0 Mo08HH1053    1053           F               5
#>   ---                                                               
#> 7194:              15  Mo08HH869     869           F               3
#> 7195:              15  Mo08HH870     870           F               1
#> 7196:              15  Mo08HH871     871           F               5
#> 7197:              15  Mo08HH872     872           F               1
#> 7198:              15  Mo08HH873     873           F               5
#>       part_occupation_detail part_education part_education_length
#>                        <int>          <int>                 <int>
#>    1:                      7              5                    16
#>    2:                      7              3                    11
#>    3:                     NA              6                    18
#>    4:                     NA              3                    13
#>    5:                     NA              1                    10
#>   ---                                                            
#> 7194:                      6              8                    10
#> 7195:                      8              2                    12
#> 7196:                     NA              8                    10
#> 7197:                      7              8                    10
#> 7198:                     NA              8                    10
#>       participant_school_year participant_nationality child_care
#>                         <int>                  <char>     <char>
#>    1:                      NA                      IT          N
#>    2:                      NA                      IT          Y
#>    3:                      NA                      IT          N
#>    4:                      NA                      IT          Y
#>    5:                      NA                                  Y
#>   ---                                                           
#> 7194:                      NA                                  Y
#> 7195:                      NA                                  N
#> 7196:                      NA                                  Y
#> 7197:                      NA                                  Y
#> 7198:                      NA                                  Y
#>       child_care_detail child_relationship child_nationality problems diary_how
#>                   <int>              <int>            <char>   <char>     <int>
#>    1:                NA                  1                IT        N         1
#>    2:                NA                  2                IT        N         2
#>    3:                NA                  1                IT                 NA
#>    4:                NA                  1                IT        N         2
#>    5:                NA                  3                                   NA
#>   ---                                                                          
#> 7194:                NA                 NA                                    1
#> 7195:                NA                  2                                    1
#> 7196:                NA                 NA                                    1
#> 7197:                NA                 NA                                    1
#> 7198:                NA                  1                                   NA
#>       diary_missed_unsp diary_missed_skin diary_missed_noskin  sday_id  type
#>                   <int>             <int>               <int>    <int> <int>
#>    1:                 1                NA                  NA 20060525     3
#>    2:                 2                NA                  NA 20060525     3
#>    3:                NA                NA                  NA 20060526     3
#>    4:                 3                NA                  NA 20060526     3
#>    5:                NA                NA                  NA 20060116     3
#>   ---                                                                       
#> 7194:                NA                NA                  NA 20060118     3
#> 7195:                NA                NA                  NA 20060117     3
#> 7196:                NA                NA                  NA 20060707     3
#> 7197:                NA                NA                  NA 20060522     3
#> 7198:                NA                NA                  NA 20060529     3
#>         day month  year dayofweek hh_age_1 hh_age_2 hh_age_3 hh_age_4 hh_age_5
#>       <int> <int> <int>     <int>    <int>    <int>    <int>    <int>    <int>
#>    1:    25     5  2006         4        0        3       40       NA       NA
#>    2:    25     5  2006         4        4       12       17       43       NA
#>    3:    26     5  2006         5        1        3       44       NA       NA
#>    4:    26     5  2006         5        2        3       33       NA       NA
#>    5:    16     1  2006         1        4       14       18       40       42
#>   ---                                                                         
#> 7194:    18     1  2006         3       15       30       32       NA       NA
#> 7195:    17     1  2006         2       15       37       37       NA       NA
#> 7196:     7     7  2006         5       15       19       46       48       NA
#> 7197:    22     5  2006         1       15       36       38       NA       NA
#> 7198:    29     5  2006         1       15       24       47       60       NA
#>       hh_age_6 hh_age_7 hh_age_8 hh_age_9 hh_age_10 hh_age_11 hh_age_12
#>          <int>    <int>    <int>    <int>     <int>     <int>     <int>
#>    1:       NA       NA       NA       NA        NA        NA        NA
#>    2:       NA       NA       NA       NA        NA        NA        NA
#>    3:       NA       NA       NA       NA        NA        NA        NA
#>    4:       NA       NA       NA       NA        NA        NA        NA
#>    5:       NA       NA       NA       NA        NA        NA        NA
#>   ---                                                                  
#> 7194:       NA       NA       NA       NA        NA        NA        NA
#> 7195:       NA       NA       NA       NA        NA        NA        NA
#> 7196:       NA       NA       NA       NA        NA        NA        NA
#> 7197:       NA       NA       NA       NA        NA        NA        NA
#> 7198:       NA       NA       NA       NA        NA        NA        NA
#>       hh_age_13 hh_age_14 hh_age_15 hh_age_16 hh_age_17 hh_age_18 hh_age_19
#>           <int>     <int>     <int>    <lgcl>    <lgcl>    <lgcl>    <lgcl>
#>    1:        NA        NA        NA        NA        NA        NA        NA
#>    2:        NA        NA        NA        NA        NA        NA        NA
#>    3:        NA        NA        NA        NA        NA        NA        NA
#>    4:        NA        NA        NA        NA        NA        NA        NA
#>    5:        NA        NA        NA        NA        NA        NA        NA
#>   ---                                                                      
#> 7194:        NA        NA        NA        NA        NA        NA        NA
#> 7195:        NA        NA        NA        NA        NA        NA        NA
#> 7196:        NA        NA        NA        NA        NA        NA        NA
#> 7197:        NA        NA        NA        NA        NA        NA        NA
#> 7198:        NA        NA        NA        NA        NA        NA        NA
#>       hh_age_20 class_size country hh_size part_age_exact part_age age.group
#>          <lgcl>      <int>  <fctr>   <int>          <int>    <int>    <fctr>
#>    1:        NA         NA   Italy       4              3        3     [0,5)
#>    2:        NA         NA   Italy       5              4        4     [0,5)
#>    3:        NA         NA   Italy       4              3        3     [0,5)
#>    4:        NA         NA   Italy       4              3        3     [0,5)
#>    5:        NA         15 Germany       5              4        4     [0,5)
#>   ---                                                                       
#> 7194:        NA         26 Germany       3             15       15  [15,Inf)
#> 7195:        NA         NA Germany       3             15       15  [15,Inf)
#> 7196:        NA         25 Germany       4             15       15  [15,Inf)
#> 7197:        NA         17 Germany       3             15       15  [15,Inf)
#> 7198:        NA         32 Germany       4             15       15  [15,Inf)
#>       upper.age.limit weight
#>                 <num>  <num>
#>    1:               5    0.3
#>    2:               5    0.3
#>    3:               5    0.3
#>    4:               5    0.3
#>    5:               5    0.4
#>   ---                       
#> 7194:              91    0.4
#> 7195:              91    0.4
#> 7196:              91    0.4
#> 7197:              91    0.4
#> 7198:              91    0.4
#> 
#> $contacts
#>        cont_id part_id cnt_age_exact cnt_age_est_min cnt_age_est_max cnt_gender
#>          <int>   <int>         <int>           <int>           <int>     <char>
#>     1:       1       1            42              NA              NA          F
#>     2:       2       1             9              NA              NA          F
#>     3:       3       1            NA              40              45          F
#>     4:       4       1             8              NA              NA          F
#>     5:       5       1            NA              28              30          F
#>    ---                                                                         
#> 97900:   97900    8001            11              NA              NA          F
#> 97901:   97901    8001             3              NA              NA          M
#> 97902:   97902    8001            43              NA              NA          F
#> 97903:   97903    8001            33              NA              NA          F
#> 97904:   97904    8001            15              NA              NA          F
#>        cnt_home cnt_work cnt_school cnt_transport cnt_leisure cnt_otherplace
#>           <int>    <int>      <int>         <int>       <int>          <int>
#>     1:        1        0          0             0           0              0
#>     2:        0        0          1             0           0              0
#>     3:        0        0          0             0           1              0
#>     4:        0        0          1             0           0              0
#>     5:        0        0          1             0           0              0
#>    ---                                                                      
#> 97900:        1        0          0             0           0              0
#> 97901:        1        0          0             0           0              0
#> 97902:        1        0          0             0           0              0
#> 97903:        0        0          0             0           0              1
#> 97904:        0        0          0             0           1              0
#>        frequency_multi phys_contact duration_multi cnt_age contact.age.group
#>                  <int>        <int>          <int>   <int>            <fctr>
#>     1:               1            1              4      42          [15,Inf)
#>     2:               1            1              5       9            [5,15)
#>     3:               3            1              4      42          [15,Inf)
#>     4:               1            1              5       8            [5,15)
#>     5:               2            1              3      29          [15,Inf)
#>    ---                                                                      
#> 97900:               1            1              5      11            [5,15)
#> 97901:               1            1              5       3             [0,5)
#> 97902:               1            1              5      43          [15,Inf)
#> 97903:               4            1              3      33          [15,Inf)
#> 97904:               3            1              4      15          [15,Inf)
#> 
#> $reference
#> $reference$title
#> [1] "POLYMOD social contact data"
#> 
#> $reference$bibtype
#> [1] "Misc"
#> 
#> $reference$author
#>  [1] "Joël Mossong"               "Niel Hens"                 
#>  [3] "Mark Jit"                   "Philippe Beutels"          
#>  [5] "Kari Auranen"               "Rafael Mikolajczyk"        
#>  [7] "Marco Massari"              "Stefania Salmaso"          
#>  [9] "Gianpaolo Scalia Tomba"     "Jacco Wallinga"            
#> [11] "Janneke Heijne"             "Malgorzata Sadkowska-Todys"
#> [13] "Magdalena Rosinska"         "W. John Edmunds"           
#> 
#> $reference$year
#> [1] 2017
#> 
#> $reference$note
#> [1] "Version 1.1"
#> 
#> $reference$doi
#> [1] "10.5281/zenodo.1157934"
#> 
#> 
#> attr(,"class")
#> [1] "contact_survey"

# ── unnamed vector + groups (total-weight semantics) ─────────────
# Each `target[g]` is the *total* weight assigned to participants in
# `groups[[g]]`. Here weekdays together carry weight 5, weekend days
# together carry weight 2:
uk |> weigh("dayofweek", target = c(5, 2), groups = list(1:5, c(0, 6)))
#> $participants
#> Key: <lower.age.limit>
#>       lower.age.limit      hh_id part_id part_gender part_occupation
#>                 <num>     <char>   <int>      <char>           <int>
#>    1:               0 Mo08HH4520    4520           F               5
#>    2:               0 Mo08HH4521    4521           M               5
#>    3:               0 Mo08HH4522    4522           M               5
#>    4:               0 Mo08HH4525    4525           F               5
#>    5:               0 Mo08HH4526    4526           M               6
#>   ---                                                               
#> 1007:              15 Mo08HH5518    5518           M              NA
#> 1008:              15 Mo08HH5519    5519           F              NA
#> 1009:              15 Mo08HH5520    5520           F              NA
#> 1010:              15 Mo08HH5521    5521           F               4
#> 1011:              15 Mo08HH5522    5522           M              NA
#>       part_occupation_detail part_education part_education_length
#>                        <int>          <int>                 <int>
#>    1:                      4              4                    13
#>    2:                      3              4                    13
#>    3:                      1              4                    13
#>    4:                     NA              4                    13
#>    5:                     NA              4                    13
#>   ---                                                            
#> 1007:                     NA              4                    13
#> 1008:                     NA              4                    13
#> 1009:                     NA              4                    13
#> 1010:                     NA              5                    16
#> 1011:                     NA              4                    13
#>       participant_school_year participant_nationality child_care
#>                         <int>                  <char>     <char>
#>    1:                      NA                      UK          Y
#>    2:                      NA                      UK          Y
#>    3:                      NA                      UK          Y
#>    4:                      NA                      UK          Y
#>    5:                      NA                      UK          N
#>   ---                                                           
#> 1007:                      NA                      OT           
#> 1008:                      NA                      OT           
#> 1009:                      NA                      OT           
#> 1010:                      NA                      UK           
#> 1011:                      NA                      OT           
#>       child_care_detail child_relationship child_nationality problems diary_how
#>                   <int>              <int>            <char>   <char>     <int>
#>    1:                NA                  1                UK                 NA
#>    2:                NA                  2                UK                 NA
#>    3:                NA                  1                UK                 NA
#>    4:                NA                  1                UK                 NA
#>    5:                NA                  1                UK                 NA
#>   ---                                                                          
#> 1007:                NA                 NA                                   NA
#> 1008:                NA                 NA                                   NA
#> 1009:                NA                 NA                                   NA
#> 1010:                NA                 NA                                   NA
#> 1011:                NA                 NA                                   NA
#>       diary_missed_unsp diary_missed_skin diary_missed_noskin  sday_id  type
#>                   <int>             <int>               <int>    <int> <int>
#>    1:                 1                 1                   1 20060422     3
#>    2:                 1                 1                   1 20060422     3
#>    3:                 1                 1                   1 20060422     3
#>    4:                 1                 1                   1 20060421     3
#>    5:                 1                 1                   1 20060421     3
#>   ---                                                                       
#> 1007:                 1                 1                   1 20060512     1
#> 1008:                 1                 1                   1 20060512     1
#> 1009:                 1                 1                   1 20060512     1
#> 1010:                 1                 1                   1 20060512     1
#> 1011:                 1                 1                   1 20060512     1
#>         day month  year dayofweek hh_age_1 hh_age_2 hh_age_3 hh_age_4 hh_age_5
#>       <int> <int> <int>     <int>    <int>    <int>    <int>    <int>    <int>
#>    1:    22     4  2006         6        3       33       NA       NA       NA
#>    2:    22     4  2006         6        1        2       25       30       NA
#>    3:    22     4  2006         6        4       31       31       NA       NA
#>    4:    21     4  2006         5        4        7       33       NA       NA
#>    5:    21     4  2006         5        2       18       46       48       NA
#>   ---                                                                         
#> 1007:    12     5  2006         5       43       50       NA       NA       NA
#> 1008:    12     5  2006         5       57       64       NA       NA       NA
#> 1009:    12     5  2006         5       19       27       52       55       NA
#> 1010:    12     5  2006         5        0        3       34       40       NA
#> 1011:    12     5  2006         5       35       39       NA       NA       NA
#>       hh_age_6 hh_age_7 hh_age_8 hh_age_9 hh_age_10 hh_age_11 hh_age_12
#>          <int>    <int>    <int>    <int>     <int>     <int>     <int>
#>    1:       NA       NA       NA       NA        NA        NA        NA
#>    2:       NA       NA       NA       NA        NA        NA        NA
#>    3:       NA       NA       NA       NA        NA        NA        NA
#>    4:       NA       NA       NA       NA        NA        NA        NA
#>    5:       NA       NA       NA       NA        NA        NA        NA
#>   ---                                                                  
#> 1007:       NA       NA       NA       NA        NA        NA        NA
#> 1008:       NA       NA       NA       NA        NA        NA        NA
#> 1009:       NA       NA       NA       NA        NA        NA        NA
#> 1010:       NA       NA       NA       NA        NA        NA        NA
#> 1011:       NA       NA       NA       NA        NA        NA        NA
#>       hh_age_13 hh_age_14 hh_age_15 hh_age_16 hh_age_17 hh_age_18 hh_age_19
#>           <int>     <int>     <int>    <lgcl>    <lgcl>    <lgcl>    <lgcl>
#>    1:        NA        NA        NA        NA        NA        NA        NA
#>    2:        NA        NA        NA        NA        NA        NA        NA
#>    3:        NA        NA        NA        NA        NA        NA        NA
#>    4:        NA        NA        NA        NA        NA        NA        NA
#>    5:        NA        NA        NA        NA        NA        NA        NA
#>   ---                                                                      
#> 1007:        NA        NA        NA        NA        NA        NA        NA
#> 1008:        NA        NA        NA        NA        NA        NA        NA
#> 1009:        NA        NA        NA        NA        NA        NA        NA
#> 1010:        NA        NA        NA        NA        NA        NA        NA
#> 1011:        NA        NA        NA        NA        NA        NA        NA
#>       hh_age_20 class_size        country hh_size part_age_exact part_age
#>          <lgcl>      <int>         <fctr>   <int>          <int>    <int>
#>    1:        NA         NA United Kingdom       2              3        3
#>    2:        NA         NA United Kingdom       4              2        2
#>    3:        NA         NA United Kingdom       3              4        4
#>    4:        NA         NA United Kingdom       3              4        4
#>    5:        NA         NA United Kingdom       4              2        2
#>   ---                                                                    
#> 1007:        NA         NA United Kingdom       2             50       50
#> 1008:        NA         NA United Kingdom       2             57       57
#> 1009:        NA         NA United Kingdom       4             52       52
#> 1010:        NA         NA United Kingdom       4             34       34
#> 1011:        NA         NA United Kingdom       2             39       39
#>       age.group upper.age.limit      weight
#>          <fctr>           <num>       <num>
#>    1:     [0,5)               5 0.007751938
#>    2:     [0,5)               5 0.007751938
#>    3:     [0,5)               5 0.007751938
#>    4:     [0,5)               5 0.007052186
#>    5:     [0,5)               5 0.007052186
#>   ---                                      
#> 1007:  [15,Inf)              80 0.007052186
#> 1008:  [15,Inf)              80 0.007052186
#> 1009:  [15,Inf)              80 0.007052186
#> 1010:  [15,Inf)              80 0.007052186
#> 1011:  [15,Inf)              80 0.007052186
#> 
#> $contacts
#>        cont_id part_id cnt_age_exact cnt_age_est_min cnt_age_est_max cnt_gender
#>          <int>   <int>         <int>           <int>           <int>     <char>
#>     1:   66023    4517             4              NA              NA          M
#>     2:   66024    4517            40              NA              NA          F
#>     3:   66025    4517            31              NA              NA          F
#>     4:   66026    4517            NA              50              55          F
#>     5:   66027    4517            29              NA              NA          M
#>    ---                                                                         
#> 11872:   77894    5522            NA              10              20          F
#> 11873:   77895    5522            35              NA              NA          F
#> 11874:   77896    5522            50              NA              NA          M
#> 11875:   77897    5522            NA              30              40          M
#> 11876:   77898    5522            NA              40              50          M
#>        cnt_home cnt_work cnt_school cnt_transport cnt_leisure cnt_otherplace
#>           <int>    <int>      <int>         <int>       <int>          <int>
#>     1:        0        0          1             0           0              0
#>     2:        0        0          0             0           0              1
#>     3:        1        0          0             0           0              0
#>     4:        0        0          0             0           0              1
#>     5:        1        0          0             0           0              0
#>    ---                                                                      
#> 11872:        1        0          0             0           0              0
#> 11873:        1        0          0             0           0              0
#> 11874:        0        1          0             0           0              0
#> 11875:        0        1          0             0           0              0
#> 11876:        0        1          0             0           0              0
#>        frequency_multi phys_contact duration_multi cnt_age contact.age.group
#>                  <int>        <int>          <int>   <int>            <fctr>
#>     1:               1            1              4       4             [0,5)
#>     2:               2            2              2      40          [15,Inf)
#>     3:               1            1              5      31          [15,Inf)
#>     4:               2            2              2      52          [15,Inf)
#>     5:               1            1              4      29          [15,Inf)
#>    ---                                                                      
#> 11872:               1            2              1      15          [15,Inf)
#> 11873:               1            1              5      35          [15,Inf)
#> 11874:               2            1              3      50          [15,Inf)
#> 11875:               2            2              3      35          [15,Inf)
#> 11876:               1            1              4      45          [15,Inf)
#> 
#> $reference
#> $reference$title
#> [1] "POLYMOD social contact data"
#> 
#> $reference$bibtype
#> [1] "Misc"
#> 
#> $reference$author
#>  [1] "Joël Mossong"               "Niel Hens"                 
#>  [3] "Mark Jit"                   "Philippe Beutels"          
#>  [5] "Kari Auranen"               "Rafael Mikolajczyk"        
#>  [7] "Marco Massari"              "Stefania Salmaso"          
#>  [9] "Gianpaolo Scalia Tomba"     "Jacco Wallinga"            
#> [11] "Janneke Heijne"             "Malgorzata Sadkowska-Todys"
#> [13] "Magdalena Rosinska"         "W. John Edmunds"           
#> 
#> $reference$year
#> [1] 2017
#> 
#> $reference$note
#> [1] "Version 1.1"
#> 
#> $reference$doi
#> [1] "10.5281/zenodo.1157934"
#> 
#> 
#> attr(,"class")
#> [1] "contact_survey"

# The same is available as the convenience:
uk |> weigh_by_dayofweek()
#> $participants
#> Key: <lower.age.limit>
#>       lower.age.limit      hh_id part_id part_gender part_occupation
#>                 <num>     <char>   <int>      <char>           <int>
#>    1:               0 Mo08HH4520    4520           F               5
#>    2:               0 Mo08HH4521    4521           M               5
#>    3:               0 Mo08HH4522    4522           M               5
#>    4:               0 Mo08HH4525    4525           F               5
#>    5:               0 Mo08HH4526    4526           M               6
#>   ---                                                               
#> 1007:              15 Mo08HH5518    5518           M              NA
#> 1008:              15 Mo08HH5519    5519           F              NA
#> 1009:              15 Mo08HH5520    5520           F              NA
#> 1010:              15 Mo08HH5521    5521           F               4
#> 1011:              15 Mo08HH5522    5522           M              NA
#>       part_occupation_detail part_education part_education_length
#>                        <int>          <int>                 <int>
#>    1:                      4              4                    13
#>    2:                      3              4                    13
#>    3:                      1              4                    13
#>    4:                     NA              4                    13
#>    5:                     NA              4                    13
#>   ---                                                            
#> 1007:                     NA              4                    13
#> 1008:                     NA              4                    13
#> 1009:                     NA              4                    13
#> 1010:                     NA              5                    16
#> 1011:                     NA              4                    13
#>       participant_school_year participant_nationality child_care
#>                         <int>                  <char>     <char>
#>    1:                      NA                      UK          Y
#>    2:                      NA                      UK          Y
#>    3:                      NA                      UK          Y
#>    4:                      NA                      UK          Y
#>    5:                      NA                      UK          N
#>   ---                                                           
#> 1007:                      NA                      OT           
#> 1008:                      NA                      OT           
#> 1009:                      NA                      OT           
#> 1010:                      NA                      UK           
#> 1011:                      NA                      OT           
#>       child_care_detail child_relationship child_nationality problems diary_how
#>                   <int>              <int>            <char>   <char>     <int>
#>    1:                NA                  1                UK                 NA
#>    2:                NA                  2                UK                 NA
#>    3:                NA                  1                UK                 NA
#>    4:                NA                  1                UK                 NA
#>    5:                NA                  1                UK                 NA
#>   ---                                                                          
#> 1007:                NA                 NA                                   NA
#> 1008:                NA                 NA                                   NA
#> 1009:                NA                 NA                                   NA
#> 1010:                NA                 NA                                   NA
#> 1011:                NA                 NA                                   NA
#>       diary_missed_unsp diary_missed_skin diary_missed_noskin  sday_id  type
#>                   <int>             <int>               <int>    <int> <int>
#>    1:                 1                 1                   1 20060422     3
#>    2:                 1                 1                   1 20060422     3
#>    3:                 1                 1                   1 20060422     3
#>    4:                 1                 1                   1 20060421     3
#>    5:                 1                 1                   1 20060421     3
#>   ---                                                                       
#> 1007:                 1                 1                   1 20060512     1
#> 1008:                 1                 1                   1 20060512     1
#> 1009:                 1                 1                   1 20060512     1
#> 1010:                 1                 1                   1 20060512     1
#> 1011:                 1                 1                   1 20060512     1
#>         day month  year dayofweek hh_age_1 hh_age_2 hh_age_3 hh_age_4 hh_age_5
#>       <int> <int> <int>     <int>    <int>    <int>    <int>    <int>    <int>
#>    1:    22     4  2006         6        3       33       NA       NA       NA
#>    2:    22     4  2006         6        1        2       25       30       NA
#>    3:    22     4  2006         6        4       31       31       NA       NA
#>    4:    21     4  2006         5        4        7       33       NA       NA
#>    5:    21     4  2006         5        2       18       46       48       NA
#>   ---                                                                         
#> 1007:    12     5  2006         5       43       50       NA       NA       NA
#> 1008:    12     5  2006         5       57       64       NA       NA       NA
#> 1009:    12     5  2006         5       19       27       52       55       NA
#> 1010:    12     5  2006         5        0        3       34       40       NA
#> 1011:    12     5  2006         5       35       39       NA       NA       NA
#>       hh_age_6 hh_age_7 hh_age_8 hh_age_9 hh_age_10 hh_age_11 hh_age_12
#>          <int>    <int>    <int>    <int>     <int>     <int>     <int>
#>    1:       NA       NA       NA       NA        NA        NA        NA
#>    2:       NA       NA       NA       NA        NA        NA        NA
#>    3:       NA       NA       NA       NA        NA        NA        NA
#>    4:       NA       NA       NA       NA        NA        NA        NA
#>    5:       NA       NA       NA       NA        NA        NA        NA
#>   ---                                                                  
#> 1007:       NA       NA       NA       NA        NA        NA        NA
#> 1008:       NA       NA       NA       NA        NA        NA        NA
#> 1009:       NA       NA       NA       NA        NA        NA        NA
#> 1010:       NA       NA       NA       NA        NA        NA        NA
#> 1011:       NA       NA       NA       NA        NA        NA        NA
#>       hh_age_13 hh_age_14 hh_age_15 hh_age_16 hh_age_17 hh_age_18 hh_age_19
#>           <int>     <int>     <int>    <lgcl>    <lgcl>    <lgcl>    <lgcl>
#>    1:        NA        NA        NA        NA        NA        NA        NA
#>    2:        NA        NA        NA        NA        NA        NA        NA
#>    3:        NA        NA        NA        NA        NA        NA        NA
#>    4:        NA        NA        NA        NA        NA        NA        NA
#>    5:        NA        NA        NA        NA        NA        NA        NA
#>   ---                                                                      
#> 1007:        NA        NA        NA        NA        NA        NA        NA
#> 1008:        NA        NA        NA        NA        NA        NA        NA
#> 1009:        NA        NA        NA        NA        NA        NA        NA
#> 1010:        NA        NA        NA        NA        NA        NA        NA
#> 1011:        NA        NA        NA        NA        NA        NA        NA
#>       hh_age_20 class_size        country hh_size part_age_exact part_age
#>          <lgcl>      <int>         <fctr>   <int>          <int>    <int>
#>    1:        NA         NA United Kingdom       2              3        3
#>    2:        NA         NA United Kingdom       4              2        2
#>    3:        NA         NA United Kingdom       3              4        4
#>    4:        NA         NA United Kingdom       3              4        4
#>    5:        NA         NA United Kingdom       4              2        2
#>   ---                                                                    
#> 1007:        NA         NA United Kingdom       2             50       50
#> 1008:        NA         NA United Kingdom       2             57       57
#> 1009:        NA         NA United Kingdom       4             52       52
#> 1010:        NA         NA United Kingdom       4             34       34
#> 1011:        NA         NA United Kingdom       2             39       39
#>       age.group upper.age.limit      weight
#>          <fctr>           <num>       <num>
#>    1:     [0,5)               5 0.007751938
#>    2:     [0,5)               5 0.007751938
#>    3:     [0,5)               5 0.007751938
#>    4:     [0,5)               5 0.007052186
#>    5:     [0,5)               5 0.007052186
#>   ---                                      
#> 1007:  [15,Inf)              80 0.007052186
#> 1008:  [15,Inf)              80 0.007052186
#> 1009:  [15,Inf)              80 0.007052186
#> 1010:  [15,Inf)              80 0.007052186
#> 1011:  [15,Inf)              80 0.007052186
#> 
#> $contacts
#>        cont_id part_id cnt_age_exact cnt_age_est_min cnt_age_est_max cnt_gender
#>          <int>   <int>         <int>           <int>           <int>     <char>
#>     1:   66023    4517             4              NA              NA          M
#>     2:   66024    4517            40              NA              NA          F
#>     3:   66025    4517            31              NA              NA          F
#>     4:   66026    4517            NA              50              55          F
#>     5:   66027    4517            29              NA              NA          M
#>    ---                                                                         
#> 11872:   77894    5522            NA              10              20          F
#> 11873:   77895    5522            35              NA              NA          F
#> 11874:   77896    5522            50              NA              NA          M
#> 11875:   77897    5522            NA              30              40          M
#> 11876:   77898    5522            NA              40              50          M
#>        cnt_home cnt_work cnt_school cnt_transport cnt_leisure cnt_otherplace
#>           <int>    <int>      <int>         <int>       <int>          <int>
#>     1:        0        0          1             0           0              0
#>     2:        0        0          0             0           0              1
#>     3:        1        0          0             0           0              0
#>     4:        0        0          0             0           0              1
#>     5:        1        0          0             0           0              0
#>    ---                                                                      
#> 11872:        1        0          0             0           0              0
#> 11873:        1        0          0             0           0              0
#> 11874:        0        1          0             0           0              0
#> 11875:        0        1          0             0           0              0
#> 11876:        0        1          0             0           0              0
#>        frequency_multi phys_contact duration_multi cnt_age contact.age.group
#>                  <int>        <int>          <int>   <int>            <fctr>
#>     1:               1            1              4       4             [0,5)
#>     2:               2            2              2      40          [15,Inf)
#>     3:               1            1              5      31          [15,Inf)
#>     4:               2            2              2      52          [15,Inf)
#>     5:               1            1              4      29          [15,Inf)
#>    ---                                                                      
#> 11872:               1            2              1      15          [15,Inf)
#> 11873:               1            1              5      35          [15,Inf)
#> 11874:               2            1              3      50          [15,Inf)
#> 11875:               2            2              3      35          [15,Inf)
#> 11876:               1            1              4      45          [15,Inf)
#> 
#> $reference
#> $reference$title
#> [1] "POLYMOD social contact data"
#> 
#> $reference$bibtype
#> [1] "Misc"
#> 
#> $reference$author
#>  [1] "Joël Mossong"               "Niel Hens"                 
#>  [3] "Mark Jit"                   "Philippe Beutels"          
#>  [5] "Kari Auranen"               "Rafael Mikolajczyk"        
#>  [7] "Marco Massari"              "Stefania Salmaso"          
#>  [9] "Gianpaolo Scalia Tomba"     "Jacco Wallinga"            
#> [11] "Janneke Heijne"             "Malgorzata Sadkowska-Todys"
#> [13] "Magdalena Rosinska"         "W. John Edmunds"           
#> 
#> $reference$year
#> [1] 2017
#> 
#> $reference$note
#> [1] "Version 1.1"
#> 
#> $reference$doi
#> [1] "10.5281/zenodo.1157934"
#> 
#> 
#> attr(,"class")
#> [1] "contact_survey"

# ── named vector ─────────────────────────────────────────────────
# `names(target)` are matched against `by` values; each value is the
# total weight for participants with that key.
uk$participants[, agecat := ifelse(part_age < 18, "child", "adult")]
#> Key: <lower.age.limit>
#>       lower.age.limit      hh_id part_id part_gender part_occupation
#>                 <num>     <char>   <int>      <char>           <int>
#>    1:               0 Mo08HH4520    4520           F               5
#>    2:               0 Mo08HH4521    4521           M               5
#>    3:               0 Mo08HH4522    4522           M               5
#>    4:               0 Mo08HH4525    4525           F               5
#>    5:               0 Mo08HH4526    4526           M               6
#>   ---                                                               
#> 1007:              15 Mo08HH5518    5518           M              NA
#> 1008:              15 Mo08HH5519    5519           F              NA
#> 1009:              15 Mo08HH5520    5520           F              NA
#> 1010:              15 Mo08HH5521    5521           F               4
#> 1011:              15 Mo08HH5522    5522           M              NA
#>       part_occupation_detail part_education part_education_length
#>                        <int>          <int>                 <int>
#>    1:                      4              4                    13
#>    2:                      3              4                    13
#>    3:                      1              4                    13
#>    4:                     NA              4                    13
#>    5:                     NA              4                    13
#>   ---                                                            
#> 1007:                     NA              4                    13
#> 1008:                     NA              4                    13
#> 1009:                     NA              4                    13
#> 1010:                     NA              5                    16
#> 1011:                     NA              4                    13
#>       participant_school_year participant_nationality child_care
#>                         <int>                  <char>     <char>
#>    1:                      NA                      UK          Y
#>    2:                      NA                      UK          Y
#>    3:                      NA                      UK          Y
#>    4:                      NA                      UK          Y
#>    5:                      NA                      UK          N
#>   ---                                                           
#> 1007:                      NA                      OT           
#> 1008:                      NA                      OT           
#> 1009:                      NA                      OT           
#> 1010:                      NA                      UK           
#> 1011:                      NA                      OT           
#>       child_care_detail child_relationship child_nationality problems diary_how
#>                   <int>              <int>            <char>   <char>     <int>
#>    1:                NA                  1                UK                 NA
#>    2:                NA                  2                UK                 NA
#>    3:                NA                  1                UK                 NA
#>    4:                NA                  1                UK                 NA
#>    5:                NA                  1                UK                 NA
#>   ---                                                                          
#> 1007:                NA                 NA                                   NA
#> 1008:                NA                 NA                                   NA
#> 1009:                NA                 NA                                   NA
#> 1010:                NA                 NA                                   NA
#> 1011:                NA                 NA                                   NA
#>       diary_missed_unsp diary_missed_skin diary_missed_noskin  sday_id  type
#>                   <int>             <int>               <int>    <int> <int>
#>    1:                 1                 1                   1 20060422     3
#>    2:                 1                 1                   1 20060422     3
#>    3:                 1                 1                   1 20060422     3
#>    4:                 1                 1                   1 20060421     3
#>    5:                 1                 1                   1 20060421     3
#>   ---                                                                       
#> 1007:                 1                 1                   1 20060512     1
#> 1008:                 1                 1                   1 20060512     1
#> 1009:                 1                 1                   1 20060512     1
#> 1010:                 1                 1                   1 20060512     1
#> 1011:                 1                 1                   1 20060512     1
#>         day month  year dayofweek hh_age_1 hh_age_2 hh_age_3 hh_age_4 hh_age_5
#>       <int> <int> <int>     <int>    <int>    <int>    <int>    <int>    <int>
#>    1:    22     4  2006         6        3       33       NA       NA       NA
#>    2:    22     4  2006         6        1        2       25       30       NA
#>    3:    22     4  2006         6        4       31       31       NA       NA
#>    4:    21     4  2006         5        4        7       33       NA       NA
#>    5:    21     4  2006         5        2       18       46       48       NA
#>   ---                                                                         
#> 1007:    12     5  2006         5       43       50       NA       NA       NA
#> 1008:    12     5  2006         5       57       64       NA       NA       NA
#> 1009:    12     5  2006         5       19       27       52       55       NA
#> 1010:    12     5  2006         5        0        3       34       40       NA
#> 1011:    12     5  2006         5       35       39       NA       NA       NA
#>       hh_age_6 hh_age_7 hh_age_8 hh_age_9 hh_age_10 hh_age_11 hh_age_12
#>          <int>    <int>    <int>    <int>     <int>     <int>     <int>
#>    1:       NA       NA       NA       NA        NA        NA        NA
#>    2:       NA       NA       NA       NA        NA        NA        NA
#>    3:       NA       NA       NA       NA        NA        NA        NA
#>    4:       NA       NA       NA       NA        NA        NA        NA
#>    5:       NA       NA       NA       NA        NA        NA        NA
#>   ---                                                                  
#> 1007:       NA       NA       NA       NA        NA        NA        NA
#> 1008:       NA       NA       NA       NA        NA        NA        NA
#> 1009:       NA       NA       NA       NA        NA        NA        NA
#> 1010:       NA       NA       NA       NA        NA        NA        NA
#> 1011:       NA       NA       NA       NA        NA        NA        NA
#>       hh_age_13 hh_age_14 hh_age_15 hh_age_16 hh_age_17 hh_age_18 hh_age_19
#>           <int>     <int>     <int>    <lgcl>    <lgcl>    <lgcl>    <lgcl>
#>    1:        NA        NA        NA        NA        NA        NA        NA
#>    2:        NA        NA        NA        NA        NA        NA        NA
#>    3:        NA        NA        NA        NA        NA        NA        NA
#>    4:        NA        NA        NA        NA        NA        NA        NA
#>    5:        NA        NA        NA        NA        NA        NA        NA
#>   ---                                                                      
#> 1007:        NA        NA        NA        NA        NA        NA        NA
#> 1008:        NA        NA        NA        NA        NA        NA        NA
#> 1009:        NA        NA        NA        NA        NA        NA        NA
#> 1010:        NA        NA        NA        NA        NA        NA        NA
#> 1011:        NA        NA        NA        NA        NA        NA        NA
#>       hh_age_20 class_size        country hh_size part_age_exact part_age
#>          <lgcl>      <int>         <fctr>   <int>          <int>    <int>
#>    1:        NA         NA United Kingdom       2              3        3
#>    2:        NA         NA United Kingdom       4              2        2
#>    3:        NA         NA United Kingdom       3              4        4
#>    4:        NA         NA United Kingdom       3              4        4
#>    5:        NA         NA United Kingdom       4              2        2
#>   ---                                                                    
#> 1007:        NA         NA United Kingdom       2             50       50
#> 1008:        NA         NA United Kingdom       2             57       57
#> 1009:        NA         NA United Kingdom       4             52       52
#> 1010:        NA         NA United Kingdom       4             34       34
#> 1011:        NA         NA United Kingdom       2             39       39
#>       age.group upper.age.limit agecat
#>          <fctr>           <num> <char>
#>    1:     [0,5)               5  child
#>    2:     [0,5)               5  child
#>    3:     [0,5)               5  child
#>    4:     [0,5)               5  child
#>    5:     [0,5)               5  child
#>   ---                                 
#> 1007:  [15,Inf)              80  adult
#> 1008:  [15,Inf)              80  adult
#> 1009:  [15,Inf)              80  adult
#> 1010:  [15,Inf)              80  adult
#> 1011:  [15,Inf)              80  adult
uk |> weigh("agecat", target = c(child = 0.25, adult = 0.75))
#> $participants
#> Key: <lower.age.limit>
#>       lower.age.limit      hh_id part_id part_gender part_occupation
#>                 <num>     <char>   <int>      <char>           <int>
#>    1:               0 Mo08HH4520    4520           F               5
#>    2:               0 Mo08HH4521    4521           M               5
#>    3:               0 Mo08HH4522    4522           M               5
#>    4:               0 Mo08HH4525    4525           F               5
#>    5:               0 Mo08HH4526    4526           M               6
#>   ---                                                               
#> 1007:              15 Mo08HH5518    5518           M              NA
#> 1008:              15 Mo08HH5519    5519           F              NA
#> 1009:              15 Mo08HH5520    5520           F              NA
#> 1010:              15 Mo08HH5521    5521           F               4
#> 1011:              15 Mo08HH5522    5522           M              NA
#>       part_occupation_detail part_education part_education_length
#>                        <int>          <int>                 <int>
#>    1:                      4              4                    13
#>    2:                      3              4                    13
#>    3:                      1              4                    13
#>    4:                     NA              4                    13
#>    5:                     NA              4                    13
#>   ---                                                            
#> 1007:                     NA              4                    13
#> 1008:                     NA              4                    13
#> 1009:                     NA              4                    13
#> 1010:                     NA              5                    16
#> 1011:                     NA              4                    13
#>       participant_school_year participant_nationality child_care
#>                         <int>                  <char>     <char>
#>    1:                      NA                      UK          Y
#>    2:                      NA                      UK          Y
#>    3:                      NA                      UK          Y
#>    4:                      NA                      UK          Y
#>    5:                      NA                      UK          N
#>   ---                                                           
#> 1007:                      NA                      OT           
#> 1008:                      NA                      OT           
#> 1009:                      NA                      OT           
#> 1010:                      NA                      UK           
#> 1011:                      NA                      OT           
#>       child_care_detail child_relationship child_nationality problems diary_how
#>                   <int>              <int>            <char>   <char>     <int>
#>    1:                NA                  1                UK                 NA
#>    2:                NA                  2                UK                 NA
#>    3:                NA                  1                UK                 NA
#>    4:                NA                  1                UK                 NA
#>    5:                NA                  1                UK                 NA
#>   ---                                                                          
#> 1007:                NA                 NA                                   NA
#> 1008:                NA                 NA                                   NA
#> 1009:                NA                 NA                                   NA
#> 1010:                NA                 NA                                   NA
#> 1011:                NA                 NA                                   NA
#>       diary_missed_unsp diary_missed_skin diary_missed_noskin  sday_id  type
#>                   <int>             <int>               <int>    <int> <int>
#>    1:                 1                 1                   1 20060422     3
#>    2:                 1                 1                   1 20060422     3
#>    3:                 1                 1                   1 20060422     3
#>    4:                 1                 1                   1 20060421     3
#>    5:                 1                 1                   1 20060421     3
#>   ---                                                                       
#> 1007:                 1                 1                   1 20060512     1
#> 1008:                 1                 1                   1 20060512     1
#> 1009:                 1                 1                   1 20060512     1
#> 1010:                 1                 1                   1 20060512     1
#> 1011:                 1                 1                   1 20060512     1
#>         day month  year dayofweek hh_age_1 hh_age_2 hh_age_3 hh_age_4 hh_age_5
#>       <int> <int> <int>     <int>    <int>    <int>    <int>    <int>    <int>
#>    1:    22     4  2006         6        3       33       NA       NA       NA
#>    2:    22     4  2006         6        1        2       25       30       NA
#>    3:    22     4  2006         6        4       31       31       NA       NA
#>    4:    21     4  2006         5        4        7       33       NA       NA
#>    5:    21     4  2006         5        2       18       46       48       NA
#>   ---                                                                         
#> 1007:    12     5  2006         5       43       50       NA       NA       NA
#> 1008:    12     5  2006         5       57       64       NA       NA       NA
#> 1009:    12     5  2006         5       19       27       52       55       NA
#> 1010:    12     5  2006         5        0        3       34       40       NA
#> 1011:    12     5  2006         5       35       39       NA       NA       NA
#>       hh_age_6 hh_age_7 hh_age_8 hh_age_9 hh_age_10 hh_age_11 hh_age_12
#>          <int>    <int>    <int>    <int>     <int>     <int>     <int>
#>    1:       NA       NA       NA       NA        NA        NA        NA
#>    2:       NA       NA       NA       NA        NA        NA        NA
#>    3:       NA       NA       NA       NA        NA        NA        NA
#>    4:       NA       NA       NA       NA        NA        NA        NA
#>    5:       NA       NA       NA       NA        NA        NA        NA
#>   ---                                                                  
#> 1007:       NA       NA       NA       NA        NA        NA        NA
#> 1008:       NA       NA       NA       NA        NA        NA        NA
#> 1009:       NA       NA       NA       NA        NA        NA        NA
#> 1010:       NA       NA       NA       NA        NA        NA        NA
#> 1011:       NA       NA       NA       NA        NA        NA        NA
#>       hh_age_13 hh_age_14 hh_age_15 hh_age_16 hh_age_17 hh_age_18 hh_age_19
#>           <int>     <int>     <int>    <lgcl>    <lgcl>    <lgcl>    <lgcl>
#>    1:        NA        NA        NA        NA        NA        NA        NA
#>    2:        NA        NA        NA        NA        NA        NA        NA
#>    3:        NA        NA        NA        NA        NA        NA        NA
#>    4:        NA        NA        NA        NA        NA        NA        NA
#>    5:        NA        NA        NA        NA        NA        NA        NA
#>   ---                                                                      
#> 1007:        NA        NA        NA        NA        NA        NA        NA
#> 1008:        NA        NA        NA        NA        NA        NA        NA
#> 1009:        NA        NA        NA        NA        NA        NA        NA
#> 1010:        NA        NA        NA        NA        NA        NA        NA
#> 1011:        NA        NA        NA        NA        NA        NA        NA
#>       hh_age_20 class_size        country hh_size part_age_exact part_age
#>          <lgcl>      <int>         <fctr>   <int>          <int>    <int>
#>    1:        NA         NA United Kingdom       2              3        3
#>    2:        NA         NA United Kingdom       4              2        2
#>    3:        NA         NA United Kingdom       3              4        4
#>    4:        NA         NA United Kingdom       3              4        4
#>    5:        NA         NA United Kingdom       4              2        2
#>   ---                                                                    
#> 1007:        NA         NA United Kingdom       2             50       50
#> 1008:        NA         NA United Kingdom       2             57       57
#> 1009:        NA         NA United Kingdom       4             52       52
#> 1010:        NA         NA United Kingdom       4             34       34
#> 1011:        NA         NA United Kingdom       2             39       39
#>       age.group upper.age.limit agecat       weight
#>          <fctr>           <num> <char>        <num>
#>    1:     [0,5)               5  child 0.0006868132
#>    2:     [0,5)               5  child 0.0006868132
#>    3:     [0,5)               5  child 0.0006868132
#>    4:     [0,5)               5  child 0.0006868132
#>    5:     [0,5)               5  child 0.0006868132
#>   ---                                              
#> 1007:  [15,Inf)              80  adult 0.0011591963
#> 1008:  [15,Inf)              80  adult 0.0011591963
#> 1009:  [15,Inf)              80  adult 0.0011591963
#> 1010:  [15,Inf)              80  adult 0.0011591963
#> 1011:  [15,Inf)              80  adult 0.0011591963
#> 
#> $contacts
#>        cont_id part_id cnt_age_exact cnt_age_est_min cnt_age_est_max cnt_gender
#>          <int>   <int>         <int>           <int>           <int>     <char>
#>     1:   66023    4517             4              NA              NA          M
#>     2:   66024    4517            40              NA              NA          F
#>     3:   66025    4517            31              NA              NA          F
#>     4:   66026    4517            NA              50              55          F
#>     5:   66027    4517            29              NA              NA          M
#>    ---                                                                         
#> 11872:   77894    5522            NA              10              20          F
#> 11873:   77895    5522            35              NA              NA          F
#> 11874:   77896    5522            50              NA              NA          M
#> 11875:   77897    5522            NA              30              40          M
#> 11876:   77898    5522            NA              40              50          M
#>        cnt_home cnt_work cnt_school cnt_transport cnt_leisure cnt_otherplace
#>           <int>    <int>      <int>         <int>       <int>          <int>
#>     1:        0        0          1             0           0              0
#>     2:        0        0          0             0           0              1
#>     3:        1        0          0             0           0              0
#>     4:        0        0          0             0           0              1
#>     5:        1        0          0             0           0              0
#>    ---                                                                      
#> 11872:        1        0          0             0           0              0
#> 11873:        1        0          0             0           0              0
#> 11874:        0        1          0             0           0              0
#> 11875:        0        1          0             0           0              0
#> 11876:        0        1          0             0           0              0
#>        frequency_multi phys_contact duration_multi cnt_age contact.age.group
#>                  <int>        <int>          <int>   <int>            <fctr>
#>     1:               1            1              4       4             [0,5)
#>     2:               2            2              2      40          [15,Inf)
#>     3:               1            1              5      31          [15,Inf)
#>     4:               2            2              2      52          [15,Inf)
#>     5:               1            1              4      29          [15,Inf)
#>    ---                                                                      
#> 11872:               1            2              1      15          [15,Inf)
#> 11873:               1            1              5      35          [15,Inf)
#> 11874:               2            1              3      50          [15,Inf)
#> 11875:               2            2              3      35          [15,Inf)
#> 11876:               1            1              4      45          [15,Inf)
#> 
#> $reference
#> $reference$title
#> [1] "POLYMOD social contact data"
#> 
#> $reference$bibtype
#> [1] "Misc"
#> 
#> $reference$author
#>  [1] "Joël Mossong"               "Niel Hens"                 
#>  [3] "Mark Jit"                   "Philippe Beutels"          
#>  [5] "Kari Auranen"               "Rafael Mikolajczyk"        
#>  [7] "Marco Massari"              "Stefania Salmaso"          
#>  [9] "Gianpaolo Scalia Tomba"     "Jacco Wallinga"            
#> [11] "Janneke Heijne"             "Malgorzata Sadkowska-Todys"
#> [13] "Magdalena Rosinska"         "W. John Edmunds"           
#> 
#> $reference$year
#> [1] 2017
#> 
#> $reference$note
#> [1] "Version 1.1"
#> 
#> $reference$doi
#> [1] "10.5281/zenodo.1157934"
#> 
#> 
#> attr(,"class")
#> [1] "contact_survey"

# ── age post-stratification ──────────────────────────────────────
uk_pop <- data.frame(
  lower.age.limit = c(0, 5, 15, 65),
  population = c(3500000, 6000000, 40000000, 10000000)
)
uk |> weigh_by_age(uk_pop)
#> Warning: Not all age groups represented in population data (5-year age band).
#> ℹ Linearly estimating age group sizes from the 5-year bands.
#> $participants
#> Key: <part_age>
#>      part_age lower.age.limit      hh_id part_id part_gender part_occupation
#>         <int>           <num>     <char>   <int>      <char>           <int>
#>   1:        0               0 Mo08HH4536    4536           F               6
#>   2:        0               0 Mo08HH4538    4538           F               6
#>   3:        0               0 Mo08HH4540    4540           F               6
#>   4:        0               0 Mo08HH4541    4541           F               6
#>   5:        0               0 Mo08HH4542    4542           F               5
#>  ---                                                                        
#> 964:       65              15 Mo08HH5410    5410           M               3
#> 965:       65              15 Mo08HH5413    5413           F               3
#> 966:       65              15 Mo08HH5421    5421           F               3
#> 967:       65              15 Mo08HH5503    5503           M               1
#> 968:       65              15 Mo08HH5512    5512           F               3
#>      part_occupation_detail part_education part_education_length
#>                       <int>          <int>                 <int>
#>   1:                      2              4                    13
#>   2:                      2              4                    13
#>   3:                      1              6                    18
#>   4:                      1              6                    18
#>   5:                      4              4                    13
#>  ---                                                            
#> 964:                     NA              4                    13
#> 965:                     NA              4                    13
#> 966:                     NA              4                    13
#> 967:                      1              5                    16
#> 968:                     NA              4                    13
#>      participant_school_year participant_nationality child_care
#>                        <int>                  <char>     <char>
#>   1:                      NA                      UK          N
#>   2:                      NA                      UK          N
#>   3:                      NA                      UK          N
#>   4:                      NA                      UK          N
#>   5:                      NA                      UK          Y
#>  ---                                                           
#> 964:                      NA                      UK           
#> 965:                      NA                      UK           
#> 966:                      NA                      UK           
#> 967:                      NA                      UK           
#> 968:                      NA                      UK           
#>      child_care_detail child_relationship child_nationality problems diary_how
#>                  <int>              <int>            <char>   <char>     <int>
#>   1:                NA                  1                UK                 NA
#>   2:                NA                  1                UK                 NA
#>   3:                NA                  1                UK                 NA
#>   4:                NA                  1                UK                 NA
#>   5:                NA                  1                UK                 NA
#>  ---                                                                          
#> 964:                NA                 NA                                   NA
#> 965:                NA                 NA                                   NA
#> 966:                NA                 NA                                   NA
#> 967:                NA                 NA                                   NA
#> 968:                NA                 NA                                   NA
#>      diary_missed_unsp diary_missed_skin diary_missed_noskin  sday_id  type
#>                  <int>             <int>               <int>    <int> <int>
#>   1:                 4                 1                   4 20060424     3
#>   2:                 1                 1                   1 20060421     3
#>   3:                 1                 1                   1 20060422     3
#>   4:                 1                 1                   1 20060422     3
#>   5:                 1                 1                   1 20060422     3
#>  ---                                                                       
#> 964:                 1                 1                   1 20060424     1
#> 965:                 1                 1                   1 20060502     1
#> 966:                 1                 1                   1 20060429     1
#> 967:                 1                 1                   1 20060503     1
#> 968:                 1                 1                   1 20060430     1
#>        day month  year dayofweek hh_age_1 hh_age_2 hh_age_3 hh_age_4 hh_age_5
#>      <int> <int> <int>     <int>    <int>    <int>    <int>    <int>    <int>
#>   1:    24     4  2006         1       30        0       37       39       NA
#>   2:    21     4  2006         5        0        5        6        8       29
#>   3:    22     4  2006         6        0       43       47       NA       NA
#>   4:    22     4  2006         6        0       35       37       NA       NA
#>   5:    22     4  2006         6        0       10        7       30       29
#>  ---                                                                         
#> 964:    24     4  2006         1       65       NA       NA       NA       NA
#> 965:     2     5  2006         2       63       65       NA       NA       NA
#> 966:    29     4  2006         6       65       NA       NA       NA       NA
#> 967:     3     5  2006         3       65       NA       NA       NA       NA
#> 968:    30     4  2006         0        7       35       65       NA       NA
#>      hh_age_6 hh_age_7 hh_age_8 hh_age_9 hh_age_10 hh_age_11 hh_age_12
#>         <int>    <int>    <int>    <int>     <int>     <int>     <int>
#>   1:       NA       NA       NA       NA        NA        NA        NA
#>   2:       36       NA       NA       NA        NA        NA        NA
#>   3:       NA       NA       NA       NA        NA        NA        NA
#>   4:       NA       NA       NA       NA        NA        NA        NA
#>   5:       NA       NA       NA       NA        NA        NA        NA
#>  ---                                                                  
#> 964:       NA       NA       NA       NA        NA        NA        NA
#> 965:       NA       NA       NA       NA        NA        NA        NA
#> 966:       NA       NA       NA       NA        NA        NA        NA
#> 967:       NA       NA       NA       NA        NA        NA        NA
#> 968:       NA       NA       NA       NA        NA        NA        NA
#>      hh_age_13 hh_age_14 hh_age_15 hh_age_16 hh_age_17 hh_age_18 hh_age_19
#>          <int>     <int>     <int>    <lgcl>    <lgcl>    <lgcl>    <lgcl>
#>   1:        NA        NA        NA        NA        NA        NA        NA
#>   2:        NA        NA        NA        NA        NA        NA        NA
#>   3:        NA        NA        NA        NA        NA        NA        NA
#>   4:        NA        NA        NA        NA        NA        NA        NA
#>   5:        NA        NA        NA        NA        NA        NA        NA
#>  ---                                                                      
#> 964:        NA        NA        NA        NA        NA        NA        NA
#> 965:        NA        NA        NA        NA        NA        NA        NA
#> 966:        NA        NA        NA        NA        NA        NA        NA
#> 967:        NA        NA        NA        NA        NA        NA        NA
#> 968:        NA        NA        NA        NA        NA        NA        NA
#>      hh_age_20 class_size        country hh_size part_age_exact age.group
#>         <lgcl>      <int>         <fctr>   <int>          <int>    <fctr>
#>   1:        NA         NA United Kingdom       4              0     [0,5)
#>   2:        NA         NA United Kingdom       6              0     [0,5)
#>   3:        NA         NA United Kingdom       3              0     [0,5)
#>   4:        NA         NA United Kingdom       3              0     [0,5)
#>   5:        NA         NA United Kingdom       5              0     [0,5)
#>  ---                                                                     
#> 964:        NA         NA United Kingdom       1             65  [15,Inf)
#> 965:        NA         NA United Kingdom       2             65  [15,Inf)
#> 966:        NA         NA United Kingdom       1             65  [15,Inf)
#> 967:        NA         NA United Kingdom       1             65  [15,Inf)
#> 968:        NA         NA United Kingdom       3             65  [15,Inf)
#>      upper.age.limit agecat     weight
#>                <num> <char>      <num>
#>   1:               5  child  0.7929412
#>   2:               5  child  0.7929412
#>   3:               5  child  0.7929412
#>   4:               5  child  0.7929412
#>   5:               5  child  0.7929412
#>  ---                                  
#> 964:              80  adult 13.0704590
#> 965:              80  adult 13.0704590
#> 966:              80  adult 13.0704590
#> 967:              80  adult 13.0704590
#> 968:              80  adult 13.0704590
#> 
#> $contacts
#>        cont_id part_id cnt_age_exact cnt_age_est_min cnt_age_est_max cnt_gender
#>          <int>   <int>         <int>           <int>           <int>     <char>
#>     1:   66023    4517             4              NA              NA          M
#>     2:   66024    4517            40              NA              NA          F
#>     3:   66025    4517            31              NA              NA          F
#>     4:   66026    4517            NA              50              55          F
#>     5:   66027    4517            29              NA              NA          M
#>    ---                                                                         
#> 11872:   77894    5522            NA              10              20          F
#> 11873:   77895    5522            35              NA              NA          F
#> 11874:   77896    5522            50              NA              NA          M
#> 11875:   77897    5522            NA              30              40          M
#> 11876:   77898    5522            NA              40              50          M
#>        cnt_home cnt_work cnt_school cnt_transport cnt_leisure cnt_otherplace
#>           <int>    <int>      <int>         <int>       <int>          <int>
#>     1:        0        0          1             0           0              0
#>     2:        0        0          0             0           0              1
#>     3:        1        0          0             0           0              0
#>     4:        0        0          0             0           0              1
#>     5:        1        0          0             0           0              0
#>    ---                                                                      
#> 11872:        1        0          0             0           0              0
#> 11873:        1        0          0             0           0              0
#> 11874:        0        1          0             0           0              0
#> 11875:        0        1          0             0           0              0
#> 11876:        0        1          0             0           0              0
#>        frequency_multi phys_contact duration_multi cnt_age contact.age.group
#>                  <int>        <int>          <int>   <int>            <fctr>
#>     1:               1            1              4       4             [0,5)
#>     2:               2            2              2      40          [15,Inf)
#>     3:               1            1              5      31          [15,Inf)
#>     4:               2            2              2      52          [15,Inf)
#>     5:               1            1              4      29          [15,Inf)
#>    ---                                                                      
#> 11872:               1            2              1      15          [15,Inf)
#> 11873:               1            1              5      35          [15,Inf)
#> 11874:               2            1              3      50          [15,Inf)
#> 11875:               2            2              3      35          [15,Inf)
#> 11876:               1            1              4      45          [15,Inf)
#> 
#> $reference
#> $reference$title
#> [1] "POLYMOD social contact data"
#> 
#> $reference$bibtype
#> [1] "Misc"
#> 
#> $reference$author
#>  [1] "Joël Mossong"               "Niel Hens"                 
#>  [3] "Mark Jit"                   "Philippe Beutels"          
#>  [5] "Kari Auranen"               "Rafael Mikolajczyk"        
#>  [7] "Marco Massari"              "Stefania Salmaso"          
#>  [9] "Gianpaolo Scalia Tomba"     "Jacco Wallinga"            
#> [11] "Janneke Heijne"             "Malgorzata Sadkowska-Todys"
#> [13] "Magdalena Rosinska"         "W. John Edmunds"           
#> 
#> $reference$year
#> [1] 2017
#> 
#> $reference$note
#> [1] "Version 1.1"
#> 
#> $reference$doi
#> [1] "10.5281/zenodo.1157934"
#> 
#> 
#> attr(,"class")
#> [1] "contact_survey"
```
