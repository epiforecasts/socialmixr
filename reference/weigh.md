# Weigh survey participants

Applies weights to participants in a `contact_survey` object. Weights
are always multiplied into an existing `weight` column (or one is
created with value 1), making multiple calls composable.

The behaviour depends on the combination of arguments:

- `target = NULL`:

  Numeric column: multiply `weight` by column values directly.

- Unnamed `target` + `groups`:

  Map column values to groups, assign `target[g] / n_in_group` per
  participant.

- Named `target`:

  Names match column values, assign `target[val] / n_with_val` per
  participant.

- Data frame `target`:

  Post-stratify against population data (expanded to single-year ages
  via
  [`pop_age()`](https://epiforecasts.io/socialmixr/reference/pop_age.md)).

## Usage

``` r
weigh(survey, by, target = NULL, groups = NULL, ...)
```

## Arguments

- survey:

  a [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object (must have been processed by
  [`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md)
  if using data frame target)

- by:

  column name in the participant data to weigh by

- target:

  target weights: `NULL` for direct numeric weighting, an unnamed
  numeric vector (with `groups`), a named numeric vector, or a data
  frame with columns `lower.age.limit` and `population`

- groups:

  a list of value sets mapping column values to groups (used with
  unnamed `target` vector); must be the same length as `target`

- ...:

  further arguments passed to
  [`pop_age()`](https://epiforecasts.io/socialmixr/reference/pop_age.md)
  when `target` is a data frame

## Value

the survey object with updated participant weights

## Examples

``` r
data(polymod)
# Direct numeric weighting
if ("survey_weight" %in% names(polymod$participants)) {
  polymod |> weigh("survey_weight")
}

# Dayofweek weighting with groups (POLYMOD uses 0 = Sunday, 6 = Saturday)
polymod |>
  weigh("dayofweek", target = c(5, 2), groups = list(1:5, c(0, 6)))
#> $participants
#> Key: <hh_id>
#>            hh_id part_id part_gender part_occupation part_occupation_detail
#>           <char>   <int>      <char>           <int>                  <int>
#>    1:    Mo08HH1       1           F               5                      7
#>    2:   Mo08HH10      10           F               6                      7
#>    3:  Mo08HH100     100           M               5                     NA
#>    4: Mo08HH1000    1000           F               1                      8
#>    5: Mo08HH1001    1001           M               1                      8
#>   ---                                                                      
#> 7286:  Mo08HH995     995           M               4                      8
#> 7287:  Mo08HH996     996           F               3                      6
#> 7288:  Mo08HH997     997           F               5                     NA
#> 7289:  Mo08HH998     998           M               4                      7
#> 7290:  Mo08HH999     999           M               1                      7
#>       part_education part_education_length participant_school_year
#>                <int>                 <int>                   <int>
#>    1:              3                    13                      NA
#>    2:              5                    16                      NA
#>    3:              6                    18                      NA
#>    4:              2                    12                      NA
#>    5:              2                    12                      NA
#>   ---                                                             
#> 7286:              2                    12                      NA
#> 7287:              2                    12                      NA
#> 7288:              1                    10                      NA
#> 7289:              2                    12                      NA
#> 7290:              2                    12                      NA
#>       participant_nationality child_care child_care_detail child_relationship
#>                        <char>     <char>             <int>              <int>
#>    1:                      IT          Y                NA                  2
#>    2:                      IT          N                NA                  1
#>    3:                      IT          Y                NA                  1
#>    4:                                  Y                NA                 NA
#>    5:                                  Y                NA                  2
#>   ---                                                                        
#> 7286:                                  Y                NA                  1
#> 7287:                                  Y                NA                  1
#> 7288:                                  Y                NA                  1
#> 7289:                                  Y                NA                  1
#> 7290:                                  Y                NA                  2
#>       child_nationality problems diary_how diary_missed_unsp diary_missed_skin
#>                  <char>   <char>     <int>             <int>             <int>
#>    1:                IT        N         2                 2                NA
#>    2:                IT        N         1                 1                NA
#>    3:                IT        N         2                 1                NA
#>    4:                                    1                NA                NA
#>    5:                                   NA                NA                NA
#>   ---                                                                         
#> 7286:                                   NA                NA                NA
#> 7287:                                    1                NA                NA
#> 7288:                                    1                NA                NA
#> 7289:                                    1                NA                NA
#> 7290:                                    1                NA                NA
#>       diary_missed_noskin  sday_id  type   day month  year dayofweek hh_age_1
#>                     <int>    <int> <int> <int> <int> <int>     <int>    <int>
#>    1:                  NA 20060525     3    25     5  2006         4        8
#>    2:                  NA 20060525     3    25     5  2006         4        0
#>    3:                  NA 20060526     3    26     5  2006         5        2
#>    4:                  NA 20060612     3    12     6  2006         1        7
#>    5:                  NA 20060522     3    22     5  2006         1        7
#>   ---                                                                        
#> 7286:                  NA 20060618     3    18     6  2006         0        8
#> 7287:                  NA 20060117     3    17     1  2006         2        7
#> 7288:                  NA 20060618     3    18     6  2006         0        7
#> 7289:                  NA 20060706     3     6     7  2006         4        7
#> 7290:                  NA 20060612     3    12     6  2006         1        1
#>       hh_age_2 hh_age_3 hh_age_4 hh_age_5 hh_age_6 hh_age_7 hh_age_8 hh_age_9
#>          <int>    <int>    <int>    <int>    <int>    <int>    <int>    <int>
#>    1:       42       NA       NA       NA       NA       NA       NA       NA
#>    2:        3       40       NA       NA       NA       NA       NA       NA
#>    3:        5        8       44       NA       NA       NA       NA       NA
#>    4:       32       NA       NA       NA       NA       NA       NA       NA
#>    5:       14       37       41       NA       NA       NA       NA       NA
#>   ---                                                                        
#> 7286:       14       15       44       NA       NA       NA       NA       NA
#> 7287:       16       40       NA       NA       NA       NA       NA       NA
#> 7288:       11       15       40       44       NA       NA       NA       NA
#> 7289:       22       48       50       NA       NA       NA       NA       NA
#> 7290:        7       25       29       NA       NA       NA       NA       NA
#>       hh_age_10 hh_age_11 hh_age_12 hh_age_13 hh_age_14 hh_age_15 hh_age_16
#>           <int>     <int>     <int>     <int>     <int>     <int>    <lgcl>
#>    1:        NA        NA        NA        NA        NA        NA        NA
#>    2:        NA        NA        NA        NA        NA        NA        NA
#>    3:        NA        NA        NA        NA        NA        NA        NA
#>    4:        NA        NA        NA        NA        NA        NA        NA
#>    5:        NA        NA        NA        NA        NA        NA        NA
#>   ---                                                                      
#> 7286:        NA        NA        NA        NA        NA        NA        NA
#> 7287:        NA        NA        NA        NA        NA        NA        NA
#> 7288:        NA        NA        NA        NA        NA        NA        NA
#> 7289:        NA        NA        NA        NA        NA        NA        NA
#> 7290:        NA        NA        NA        NA        NA        NA        NA
#>       hh_age_17 hh_age_18 hh_age_19 hh_age_20 class_size country hh_size
#>          <lgcl>    <lgcl>    <lgcl>    <lgcl>      <int>  <fctr>   <int>
#>    1:        NA        NA        NA        NA         NA   Italy       3
#>    2:        NA        NA        NA        NA         NA   Italy       4
#>    3:        NA        NA        NA        NA         NA   Italy       5
#>    4:        NA        NA        NA        NA         22 Germany       2
#>    5:        NA        NA        NA        NA         22 Germany       4
#>   ---                                                                   
#> 7286:        NA        NA        NA        NA         15 Germany       4
#> 7287:        NA        NA        NA        NA          9 Germany       3
#> 7288:        NA        NA        NA        NA         28 Germany       5
#> 7289:        NA        NA        NA        NA         21 Germany       4
#> 7290:        NA        NA        NA        NA         30 Germany       4
#>       part_age_exact       weight
#>                <int>        <num>
#>    1:              8 0.0009335325
#>    2:              3 0.0009335325
#>    3:              5 0.0009335325
#>    4:              7 0.0009335325
#>    5:              7 0.0009335325
#>   ---                            
#> 7286:              7 0.0011123471
#> 7287:              7 0.0009335325
#> 7288:              7 0.0011123471
#> 7289:              7 0.0009335325
#> 7290:              7 0.0009335325
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
#>        frequency_multi phys_contact duration_multi
#>                  <int>        <int>          <int>
#>     1:               1            1              4
#>     2:               1            1              5
#>     3:               3            1              4
#>     4:               1            1              5
#>     5:               2            1              3
#>    ---                                            
#> 97900:               1            1              5
#> 97901:               1            1              5
#> 97902:               1            1              5
#> 97903:               4            1              3
#> 97904:               3            1              4
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
