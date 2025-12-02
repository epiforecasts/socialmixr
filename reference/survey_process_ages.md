# Process ages in survey data

This function deals with age ranges and missing data in survey data. It
adjusts the age group breaks to the lower and upper ages in the survey,
and processes contact age ranges and drops missing values. We suggest
you impute missing values prior to using `survey_process_ages`, using
[`survey_impute_ages()`](https://epiforecasts.io/socialmixr/reference/survey_impute_ages.md).

## Usage

``` r
survey_process_ages(
  survey,
  age_limits = NULL,
  missing_participant_age = c("remove", "keep"),
  missing_contact_age = c("remove", "sample", "keep", "ignore")
)
```

## Arguments

- survey:

  a [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object

- age_limits:

  lower limits of the age groups over which to construct the matrix.
  Defaults to NULL. If NULL, it will construct age limits based upon age
  ranges in participants data of the survey.

- missing_participant_age:

  if set to "remove" (default), participants without age information are
  removed; if set to "keep", participants with missing age are kept and
  treated as a separate age group

- missing_contact_age:

  if set to "remove" (default), participants that have contacts without
  age information are removed; if set to "sample", contacts without age
  information are sampled from all the contacts of participants of the
  same age group; if set to "keep", contacts with missing age are kept
  and treated as a separate age group; if set to "ignore", contact with
  missing age are ignored in the contact analysis

## Value

The survey object with processed age data.

## Examples

``` r
polymod_imputed_processed <- polymod |> survey_impute_ages() |> survey_process_ages()
#> Removing participants without age information.
#> ℹ To change this behaviour, set the `missing.participant.age` option.
#> Removing participants that have contacts without age information.
#> ℹ To change this behaviour, set the `missing.contact.age` option.
polymod_imputed_processed
#> $participants
#> Key: <lower.age.limit>
#>       lower.age.limit       hh_id part_id part_gender part_occupation
#>                 <num>      <char>   <int>      <char>           <int>
#>    1:               0  Mo08HH1128    1128           F               1
#>    2:               0  Mo08HH1129    1129           M               6
#>    3:               0  Mo08HH1130    1130           F               6
#>    4:               0  Mo08HH1131    1131           F               5
#>    5:               0  Mo08HH1132    1132           M               1
#>   ---                                                                
#> 2180:              83  Mo08HH3183    3183           M               2
#> 2181:              84 Mo08HH50020   50020           M               2
#> 2182:              84  Mo08HH7175    7175           F               2
#> 2183:              84  Mo08HH7910    7910           M               2
#> 2184:              85  Mo08HH3114    3114           M               2
#>       part_occupation_detail part_education part_education_length
#>                        <int>          <int>                 <int>
#>    1:                      6              3                    13
#>    2:                     NA              2                    12
#>    3:                     NA              2                    12
#>    4:                     NA              2                    12
#>    5:                      6              3                    13
#>   ---                                                            
#> 2180:                     NA              1                     6
#> 2181:                     NA              7                    17
#> 2182:                     NA              1                     9
#> 2183:                     NA              7                    15
#> 2184:                     NA              3                    13
#>       participant_school_year participant_nationality child_care
#>                         <int>                  <char>     <char>
#>    1:                      NA                                  N
#>    2:                      NA                                  N
#>    3:                      NA                                  N
#>    4:                      NA                                  N
#>    5:                      NA                                  Y
#>   ---                                                           
#> 2180:                      NA                      LU           
#> 2181:                      NA                                   
#> 2182:                      NA                                   
#> 2183:                      NA                      BE           
#> 2184:                      NA                      LU           
#>       child_care_detail child_relationship child_nationality problems diary_how
#>                   <int>              <int>            <char>   <char>     <int>
#>    1:                NA                  1                                    1
#>    2:                NA                  1                                    1
#>    3:                NA                  1                                    1
#>    4:                NA                  1                                   NA
#>    5:                NA                  2                                    1
#>   ---                                                                          
#> 2180:                NA                 NA                          N         1
#> 2181:                NA                 NA                                    2
#> 2182:                NA                 NA                                    2
#> 2183:                NA                 NA                          N         1
#> 2184:                NA                 NA                          N         2
#>       diary_missed_unsp diary_missed_skin diary_missed_noskin  sday_id  type
#>                   <int>             <int>               <int>    <int> <int>
#>    1:                NA                NA                  NA 20060704     3
#>    2:                NA                NA                  NA 20060526     3
#>    3:                NA                NA                  NA 20060116     3
#>    4:                NA                NA                  NA 20060522     3
#>    5:                NA                NA                  NA 20060615     3
#>   ---                                                                       
#> 2180:                 1                NA                  NA 20050513     1
#> 2181:                NA                NA                  NA 20060408     1
#> 2182:                 1                NA                  NA 20060411     1
#> 2183:                 1                NA                  NA 20060406     1
#> 2184:                 1                NA                  NA 20050512     1
#>         day month  year dayofweek hh_age_1 hh_age_2 hh_age_3 hh_age_4 hh_age_5
#>       <int> <int> <int>     <int>    <int>    <int>    <int>    <int>    <int>
#>    1:     4     7  2006         2        1       29       32       NA       NA
#>    2:    26     5  2006         5       NA        2        2       22       28
#>    3:    16     1  2006         1        1       18       21       NA       NA
#>    4:    22     5  2006         1        1       12       16       44       46
#>    5:    15     6  2006         4        1       22       33       NA       NA
#>   ---                                                                         
#> 2180:    13     5  2005         5       82       NA       NA       NA       NA
#> 2181:     8     4  2006         6       NA       NA       NA       NA       NA
#> 2182:    11     4  2006         2       NA       NA       NA       NA       NA
#> 2183:     6     4  2006         4       NA       NA       NA       NA       NA
#> 2184:    12     5  2005         4       NA       NA       NA       NA       NA
#>       hh_age_6 hh_age_7 hh_age_8 hh_age_9 hh_age_10 hh_age_11 hh_age_12
#>          <int>    <int>    <int>    <int>     <int>     <int>     <int>
#>    1:       NA       NA       NA       NA        NA        NA        NA
#>    2:       NA       NA       NA       NA        NA        NA        NA
#>    3:       NA       NA       NA       NA        NA        NA        NA
#>    4:       NA       NA       NA       NA        NA        NA        NA
#>    5:       NA       NA       NA       NA        NA        NA        NA
#>   ---                                                                  
#> 2180:       NA       NA       NA       NA        NA        NA        NA
#> 2181:       NA       NA       NA       NA        NA        NA        NA
#> 2182:       NA       NA       NA       NA        NA        NA        NA
#> 2183:       NA       NA       NA       NA        NA        NA        NA
#> 2184:       NA       NA       NA       NA        NA        NA        NA
#>       hh_age_13 hh_age_14 hh_age_15 hh_age_16 hh_age_17 hh_age_18 hh_age_19
#>           <int>     <int>     <int>    <lgcl>    <lgcl>    <lgcl>    <lgcl>
#>    1:        NA        NA        NA        NA        NA        NA        NA
#>    2:        NA        NA        NA        NA        NA        NA        NA
#>    3:        NA        NA        NA        NA        NA        NA        NA
#>    4:        NA        NA        NA        NA        NA        NA        NA
#>    5:        NA        NA        NA        NA        NA        NA        NA
#>   ---                                                                      
#> 2180:        NA        NA        NA        NA        NA        NA        NA
#> 2181:        NA        NA        NA        NA        NA        NA        NA
#> 2182:        NA        NA        NA        NA        NA        NA        NA
#> 2183:        NA        NA        NA        NA        NA        NA        NA
#> 2184:        NA        NA        NA        NA        NA        NA        NA
#>       hh_age_20 class_size    country hh_size part_age_exact part_age age.group
#>          <lgcl>      <int>     <fctr>   <int>          <int>    <int>    <fctr>
#>    1:        NA         NA    Germany       3              0        0     [0,1)
#>    2:        NA         NA    Germany       4              0        0     [0,1)
#>    3:        NA         NA    Germany       3              0        0     [0,1)
#>    4:        NA         NA    Germany       5              0        0     [0,1)
#>    5:        NA          5    Germany       3              0        0     [0,1)
#>   ---                                                                          
#> 2180:        NA         NA Luxembourg       2             83       83   [83,84)
#> 2181:        NA         NA    Finland       1             84       84   [84,85)
#> 2182:        NA         NA    Finland       1             84       84   [84,85)
#> 2183:        NA         NA    Belgium       1             84       84   [84,85)
#> 2184:        NA         NA Luxembourg       1             85       85       85+
#>       upper.age.limit
#>                 <num>
#>    1:               1
#>    2:               1
#>    3:               1
#>    4:               1
#>    5:               1
#>   ---                
#> 2180:              84
#> 2181:              85
#> 2182:              85
#> 2183:              85
#> 2184:              86
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
#>        frequency_multi phys_contact duration_multi cnt_age
#>                  <int>        <int>          <int>   <int>
#>     1:               1            1              4      42
#>     2:               1            1              5       9
#>     3:               3            1              4      NA
#>     4:               1            1              5       8
#>     5:               2            1              3      NA
#>    ---                                                    
#> 97900:               1            1              5      11
#> 97901:               1            1              5       3
#> 97902:               1            1              5      43
#> 97903:               4            1              3      33
#> 97904:               3            1              4      15
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
polymod_processed <- polymod |> survey_process_ages()
#> Removing participants without age information.
#> ℹ To change this behaviour, set the `missing.participant.age` option.
#> Removing participants that have contacts without age information.
#> ℹ To change this behaviour, set the `missing.contact.age` option.
polymod_processed
#> $participants
#> Key: <lower.age.limit>
#>       lower.age.limit       hh_id part_id part_gender part_occupation
#>                 <num>      <char>   <int>      <char>           <int>
#>    1:               0  Mo08HH1128    1128           F               1
#>    2:               0  Mo08HH1129    1129           M               6
#>    3:               0  Mo08HH1130    1130           F               6
#>    4:               0  Mo08HH1131    1131           F               5
#>    5:               0  Mo08HH1132    1132           M               1
#>   ---                                                                
#> 2180:              83  Mo08HH3183    3183           M               2
#> 2181:              84 Mo08HH50020   50020           M               2
#> 2182:              84  Mo08HH7175    7175           F               2
#> 2183:              84  Mo08HH7910    7910           M               2
#> 2184:              85  Mo08HH3114    3114           M               2
#>       part_occupation_detail part_education part_education_length
#>                        <int>          <int>                 <int>
#>    1:                      6              3                    13
#>    2:                     NA              2                    12
#>    3:                     NA              2                    12
#>    4:                     NA              2                    12
#>    5:                      6              3                    13
#>   ---                                                            
#> 2180:                     NA              1                     6
#> 2181:                     NA              7                    17
#> 2182:                     NA              1                     9
#> 2183:                     NA              7                    15
#> 2184:                     NA              3                    13
#>       participant_school_year participant_nationality child_care
#>                         <int>                  <char>     <char>
#>    1:                      NA                                  N
#>    2:                      NA                                  N
#>    3:                      NA                                  N
#>    4:                      NA                                  N
#>    5:                      NA                                  Y
#>   ---                                                           
#> 2180:                      NA                      LU           
#> 2181:                      NA                                   
#> 2182:                      NA                                   
#> 2183:                      NA                      BE           
#> 2184:                      NA                      LU           
#>       child_care_detail child_relationship child_nationality problems diary_how
#>                   <int>              <int>            <char>   <char>     <int>
#>    1:                NA                  1                                    1
#>    2:                NA                  1                                    1
#>    3:                NA                  1                                    1
#>    4:                NA                  1                                   NA
#>    5:                NA                  2                                    1
#>   ---                                                                          
#> 2180:                NA                 NA                          N         1
#> 2181:                NA                 NA                                    2
#> 2182:                NA                 NA                                    2
#> 2183:                NA                 NA                          N         1
#> 2184:                NA                 NA                          N         2
#>       diary_missed_unsp diary_missed_skin diary_missed_noskin  sday_id  type
#>                   <int>             <int>               <int>    <int> <int>
#>    1:                NA                NA                  NA 20060704     3
#>    2:                NA                NA                  NA 20060526     3
#>    3:                NA                NA                  NA 20060116     3
#>    4:                NA                NA                  NA 20060522     3
#>    5:                NA                NA                  NA 20060615     3
#>   ---                                                                       
#> 2180:                 1                NA                  NA 20050513     1
#> 2181:                NA                NA                  NA 20060408     1
#> 2182:                 1                NA                  NA 20060411     1
#> 2183:                 1                NA                  NA 20060406     1
#> 2184:                 1                NA                  NA 20050512     1
#>         day month  year dayofweek hh_age_1 hh_age_2 hh_age_3 hh_age_4 hh_age_5
#>       <int> <int> <int>     <int>    <int>    <int>    <int>    <int>    <int>
#>    1:     4     7  2006         2        1       29       32       NA       NA
#>    2:    26     5  2006         5       NA        2        2       22       28
#>    3:    16     1  2006         1        1       18       21       NA       NA
#>    4:    22     5  2006         1        1       12       16       44       46
#>    5:    15     6  2006         4        1       22       33       NA       NA
#>   ---                                                                         
#> 2180:    13     5  2005         5       82       NA       NA       NA       NA
#> 2181:     8     4  2006         6       NA       NA       NA       NA       NA
#> 2182:    11     4  2006         2       NA       NA       NA       NA       NA
#> 2183:     6     4  2006         4       NA       NA       NA       NA       NA
#> 2184:    12     5  2005         4       NA       NA       NA       NA       NA
#>       hh_age_6 hh_age_7 hh_age_8 hh_age_9 hh_age_10 hh_age_11 hh_age_12
#>          <int>    <int>    <int>    <int>     <int>     <int>     <int>
#>    1:       NA       NA       NA       NA        NA        NA        NA
#>    2:       NA       NA       NA       NA        NA        NA        NA
#>    3:       NA       NA       NA       NA        NA        NA        NA
#>    4:       NA       NA       NA       NA        NA        NA        NA
#>    5:       NA       NA       NA       NA        NA        NA        NA
#>   ---                                                                  
#> 2180:       NA       NA       NA       NA        NA        NA        NA
#> 2181:       NA       NA       NA       NA        NA        NA        NA
#> 2182:       NA       NA       NA       NA        NA        NA        NA
#> 2183:       NA       NA       NA       NA        NA        NA        NA
#> 2184:       NA       NA       NA       NA        NA        NA        NA
#>       hh_age_13 hh_age_14 hh_age_15 hh_age_16 hh_age_17 hh_age_18 hh_age_19
#>           <int>     <int>     <int>    <lgcl>    <lgcl>    <lgcl>    <lgcl>
#>    1:        NA        NA        NA        NA        NA        NA        NA
#>    2:        NA        NA        NA        NA        NA        NA        NA
#>    3:        NA        NA        NA        NA        NA        NA        NA
#>    4:        NA        NA        NA        NA        NA        NA        NA
#>    5:        NA        NA        NA        NA        NA        NA        NA
#>   ---                                                                      
#> 2180:        NA        NA        NA        NA        NA        NA        NA
#> 2181:        NA        NA        NA        NA        NA        NA        NA
#> 2182:        NA        NA        NA        NA        NA        NA        NA
#> 2183:        NA        NA        NA        NA        NA        NA        NA
#> 2184:        NA        NA        NA        NA        NA        NA        NA
#>       hh_age_20 class_size    country hh_size part_age_exact part_age age.group
#>          <lgcl>      <int>     <fctr>   <int>          <int>    <int>    <fctr>
#>    1:        NA         NA    Germany       3              0        0     [0,1)
#>    2:        NA         NA    Germany       4              0        0     [0,1)
#>    3:        NA         NA    Germany       3              0        0     [0,1)
#>    4:        NA         NA    Germany       5              0        0     [0,1)
#>    5:        NA          5    Germany       3              0        0     [0,1)
#>   ---                                                                          
#> 2180:        NA         NA Luxembourg       2             83       83   [83,84)
#> 2181:        NA         NA    Finland       1             84       84   [84,85)
#> 2182:        NA         NA    Finland       1             84       84   [84,85)
#> 2183:        NA         NA    Belgium       1             84       84   [84,85)
#> 2184:        NA         NA Luxembourg       1             85       85       85+
#>       upper.age.limit
#>                 <num>
#>    1:               1
#>    2:               1
#>    3:               1
#>    4:               1
#>    5:               1
#>   ---                
#> 2180:              84
#> 2181:              85
#> 2182:              85
#> 2183:              85
#> 2184:              86
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
#>        frequency_multi phys_contact duration_multi cnt_age
#>                  <int>        <int>          <int>   <int>
#>     1:               1            1              4      42
#>     2:               1            1              5       9
#>     3:               3            1              4      NA
#>     4:               1            1              5       8
#>     5:               2            1              3      NA
#>    ---                                                    
#> 97900:               1            1              5      11
#> 97901:               1            1              5       3
#> 97902:               1            1              5      43
#> 97903:               4            1              3      33
#> 97904:               3            1              4      15
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
