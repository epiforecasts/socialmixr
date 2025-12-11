# Assign age groups in survey data

This function processes age data in a survey object. It imputes ages
from ranges, handles missing values, and assigns age groups.

## Usage

``` r
assign_age_groups(
  survey,
  age_limits = NULL,
  estimated_participant_age = c("mean", "sample", "missing"),
  estimated_contact_age = c("mean", "sample", "missing"),
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
  Defaults to NULL. If NULL, age limits are inferred from participant
  and contact ages.

- estimated_participant_age:

  if set to "mean" (default), people whose ages are given as a range (in
  columns named "...\_est_min" and "...\_est_max") but not exactly (in a
  column named "...\_exact") will have their age set to the mid-point of
  the range; if set to "sample", the age will be sampled from the range;
  if set to "missing", age ranges will be treated as missing

- estimated_contact_age:

  if set to "mean" (default), contacts whose ages are given as a range
  (in columns named "...\_est_min" and "...\_est_max") but not exactly
  (in a column named "...\_exact") will have their age set to the
  mid-point of the range; if set to "sample", the age will be sampled
  from the range; if set to "missing", age ranges will be treated as
  missing

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
polymod_grouped <- polymod |> assign_age_groups()
polymod_grouped
#> $participants
#> Key: <lower.age.limit>
#>       lower.age.limit      hh_id part_id part_gender part_occupation
#>                 <num>     <char>   <int>      <char>           <int>
#>    1:               0 Mo08HH1127    1127           M               1
#>    2:               0 Mo08HH1128    1128           F               1
#>    3:               0 Mo08HH1129    1129           M               6
#>    4:               0 Mo08HH1130    1130           F               6
#>    5:               0 Mo08HH1131    1131           F               5
#>   ---                                                               
#> 7194:              84 Mo08HH7910    7910           M               2
#> 7195:              85 Mo08HH1155    1155           F               2
#> 7196:              85 Mo08HH1367    1367           M               2
#> 7197:              85 Mo08HH3114    3114           M               2
#> 7198:              90 Mo08HH3743    3743           M               2
#>       part_occupation_detail part_education part_education_length
#>                        <int>          <int>                 <int>
#>    1:                      8              2                    12
#>    2:                      6              3                    13
#>    3:                     NA              2                    12
#>    4:                     NA              2                    12
#>    5:                     NA              2                    12
#>   ---                                                            
#> 7194:                     NA              7                    15
#> 7195:                      6              8                    10
#> 7196:                      7              8                    10
#> 7197:                     NA              3                    13
#> 7198:                     NA              1                     8
#>       participant_school_year participant_nationality child_care
#>                         <int>                  <char>     <char>
#>    1:                      NA                                  N
#>    2:                      NA                                  N
#>    3:                      NA                                  N
#>    4:                      NA                                  N
#>    5:                      NA                                  N
#>   ---                                                           
#> 7194:                      NA                      BE           
#> 7195:                      NA                                   
#> 7196:                      NA                                   
#> 7197:                      NA                      LU           
#> 7198:                      NA                      PL           
#>       child_care_detail child_relationship child_nationality problems diary_how
#>                   <int>              <int>            <char>   <char>     <int>
#>    1:                NA                  2                                    1
#>    2:                NA                  1                                    1
#>    3:                NA                  1                                    1
#>    4:                NA                  1                                    1
#>    5:                NA                  1                                   NA
#>   ---                                                                          
#> 7194:                NA                 NA                          N         1
#> 7195:                NA                 NA                                    1
#> 7196:                NA                 NA                                    1
#> 7197:                NA                 NA                          N         2
#> 7198:                NA                 NA                          N         2
#>       diary_missed_unsp diary_missed_skin diary_missed_noskin  sday_id  type
#>                   <int>             <int>               <int>    <int> <int>
#>    1:                NA                NA                  NA 20060206     3
#>    2:                NA                NA                  NA 20060704     3
#>    3:                NA                NA                  NA 20060526     3
#>    4:                NA                NA                  NA 20060116     3
#>    5:                NA                NA                  NA 20060522     3
#>   ---                                                                       
#> 7194:                 1                NA                  NA 20060406     1
#> 7195:                NA                NA                  NA 20060117     1
#> 7196:                NA                NA                  NA 20060119     1
#> 7197:                 1                NA                  NA 20050512     1
#> 7198:                 1                NA                  NA 20060329     1
#>         day month  year dayofweek hh_age_1 hh_age_2 hh_age_3 hh_age_4 hh_age_5
#>       <int> <int> <int>     <int>    <int>    <int>    <int>    <int>    <int>
#>    1:     6     2  2006         1        1       21       31       NA       NA
#>    2:     4     7  2006         2        1       29       32       NA       NA
#>    3:    26     5  2006         5       NA        2        2       22       28
#>    4:    16     1  2006         1        1       18       21       NA       NA
#>    5:    22     5  2006         1        1       12       16       44       46
#>   ---                                                                         
#> 7194:     6     4  2006         4       NA       NA       NA       NA       NA
#> 7195:    17     1  2006         2       85       NA       NA       NA       NA
#> 7196:    19     1  2006         4       82       85       NA       NA       NA
#> 7197:    12     5  2005         4       NA       NA       NA       NA       NA
#> 7198:    29     3  2006         3       23       26       59       61       NA
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
#>       hh_age_20 class_size    country hh_size part_age_exact part_age age.group
#>          <lgcl>      <int>     <fctr>   <int>          <int>    <int>    <fctr>
#>    1:        NA         NA    Germany       3              0        0     [0,1)
#>    2:        NA         NA    Germany       3              0        0     [0,1)
#>    3:        NA         NA    Germany       4              0        0     [0,1)
#>    4:        NA         NA    Germany       3              0        0     [0,1)
#>    5:        NA         NA    Germany       5              0        0     [0,1)
#>   ---                                                                          
#> 7194:        NA         NA    Belgium       1             84       84   [84,85)
#> 7195:        NA         NA    Germany       1             85       85   [85,86)
#> 7196:        NA         NA    Germany       2             85       85   [85,86)
#> 7197:        NA         NA Luxembourg       1             85       85   [85,86)
#> 7198:        NA         NA     Poland       5             90       90   [90,91)
#>       upper.age.limit
#>                 <num>
#>    1:               1
#>    2:               1
#>    3:               1
#>    4:               1
#>    5:               1
#>   ---                
#> 7194:              85
#> 7195:              86
#> 7196:              86
#> 7197:              86
#> 7198:              91
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
#>     3:               3            1              4      42
#>     4:               1            1              5       8
#>     5:               2            1              3      29
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
polymod_custom <- polymod |> assign_age_groups(age_limits = c(0, 5, 10, 15))
polymod_custom
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
#> 7194:        NA         26 Germany       3             15       15       15+
#> 7195:        NA         NA Germany       3             15       15       15+
#> 7196:        NA         25 Germany       4             15       15       15+
#> 7197:        NA         17 Germany       3             15       15       15+
#> 7198:        NA         32 Germany       4             15       15       15+
#>       upper.age.limit
#>                 <num>
#>    1:               5
#>    2:               5
#>    3:               5
#>    4:               5
#>    5:               5
#>   ---                
#> 7194:              91
#> 7195:              91
#> 7196:              91
#> 7197:              91
#> 7198:              91
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
#>     3:               3            1              4      42
#>     4:               1            1              5       8
#>     5:               2            1              3      29
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
