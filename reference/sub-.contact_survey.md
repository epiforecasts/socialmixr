# Subset a contact survey

Filters a `contact_survey` object using an expression. The expression is
evaluated against whichever table(s) contain the referenced columns
(participants, contacts, or both). When participants are filtered,
contacts are automatically pruned to matching `part_id`s.

## Usage

``` r
# S3 method for class 'contact_survey'
x[i, ...]
```

## Arguments

- x:

  a `contact_survey` object

- i:

  an expression to evaluate as a row filter (e.g.
  `country == "United Kingdom"`)

- ...:

  ignored

## Value

a filtered `contact_survey` object

## Examples

``` r
data(polymod)
polymod[country == "United Kingdom"]
#> $participants
#> Key: <hh_id>
#>            hh_id part_id part_gender part_occupation part_occupation_detail
#>           <char>   <int>      <char>           <int>                  <int>
#>    1: Mo08HH4517    4517           M               5                      4
#>    2: Mo08HH4518    4518           F               5                     NA
#>    3: Mo08HH4519    4519           F               5                      2
#>    4: Mo08HH4520    4520           F               5                      4
#>    5: Mo08HH4521    4521           M               5                      3
#>   ---                                                                      
#> 1008: Mo08HH5518    5518           M              NA                     NA
#> 1009: Mo08HH5519    5519           F              NA                     NA
#> 1010: Mo08HH5520    5520           F              NA                     NA
#> 1011: Mo08HH5521    5521           F               4                     NA
#> 1012: Mo08HH5522    5522           M              NA                     NA
#>       part_education part_education_length participant_school_year
#>                <int>                 <int>                   <int>
#>    1:              4                    13                      NA
#>    2:              4                    13                      NA
#>    3:              4                    13                      NA
#>    4:              4                    13                      NA
#>    5:              4                    13                      NA
#>   ---                                                             
#> 1008:              4                    13                      NA
#> 1009:              4                    13                      NA
#> 1010:              4                    13                      NA
#> 1011:              5                    16                      NA
#> 1012:              4                    13                      NA
#>       participant_nationality child_care child_care_detail child_relationship
#>                        <char>     <char>             <int>              <int>
#>    1:                      UK          Y                NA                  1
#>    2:                      UK          Y                NA                  1
#>    3:                      UK          Y                NA                  1
#>    4:                      UK          Y                NA                  1
#>    5:                      UK          Y                NA                  2
#>   ---                                                                        
#> 1008:                      OT                           NA                 NA
#> 1009:                      OT                           NA                 NA
#> 1010:                      OT                           NA                 NA
#> 1011:                      UK                           NA                 NA
#> 1012:                      OT                           NA                 NA
#>       child_nationality problems diary_how diary_missed_unsp diary_missed_skin
#>                  <char>   <char>     <int>             <int>             <int>
#>    1:                UK                 NA                 1                 1
#>    2:                UK                 NA                 3                 1
#>    3:                UK                 NA                 1                 1
#>    4:                UK                 NA                 1                 1
#>    5:                UK                 NA                 1                 1
#>   ---                                                                         
#> 1008:                                   NA                 1                 1
#> 1009:                                   NA                 1                 1
#> 1010:                                   NA                 1                 1
#> 1011:                                   NA                 1                 1
#> 1012:                                   NA                 1                 1
#>       diary_missed_noskin  sday_id  type   day month  year dayofweek hh_age_1
#>                     <int>    <int> <int> <int> <int> <int>     <int>    <int>
#>    1:                   1 20060420     3    20     4  2006         4        5
#>    2:                   3 20060420     3    20     4  2006         4        0
#>    3:                   1 20060420     3    20     4  2006         4        6
#>    4:                   1 20060422     3    22     4  2006         6        3
#>    5:                   1 20060422     3    22     4  2006         6        1
#>   ---                                                                        
#> 1008:                   1 20060512     1    12     5  2006         5       43
#> 1009:                   1 20060512     1    12     5  2006         5       57
#> 1010:                   1 20060512     1    12     5  2006         5       19
#> 1011:                   1 20060512     1    12     5  2006         5        0
#> 1012:                   1 20060512     1    12     5  2006         5       35
#>       hh_age_2 hh_age_3 hh_age_4 hh_age_5 hh_age_6 hh_age_7 hh_age_8 hh_age_9
#>          <int>    <int>    <int>    <int>    <int>    <int>    <int>    <int>
#>    1:        7       29       31       NA       NA       NA       NA       NA
#>    2:        5       30       30       NA       NA       NA       NA       NA
#>    3:       28       31       NA       NA       NA       NA       NA       NA
#>    4:       33       NA       NA       NA       NA       NA       NA       NA
#>    5:        2       25       30       NA       NA       NA       NA       NA
#>   ---                                                                        
#> 1008:       50       NA       NA       NA       NA       NA       NA       NA
#> 1009:       64       NA       NA       NA       NA       NA       NA       NA
#> 1010:       27       52       55       NA       NA       NA       NA       NA
#> 1011:        3       34       40       NA       NA       NA       NA       NA
#> 1012:       39       NA       NA       NA       NA       NA       NA       NA
#>       hh_age_10 hh_age_11 hh_age_12 hh_age_13 hh_age_14 hh_age_15 hh_age_16
#>           <int>     <int>     <int>     <int>     <int>     <int>    <lgcl>
#>    1:        NA        NA        NA        NA        NA        NA        NA
#>    2:        NA        NA        NA        NA        NA        NA        NA
#>    3:        NA        NA        NA        NA        NA        NA        NA
#>    4:        NA        NA        NA        NA        NA        NA        NA
#>    5:        NA        NA        NA        NA        NA        NA        NA
#>   ---                                                                      
#> 1008:        NA        NA        NA        NA        NA        NA        NA
#> 1009:        NA        NA        NA        NA        NA        NA        NA
#> 1010:        NA        NA        NA        NA        NA        NA        NA
#> 1011:        NA        NA        NA        NA        NA        NA        NA
#> 1012:        NA        NA        NA        NA        NA        NA        NA
#>       hh_age_17 hh_age_18 hh_age_19 hh_age_20 class_size        country hh_size
#>          <lgcl>    <lgcl>    <lgcl>    <lgcl>      <int>         <fctr>   <int>
#>    1:        NA        NA        NA        NA         NA United Kingdom       4
#>    2:        NA        NA        NA        NA         NA United Kingdom       4
#>    3:        NA        NA        NA        NA         NA United Kingdom       3
#>    4:        NA        NA        NA        NA         NA United Kingdom       2
#>    5:        NA        NA        NA        NA         NA United Kingdom       4
#>   ---                                                                          
#> 1008:        NA        NA        NA        NA         NA United Kingdom       2
#> 1009:        NA        NA        NA        NA         NA United Kingdom       2
#> 1010:        NA        NA        NA        NA         NA United Kingdom       4
#> 1011:        NA        NA        NA        NA         NA United Kingdom       4
#> 1012:        NA        NA        NA        NA         NA United Kingdom       2
#>       part_age_exact
#>                <int>
#>    1:              5
#>    2:              5
#>    3:              6
#>    4:              3
#>    5:              2
#>   ---               
#> 1008:             50
#> 1009:             57
#> 1010:             52
#> 1011:             34
#> 1012:             39
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
#>        frequency_multi phys_contact duration_multi
#>                  <int>        <int>          <int>
#>     1:               1            1              4
#>     2:               2            2              2
#>     3:               1            1              5
#>     4:               2            2              2
#>     5:               1            1              4
#>    ---                                            
#> 11872:               1            2              1
#> 11873:               1            1              5
#> 11874:               2            1              3
#> 11875:               2            2              3
#> 11876:               1            1              4
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
