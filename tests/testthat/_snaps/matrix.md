# contact matrices look as expected

    Code
      contacts
    Output
      $test1
      $test1$matrix
                contact.age.group
      age.group       [0,5)     [5,10)    [10,15)    [15,20)     [20,25)    [25,30)
        [0,5)    178.966087  88.880184  45.354727  30.972284  42.6051270  74.733934
        [5,10)    66.232016 668.044656 134.274185  35.203096  31.4355346  66.434003
        [10,15)   42.023723 111.436616 688.396514 103.714503  22.1280347  44.956109
        [15,20)   24.673006  78.346794 158.510591 700.280260  97.5761783  77.930858
        [20,25)   26.973497  35.707872  15.991597  93.248381 152.7117404  76.040717
        [25,30)   43.200207  43.480995  18.063760  42.873620  88.9892562 107.950930
        [30,35)   40.425052  56.416277  28.910736  24.861596  45.4570091  57.681248
        [35,40)   57.413396  96.817442  53.334831  39.057969  44.1828453  49.687374
        [40,45)   14.841394  55.860697  61.956371  53.157270  47.6076053  45.907692
        [45,50)   12.023640   8.993476  37.232448  63.569833  48.1737784  46.208130
        [50,55)   24.897017  19.732155  21.398703  46.303262  58.2690366  58.172490
        [55,60)   11.463500  12.454714  24.118950  16.234386  33.0913231  46.499652
        [60,65)   13.095699  32.832242  17.740504  13.131377  35.0665384  43.906001
        [65,70)    7.009383  13.174960  11.319417  13.157194  10.0526235  19.717793
        [70,75)    3.046779   2.949233   8.945245  15.924334   9.6280647   9.140336
        [75,80)    0.000000   2.137842   1.068921   3.110274   0.9724316   1.068921
        [80,Inf)   0.000000   0.000000   0.000000   0.000000   0.0000000   0.000000
                contact.age.group
      age.group     [30,35)    [35,40)    [40,45)     [45,50)   [50,55)   [55,60)
        [0,5)     91.708976  95.882802  51.956159  27.5221001 30.779454 29.361522
        [5,10)   108.803439 100.314642 102.291590  54.9530408 38.811919 20.808874
        [10,15)   63.258912 127.846712 115.751398  58.3753992 40.509099 25.514605
        [15,20)   52.068678 113.778963  98.589173  81.3480282 42.672234 34.694509
        [20,25)   51.094350  44.650806  43.261966  57.6368055 25.722105 23.020902
        [25,30)   69.506069  55.360694  51.309724  54.4364839 49.972141 30.754586
        [30,35)   99.809224  91.659424  48.732356  34.1893655 35.866531 40.608316
        [35,40)   61.866181 104.774182  86.462595  55.7946573 42.695162 37.111034
        [40,45)   61.971992  81.658609  84.170188  82.6206655 43.740523 33.866735
        [45,50)   49.432321  59.367926  69.586529 102.0096136 51.387754 27.667104
        [50,55)   61.170361  55.261460  58.258871  39.5785652 48.739562 54.806600
        [55,60)   33.424551  36.774400  36.345039  42.6153774 56.419796 62.962663
        [60,65)   49.981599  66.335051  61.743763  40.0468592 38.810582 55.694066
        [65,70)   17.141104  26.737375  22.039887  15.8129795 14.819010 22.646402
        [70,75)    4.029856   2.949233  20.149282  15.9243343 10.025868  7.962167
        [75,80)    3.206763   3.110274   4.082705   0.9724316  7.096489  2.041353
        [80,Inf)   0.000000   0.000000   0.000000   0.0000000  0.000000  0.000000
                contact.age.group
      age.group    [60,65)   [65,70)   [70,75)   [75,80)  [80,Inf)
        [0,5)    24.677783  8.727480  9.909679  2.993785  0.000000
        [5,10)   25.803873 11.048926  6.071756  4.959343  4.094807
        [10,15)  18.924121 11.851619 10.258733  7.282992  3.982674
        [15,20)  24.902552 21.186852  4.887084  4.966279  1.074401
        [20,25)  11.156829 11.025208  4.056875  8.113749  2.037269
        [25,30)  20.230725 13.096621  5.962517  0.000000  4.007345
        [30,35)  23.553124  7.798386  5.067440  3.117843  1.071522
        [35,40)  27.499796 20.825517 15.028551  8.346657  6.186883
        [40,45)  29.207953 14.039942 12.903832  7.134775 10.833673
        [45,50)  29.816565  6.941030 12.314681 15.053803 12.314681
        [50,55)  26.754035 13.911797 17.707294  5.995743 10.975055
        [55,60)  41.949384 15.278607 10.217936  7.007663 23.503946
        [60,65)  43.217595 23.798264 13.034119  6.958521 10.976500
        [65,70)  22.812626 19.057977  9.023120  3.025473  8.003353
        [70,75)  11.876586 17.988035 12.780010 13.958179  4.915388
        [75,80)   5.151626  0.000000  1.068921  3.986216  1.944863
        [80,Inf)  0.000000  0.000000  0.000000  0.000000  0.000000
      
      $test1$participants
          age.group participants  proportion
             <char>        <int>       <num>
       1:     [0,5)           95 0.093966370
       2:    [5,10)          102 0.100890208
       3:   [10,15)          102 0.100890208
       4:   [15,20)          105 0.103857567
       5:   [20,25)           59 0.058358061
       6:   [25,30)           59 0.058358061
       7:   [30,35)           60 0.059347181
       8:   [35,40)           70 0.069238378
       9:   [40,45)           62 0.061325420
      10:   [45,50)           55 0.054401583
      11:   [50,55)           66 0.065281899
      12:   [55,60)           54 0.053412463
      13:   [60,65)           66 0.065281899
      14:   [65,70)           27 0.026706231
      15:   [70,75)           22 0.021760633
      16:   [75,80)            7 0.006923838
      17:  [80,Inf)            0 0.000000000
      
      
      $test2
      $test2$matrix
               contact.age.group
      age.group     [0,5)   [5,Inf)
        [0,5)   1.5429448  7.027931
        [5,Inf) 0.3673316 13.474102
      
      $test2$demography
         age.group population proportion  year
            <char>      <num>      <num> <int>
      1:     [0,5)   13498647 0.04967121  2005
      2:   [5,Inf)  258261321 0.95032879  2005
      
      $test2$participants
         age.group participants proportion
            <char>        <int>      <num>
      1:     [0,5)          654  0.0908712
      2:   [5,Inf)         6543  0.9091288
      
      
      $test3
      $test3$mean.contacts
      [1] 3.887154
      
      $test3$normalisation
      [1] 1.021165
      
      $test3$contacts
      [1] 1.0973188 1.4624132 0.9343448
      
      $test3$matrix
                contact.age.group
      age.group     [0,5)   [5,10)  [10,Inf)
        [0,5)    1.769367 1.836114 0.8815070
        [5,10)   1.377724 2.468087 0.8621040
        [10,Inf) 1.035265 1.349344 0.9711241
      
      $test3$demography
         age.group population proportion  year
            <char>      <num>      <num> <int>
      1:     [0,5)    1271578 0.06282841  2005
      2:    [5,10)    1329536 0.06569210  2005
      3:  [10,Inf)   17637787 0.87147948  2005
      
      $test3$participants
         age.group participants proportion
            <char>        <int>      <num>
      1:     [0,5)           95 0.09396637
      2:    [5,10)          102 0.10089021
      3:  [10,Inf)          814 0.80514342
      
      
      $test4
      $test4$matrix
                contact.age.group
      age.group      [0,5)   [5,15)  [15,Inf)
        [0,5)    0.0000000 2.029545  3.077273
        [5,15)   0.9262205 7.568413 27.364233
        [15,Inf) 0.1816446 3.539355  7.139665
      
      $test4$demography
         age.group population proportion  year
            <char>      <num>      <num> <int>
      1:     [0,5)   13498647 0.04967121  2005
      2:    [5,15)   29578398 0.10884016  2005
      3:  [15,Inf)  228682923 0.84148863  2005
      
      $test4$participants
         age.group participants proportion
            <char>        <int>      <num>
      1:     [0,5)          660 0.09134948
      2:    [5,15)         1374 0.19017301
      3:  [15,Inf)         5191 0.71847751
      
      

# survey argument is validated

    Code
      contact_matrix(survey = "bogus")
    Condition
      Error in `contact_matrix()`:
      ! `survey` must be a survey object (created using `survey()` or `get_survey()`).

# error is thrown if age limits are non-numeric

    Code
      contact_matrix(survey = polymod, age_limits = c(0, 5, "fifteen"))
    Condition
      Warning in `check_age_limits_increasing()`:
      NAs introduced by coercion
      Error in `contact_matrix()`:
      ! `<int>` must be an increasing integer vector of lower age limits.
      i We see: 0, 5, and NA

# error is thrown if country is not found

    Code
      contact_matrix(survey = polymod, countries = c("Italy", "Zamonia"))
    Condition
      Error in `flexible_countrycode()`:
      ! Survey data not found for: "Zamonia".

# warning is thrown if filter column is not found

    Filter column: "test" not found.

# warning is thrown if missing data exist

    `wpp_age()` was deprecated in socialmixr 0.6.0.
    Pass population data directly via the {.arg survey_pop} argument instead.
    i The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from GitHub for more recent data.

# error is thrown if an unknown argument is passed

    Code
      contact_matrix(dummy = "test")
    Condition
      Error in `check_arg_dots_in()`:
      ! Unknown argument: "dummy".

# error is thrown if invalid age limits are passed

    Code
      contact_matrix(survey = polymod, age_limits = c(13, 11))
    Condition
      Error in `contact_matrix()`:
      ! `<int>` must be an increasing integer vector of lower age limits.
      i We see: 13 and 11

# error if no participants after selecting the country

    Code
      contact_matrix(survey = polymod, countries = "Romania")
    Condition
      Error in `filter_countries()`:
      ! No participants left after selecting countries: "Romania"

# warning if population needed but no 'year' column

    `wpp_age()` was deprecated in socialmixr 0.6.0.
    Pass population data directly via the {.arg survey_pop} argument instead.
    i The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from GitHub for more recent data.

# warning if day of week weigh requested but not present

    `weigh_dayofweek` is "TRUE", but no `dayofweek` column in the data.
    i Will ignore.

# warning is thrown if country has no survey population

    Code
      contact_matrix(survey = polymod5, symmetric = TRUE)
    Condition
      Warning:
      `wpp_age()` was deprecated in socialmixr 0.6.0.
      Pass population data directly via the {.arg survey_pop} argument instead.
      i The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from GitHub for more recent data.
      Error in `survey_pop_from_countries()`:
      ! Could not find population data for: "Zamonia".
      i Pass population data directly via the `survey_pop` argument.

# warning is thrown if contact survey has no age information

    Contact age column `cnt_age` or columns to estimate contact age (`cnt_age_exact` or `cnt_age_est_min` and `cnt_age_est_max`) do not exist in the contact data frame.

# warning is thrown if participant data has no country

    Country column `country` does not exist in the participant data frame.

# check result is reported back

    Code
      check(x = polymod2)
    Message
      > Check OK.

# good suggestions are made

    `wpp_age()` was deprecated in socialmixr 0.6.0.
    Pass population data directly via the {.arg survey_pop} argument instead.
    i The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from GitHub for more recent data.

---

    `wpp_age()` was deprecated in socialmixr 0.6.0.
    Pass population data directly via the {.arg survey_pop} argument instead.
    i The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from GitHub for more recent data.

---

    `wpp_age()` was deprecated in socialmixr 0.6.0.
    Pass population data directly via the {.arg survey_pop} argument instead.
    i The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from GitHub for more recent data.

# nonsensical operations are warned about

    `wpp_age()` was deprecated in socialmixr 0.6.0.
    Pass population data directly via the {.arg survey_pop} argument instead.
    i The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from GitHub for more recent data.

---

    Code
      contact_matrix(survey = polymod, counts = TRUE, symmetric = TRUE, age_limits = c(
        0, 5))
    Condition
      Warning:
      `wpp_age()` was deprecated in socialmixr 0.6.0.
      Pass population data directly via the {.arg survey_pop} argument instead.
      i The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from GitHub for more recent data.
      Warning in `contact_matrix()`:
      `symmetric = TRUE` does not make sense with `counts = TRUE`; will not make matrix symmetric.
      Warning in `normalise_weighted_matrix()`:
      Large differences in the size of the sub-populations with the current age breaks are likely to result in artefacts after making the matrix symmetric.
      ! Please reconsider the age breaks to obtain more equally sized sub-populations.
      i Normalization factors: [0.2 and 5.1]
    Output
      $matrix
               contact.age.group
      age.group    [0,5)  [5,Inf)
        [0,5)   1463.000 26710.24
        [5,Inf) 1396.075 87824.00
      
      $demography
         age.group population proportion  year
            <char>      <num>      <num> <int>
      1:     [0,5)   13498647 0.04967121  2005
      2:   [5,Inf)  258261321 0.95032879  2005
      
      $participants
         age.group participants proportion
            <char>        <int>      <num>
      1:     [0,5)          654 0.09085857
      2:   [5,Inf)         6544 0.90914143
      

---

    `wpp_age()` was deprecated in socialmixr 0.6.0.
    Pass population data directly via the {.arg survey_pop} argument instead.
    i The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from GitHub for more recent data.

# warning if survey is assumed to be representative

    No `survey.pop` or `countries` given, and no `country` column found in the data.
    i I don't know which population this is from (assuming the survey is representative).

# user-defined reference populations with open age groups

    Code
      contact_matrix(polymod_nocountry, age_limits = c(0, 18, 60), symmetric = TRUE,
      survey_pop = "dummy")
    Condition
      Warning:
      `wpp_age()` was deprecated in socialmixr 0.6.0.
      Pass population data directly via the {.arg survey_pop} argument instead.
      i The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from GitHub for more recent data.
      Error in `survey_pop_from_countries()`:
      ! Could not find population data for: "dummy".
      i Pass population data directly via the `survey_pop` argument.

# symmetric matrices with large norm weights warn

    `wpp_age()` was deprecated in socialmixr 0.6.0.
    Pass population data directly via the {.arg survey_pop} argument instead.
    i The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from GitHub for more recent data.

