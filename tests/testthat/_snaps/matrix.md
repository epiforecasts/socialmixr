# contact matrices look as expected

    Code
      contacts
    Output
      $test1
      $test1$matrix
               contact.age.group
      age.group      [0,5)     [5,10)     [10,15)    [15,20)    [20,25)     [25,30)
        [0,5)   183.997718  91.069386  46.5576553  31.088569  42.865507  75.2263424
        [5,10)   65.701661 682.253071 134.1139995  34.726523  30.789586  67.2116267
        [10,15)  41.989012 110.824190 707.6366749 106.062186  21.923083  45.1172068
        [15,20)  25.041289  76.227897 160.8464231 707.291536  98.255991  77.9876133
        [20,25)  27.186368  36.415092  16.0317672  93.051886 153.702056  75.9322299
        [25,30)  42.979000  44.367504  17.9306992  43.040950  87.000388 108.0798010
        [30,35)  39.751754  57.440637  28.9813815  24.782785  44.596600  58.2730707
        [35,40)  58.273390  97.171971  52.8985813  38.898581  43.902991  50.2292945
        [40,45)  15.099138  56.134187  62.2483455  53.077107  48.478666  46.1301811
        [45,50)  12.016897   9.028162  38.5181829  64.152075  47.943676  47.4843884
        [50,55)  23.502479  20.165840  20.8134295  45.958540  58.020730  58.0829202
        [55,60)  10.643803  11.724234  23.8827996  15.797772  32.986212  46.8713094
        [60,65)  12.835875  33.216645  18.2250856  12.959672  35.000938  44.2063285
        [65,70)   6.958540  12.958540  10.8548896  12.896350  10.020730  20.2902208
        [70,75)   2.980342   3.042125   9.0645910  16.101099   8.632112   8.9410256
        75+       0.000000   1.911419   0.9557096   2.929135   1.017716   0.9557096
               contact.age.group
      age.group    [30,35)    [35,40)    [40,45)    [45,50)   [50,55)   [55,60)
        [0,5)    92.616701  97.731513  52.196820  28.144334 31.213221 28.606361
        [5,10)  108.920564  99.722278 101.814448  54.782310 38.910862 20.947245
        [10,15)  62.765586 129.842424 116.432200  57.967036 41.465164 26.260053
        [15,20)  51.991742 114.086707  99.396374  80.727493 43.152769 35.268378
        [20,25)  52.321908  44.769158  42.655855  56.574319 25.973527 22.834810
        [25,30)  70.122851  55.887649  52.130202  55.056701 49.848798 31.087151
        [30,35) 100.000000  92.223421  49.291689  33.888289 36.173772 41.204803
        [35,40)  62.141104 105.216066  85.594325  56.246933 43.136695 36.933857
        [40,45)  63.515718  81.945925  83.921891  81.697579 44.154215 34.026036
        [45,50)  50.349210  60.270356  70.377372 103.687156 52.383005 28.287254
        [50,55)  61.020730  54.647589  59.601172  40.331681 49.331681 53.440288
        [55,60)  32.551881  37.121796  35.607034  43.232103 57.303342 62.669081
        [60,65)  50.178193  67.573031  62.369516  39.956859 39.250408 56.088159
        [65,70)  16.854890  25.668319  22.020730  16.207301 15.124380 23.352411
        [70,75)   3.994383   3.042125  19.971917  16.101099 10.016850  8.050549
        75+       2.867129   2.929135   3.946852   1.017716  6.937993  1.973426
               contact.age.group
      age.group   [60,65)   [65,70)    [70,75)       75+
        [0,5)   25.324752  9.144334 10.1049704  2.944234
        [5,10)  25.961192 10.919351  5.9672555  8.765331
        [10,15) 19.043953 12.139183  9.7216339 10.868142
        [15,20) 25.041289 20.851360  5.0825779  5.913293
        [20,25) 10.967174 10.904698  3.9142285  9.816809
        [25,30) 19.902349 12.970600  6.0388505  4.005250
        [30,35) 24.198597  8.148948  4.9689692  3.888289
        [35,40) 26.739839 21.154333 15.0132285 13.691334
        [40,45) 28.930904 13.893852 13.1231714 18.032045
        [45,50) 30.197135  7.056324 11.8310273 26.836660
        [50,55) 27.248761 14.103650 18.2487607 16.854890
        [55,60) 42.089624 14.841435  9.8736074 31.135585
        [60,65) 43.004689 24.135051 13.0215707 18.101289
        [65,70) 22.730509 18.771969  8.8134295 11.041460
        [70,75) 12.044933 18.067399 13.1825397 19.143223
        75+      4.902561  0.000000  0.9557096  6.044290
      
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
      16:       75+            7 0.006923838
      
      
      $test2
      $test2$matrix
               contact.age.group
      age.group     [0,5)        5+
          [0,5) 1.5429448  7.027931
          5+    0.3673316 13.474102
      
      $test2$demography
         age.group population proportion  year
            <char>      <num>      <num> <int>
      1:     [0,5)   13498647 0.04967121  2005
      2:        5+  258261321 0.95032879  2005
      
      $test2$participants
         age.group participants proportion
            <char>        <int>      <num>
      1:     [0,5)          654  0.0908712
      2:        5+         6543  0.9091288
      
      
      $test3
      $test3$mean.contacts
      [1] 3.887154
      
      $test3$normalisation
      [1] 1.021165
      
      $test3$contacts
      [1] 1.0973188 1.4624132 0.9343448
      
      $test3$matrix
               contact.age.group
      age.group    [0,5)   [5,10)       10+
         [0,5)  1.769367 1.836114 0.8815070
         [5,10) 1.377724 2.468087 0.8621040
         10+    1.035265 1.349344 0.9711241
      
      $test3$demography
         age.group population proportion  year
            <char>      <num>      <num> <int>
      1:     [0,5)    1271578 0.06282841  2005
      2:    [5,10)    1329536 0.06569210  2005
      3:       10+   17637787 0.87147948  2005
      
      $test3$participants
         age.group participants proportion
            <char>        <int>      <num>
      1:     [0,5)           95 0.09396637
      2:    [5,10)          102 0.10089021
      3:       10+          814 0.80514342
      
      
      $test4
      $test4$matrix
               contact.age.group
      age.group     [0,5)   [5,15)       15+
         [0,5)  0.0000000 2.029545  3.077273
         [5,15) 0.9262205 7.568413 27.364233
         15+    0.1816446 3.539355  7.139665
      
      $test4$demography
         age.group population proportion  year
            <char>      <num>      <num> <int>
      1:     [0,5)   13498647 0.04967121  2005
      2:    [5,15)   29578398 0.10884016  2005
      3:       15+  228682923 0.84148863  2005
      
      $test4$participants
         age.group participants proportion
            <char>        <int>      <num>
      1:     [0,5)          660 0.09134948
      2:    [5,15)         1374 0.19017301
      3:       15+         5191 0.71847751
      
      

# warning is thrown if day of week is asked to be weighed but not present

    `weigh.dayofweek` is "TRUE", but no `dayofweek` column in the data.
    i Will ignore.

# nonsensical operations are warned about

    `split = TRUE` does not make sense with `counts = TRUE`; will not split the contact matrix.

---

    Code
      contact_matrix(survey = polymod, counts = TRUE, symmetric = TRUE, age.limits = c(
        0, 5))
    Message
      Removing participants without age information.
      i To change this behaviour, set the `missing.participant.age` option.
      Removing participants that have contacts without age information.
      i To change this behaviour, set the 'missing.contact.age' option.
    Condition
      Warning in `contact_matrix()`:
      `symmetric = TRUE` does not make sense with `counts = TRUE`; will not make matrix symmetric.
      Warning in `normalise_weighted_matrix()`:
      Large differences in the size of the sub-populations with the current age breaks are likely to result in artefacts after making the matrix symmetric.
      ! Please reconsider the age breaks to obtain more equally sized sub-populations.
      i Normalization factors: [0.2 and 5.1]
    Output
      $matrix
               contact.age.group
      age.group    [0,5)       5+
          [0,5) 1463.000 26710.24
          5+    1396.075 87824.00
      
      $demography
         age.group population proportion  year
            <char>      <num>      <num> <int>
      1:     [0,5)   13498647 0.04967121  2005
      2:        5+  258261321 0.95032879  2005
      
      $participants
         age.group participants proportion
            <char>        <int>      <num>
      1:     [0,5)          654 0.09085857
      2:        5+         6544 0.90914143
      

# warning is thrown if it is assumed that the survey is representative

    No `survey.pop` or `countries` given, and no `country` column found in the data.
    i I don't know which population this is from (assuming the survey is representative).

