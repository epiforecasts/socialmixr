# survey_country_population() works

    Code
      survey_country_population(polymod)
    Output
          lower.age.limit population
                    <int>      <num>
       1:               0   13498647
       2:               5   14193543
       3:              10   15384855
       4:              15   16464604
       5:              20   17158735
       6:              25   17416147
       7:              30   18726407
       8:              35   21118833
       9:              40   21529102
      10:              45   19927357
      11:              50   18134701
      12:              55   17122402
      13:              60   14454988
      14:              65   14280416
      15:              70   11565417
      16:              75    9469420
      17:              80    6736784
      18:              85    2832127
      19:              90    1417066
      20:              95     300205
      21:             100      28212
          lower.age.limit population

---

    Code
      survey_country_population(polymod, countries = "Belgium")
    Output
          lower.age.limit population
                    <int>      <num>
       1:               0     583492
       2:               5     593148
       3:              10     632157
       4:              15     626921
       5:              20     649588
       6:              25     663176
       7:              30     705878
       8:              35     773177
       9:              40     823305
      10:              45     779436
      11:              50     710061
      12:              55     674177
      13:              60     508701
      14:              65     496720
      15:              70     470384
      16:              75     392469
      17:              80     291166
      18:              85     108599
      19:              90      52117
      20:              95      11126
      21:             100       1087
          lower.age.limit population

---

    Code
      survey_country_population(polymod, countries = c("Belgium", "Italy"))
    Output
          lower.age.limit population
                    <int>      <num>
       1:               0    3342241
       2:               5    3305938
       3:              10    3447403
       4:              15    3544397
       5:              20    3818506
       6:              25    4449760
       7:              30    5258993
       8:              35    5570182
       9:              40    5525281
      10:              45    4915874
      11:              50    4537519
      12:              55    4551877
      13:              60    3770551
      14:              65    3819156
      15:              70    3329171
      16:              75    2771930
      17:              80    2031237
      18:              85     818477
      19:              90     445093
      20:              95      93168
      21:             100       8612
          lower.age.limit population

---

    Code
      survey_country_population(polymod, countries = "Australia")
    Output
          lower.age.limit population
                    <int>      <num>
       1:               0    1271578
       2:               5    1329536
       3:              10    1405655
       4:              15    1393171
       5:              20    1415491
       6:              25    1330279
       7:              30    1515614
       8:              35    1456957
       9:              40    1560931
      10:              45    1455358
      11:              50    1326630
      12:              55    1247551
      13:              60     920492
      14:              65     750231
      15:              70     619847
      16:              75     548509
      17:              80     401271
      18:              85     191366
      19:              90      79997
      20:              95      16649
      21:             100       1788
          lower.age.limit population

# survey_country_population() errors appropriately

    Code
      survey_country_population(polymod_copy)
    Condition
      Error in `survey_country_population()`:
      ! Country name must be provided in `survey` or `countries`
      i `survey`: `survey$participants$country` is: "NULL"
      i `countries` is: "NULL"

