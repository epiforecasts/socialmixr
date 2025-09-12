# process_age() appropriately changes dimensions

    Code
      dim(polymod_age_processed$participants)
    Output
      [1] 2184   52

---

    Code
      dim(polymod_age_processed$contacts)
    Output
      [1] 97904    16

# process_age() appropriately changes age.group factor

    Code
      levels(polymod_age_processed$participants$age.group)
    Output
       [1] "[0,1)"   "[1,2)"   "[2,3)"   "[3,4)"   "[4,5)"   "[5,6)"   "[6,7)"  
       [8] "[7,8)"   "[8,9)"   "[9,10)"  "[10,11)" "[11,12)" "[12,13)" "[13,14)"
      [15] "[14,15)" "[15,16)" "[16,17)" "[17,18)" "[18,19)" "[19,20)" "[20,21)"
      [22] "[21,22)" "[22,23)" "[23,24)" "[24,25)" "[25,26)" "[26,27)" "[27,28)"
      [29] "[28,29)" "[29,30)" "[30,31)" "[31,32)" "[32,33)" "[33,34)" "[34,35)"
      [36] "[35,36)" "[36,37)" "[37,38)" "[38,39)" "[39,40)" "[40,41)" "[41,42)"
      [43] "[42,43)" "[43,44)" "[44,45)" "[45,46)" "[46,47)" "[47,48)" "[48,49)"
      [50] "[49,50)" "[50,51)" "[51,52)" "[52,53)" "[53,54)" "[54,55)" "[55,56)"
      [57] "[56,57)" "[57,58)" "[58,59)" "[59,60)" "[60,61)" "[61,62)" "[62,63)"
      [64] "[63,64)" "[64,65)" "[65,66)" "[66,67)" "[67,68)" "[68,69)" "[69,70)"
      [71] "[70,71)" "[71,72)" "[72,73)" "[73,74)" "[74,75)" "[75,76)" "[76,77)"
      [78] "[77,78)" "[78,79)" "[79,80)" "[80,81)" "[81,82)" "[82,83)" "[83,84)"
      [85] "[84,85)" "85+"    

---

    Code
      levels(polymod_age_processed_0_5_10$participants$age.group)
    Output
      [1] "[0,5)"  "[5,10)" "10+"   

---

    Code
      levels(polymod_age_processed_5_10_15$participants$age.group)
    Output
      [1] "[5,10)"  "[10,15)" "15+"    

---

    Code
      range(polymod_age_processed_0_5_10$contacts$cnt_age, na.rm = TRUE)
    Output
      [1]  0 99

---

    Code
      range(polymod_age_processed_5_10_15$contacts$cnt_age, na.rm = TRUE)
    Output
      [1]  5 99

