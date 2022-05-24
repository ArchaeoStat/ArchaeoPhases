# Summary

    Code
      summary(pha)
    Output
      $P1
                mad mean  sd   min   q1 median   q3  max lower upper
      start    -704 -771 148 -1349 -888   -747 -667 -207 -1045  -488
      end      -685 -520 168  -925 -670   -534 -382  -19  -778  -221
      duration  270  251 135     0  151    247  342  835     1   482
      
      $P2
                 mad  mean  sd   min    q1 median    q3   max lower upper
      start    -1767 -1786  98 -2000 -1858  -1786 -1720 -1244 -1980 -1614
      end      -1237 -1234  86 -1833 -1287  -1234 -1181  -780 -1400 -1064
      duration   547   552 130     4   466    553   640  1156   291   800
      

# Duration

    Code
      summary(pha_CE)
    Output
         mad mean  sd min  q1 median  q3  max lower upper
      P1 270  251 135   0 151    247 342  835     1   482
      P2 547  552 130   4 466    553 640 1156   291   800

# Boundaries

    Code
      pha_CE
    Output
             lower      upper
      P1 -1046.091  -202.1504
      P2 -1972.281 -1078.0925

---

    Code
      pha_BP
    Output
            lower    upper
      P1 2995.091 2151.150
      P2 3921.674 3027.217

# Transitions

    Code
      as.data.frame(pha_CE)
    Output
               lower     upper duration
      P2-P1 -1409.47 -501.6004 907.8698

---

    Code
      as.data.frame(pha_BP)
    Output
               lower    upper duration
      P2-P1 3358.486 2450.609 907.8772

# Hiatus

    Code
      as.data.frame(pha_CE)
    Output
                lower     upper duration
      P2-P1 -1062.599 -1046.091 16.50864

---

    Code
      as.data.frame(pha_BP)
    Output
               lower    upper duration
      P2-P1 3011.599 2995.091 16.50835

