# Credible interval

    Code
      cred_CE
    Output
      $E1
              start     stop    p
      [1,] -953.688 -356.518 0.68
      
      $E2
             start     stop    p
      [1,] -1888.3 -1688.86 0.68
      
      $E3
              start     stop    p
      [1,] -757.326 -606.635 0.68
      
      $E4
              start     stop    p
      [1,] -1310.13 -1150.36 0.68
      
      attr(,"calendar")
      [1] "CE"

---

    Code
      cred_BP
    Output
      $E1
              start     stop    p
      [1,] 2902.688 2305.518 0.68
      
      $E2
             start    stop    p
      [1,] 3837.16 3637.72 0.68
      
      $E3
              start     stop    p
      [1,] 2706.326 2555.635 0.68
      
      $E4
             start    stop    p
      [1,] 3259.13 3099.36 0.68
      
      attr(,"calendar")
      [1] "BP"

# HPD interval

    Code
      hpdi_CE
    Output
      $E1
                start      stop    p
      [1,] -1009.3026 -774.1160 0.35
      [2,]  -490.0595 -257.9273 0.33
      
      $E2
               start      stop    p
      [1,] -1891.751 -1690.391 0.68
      
      $E3
               start      stop    p
      [1,] -757.2008 -604.5292 0.68
      
      $E4
               start      stop    p
      [1,] -1311.728 -1149.416 0.68
      
      attr(,"calendar")
      [1] "CE"

---

    Code
      hpdi_BP
    Output
      $E1
              start     stop    p
      [1,] 2439.060 2206.927 0.33
      [2,] 2958.303 2723.116 0.35
      
      $E2
              start     stop    p
      [1,] 3840.751 3639.391 0.68
      
      $E3
              start     stop    p
      [1,] 2706.201 2553.529 0.68
      
      $E4
              start     stop    p
      [1,] 3260.728 3098.416 0.68
      
      attr(,"calendar")
      [1] "BP"

