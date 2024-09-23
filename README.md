Exploring R's RNG 
============================================================================================

## How R sets up a Mersenne-Twister RNG state

* Call `set.seed(seed)`
    + Create an empty vector `seeds` with length 624 + 52 = 676
    + Set `seeds[1]` to `seed`
    + Fill the rest of the vector using `seeds[i] <- Int32(69069 * seeds[i-1] + 1)`
    + `Int32` converts signed big int's C Int32s using two's complement overflow
    + Throw away the first 52 elements of `seeds`
    + The remaining 624 elements of `seeds` become `.Random.seed[3:626]` in `.GlobalEnv`
    + `.Random.seed[1]` is a code representing the RNG state method, the default is 10403
    + `.Random.seed[2]` is the index of the last used seed
    + When first calling `set.seed`, `.Random.seed[2]` equals 624 indicating that the vector we just created is completely spent
    + When asked for a random number, if the vector is spent, twist it and reset the index

* How twisting works

    ```
    N <- 624
    M <- 397
    MATRIX_A          <- 0x9908b0df
    UPPER_MASK        <- 0x80000000
    LOWER_MASK        <- 0x7fffffff
    TEMPERING_MASK_B  <- 0x9d2c5680
    TEMPERING_MASK_C  <- 0xefc60000
    mag01             <- c(0, MATRIX_A)
    
    # 1:227 references 1:228,398:624
    k <- 1:(N-M)
    y <- (seeds[k] %&% UPPER_MASK) %|% (seeds[k+1] %&% LOWER_MASK)
    seeds[k] <- seeds[k+M] %^% (y %>>% 1) %^% mag01[(y %&% 0x1) + 1]
    
    # 228:454 references 228:455,1:227
    k <- (N-M+1):(2*(N-M))
    y <- (seeds[k] %&% UPPER_MASK) %|% (seeds[k+1] %&% LOWER_MASK)
    seeds[k] <- seeds[k+(M-N)] %^% (y %>>% 1) %^% mag01[(y %&% 0x1) + 1]
    
    # 455:623 references 455:624,228:396
    k <- (2*(N-M)+1):(N-1)
    y <- (seeds[k] %&% UPPER_MASK) %|% (seeds[k+1] %&% LOWER_MASK)
    seeds[k] <- seeds[k+(M-N)] %^% (y %>>% 1) %^% mag01[(y %&% 0x1) + 1]
    
    # k = 624 references 1,624,397
    y <- (seeds[N] %&% UPPER_MASK) %|% (seeds[1] %&% LOWER_MASK)
    seeds[N] <- seeds[M] %^% (y %>>% 1) %^% mag01[(y %&% 0x1) + 1]
    
    return(Int32(seeds))
    ```

