library(bitops)


Int32 <- function(x) {
  # Mimics C's two's complement overflow of signed Int32
  x <- sign(x)*floor(abs(x))
  x <- x %% 2^32
  ifelse(x >= 2^31, x - 2^32, x)
}


# init_seeds__R <- function(seed) {
#   # src/main/RNG.c:RNG_Init
#   # https://github.com/adamleerich/r-source-3efd684/blob/302209f/src/main/RNG.c#L269
#   
#   # R does a check if all are zero, but that seems impossible unless
#   # the user has overwritten .Random.seed
#   
#   stopifnot(length(seed) == 1)
#   N <- 624
#   seeds <- rep(NA_integer_, N+52)
#   seeds[1] <- Int32(seed)
#   
#   for (i in 2:(N+52)) {
#     seeds[i] <- Int32(69069 * seeds[i-1] + 1)
#   }
#   
#   # First 52 get thrown away
#   output <- seeds[53:(N+52)]
#   stopifnot(any(seeds != 0))
#   return(output)
# }


# twist_seeds <- function(seeds) {
#   # src/main/RNG.c:MT_genrand
#   # https://github.com/adamleerich/r-source-3efd684/blob/302209f/src/main/RNG.c#L701
#   
#   stopifnot(length(seeds) == 624 && all(Int32(seeds) == seeds))
#   
#   N <- 624
#   M <- 397
#   MATRIX_A          <- 0x9908b0df
#   UPPER_MASK        <- 0x80000000
#   LOWER_MASK        <- 0x7fffffff
#   TEMPERING_MASK_B  <- 0x9d2c5680
#   TEMPERING_MASK_C  <- 0xefc60000
#   mag01             <- c(0, MATRIX_A)
#   
#   # 1:227 references 1:228,398:624
#   k <- 1:(N-M)
#   y <- (seeds[k] %&% UPPER_MASK) %|% (seeds[k+1] %&% LOWER_MASK)
#   seeds[k] <- seeds[k+M] %^% (y %>>% 1) %^% mag01[(y %&% 0x1) + 1]
#   
#   # 228:454 references 228:455,1:227
#   k <- (N-M+1):(2*(N-M))
#   y <- (seeds[k] %&% UPPER_MASK) %|% (seeds[k+1] %&% LOWER_MASK)
#   seeds[k] <- seeds[k+(M-N)] %^% (y %>>% 1) %^% mag01[(y %&% 0x1) + 1]
#   
#   # 455:623 references 455:624,228:396
#   k <- (2*(N-M)+1):(N-1)
#   y <- (seeds[k] %&% UPPER_MASK) %|% (seeds[k+1] %&% LOWER_MASK)
#   seeds[k] <- seeds[k+(M-N)] %^% (y %>>% 1) %^% mag01[(y %&% 0x1) + 1]
#   
#   # k = 624 references 1,624,397
#   y <- (seeds[N] %&% UPPER_MASK) %|% (seeds[1] %&% LOWER_MASK)
#   seeds[N] <- seeds[M] %^% (y %>>% 1) %^% mag01[(y %&% 0x1) + 1]
#   
#   Int32(seeds)
# }


# init_seeds <- function(seed) {
#   twist_seeds(init_seeds__R(seed))
# }


# runif_from_seeds <- function(seeds) {
#   # src/main/RNG.c:MT_genrand
#   # https://github.com/adamleerich/r-source-3efd684/blob/302209f/src/main/RNG.c#L715
#   
#   half_i2_32m1 <- 0.5 * 2.328306437080797e-10
#   TEMPERING_MASK_B <- 0x9d2c5680
#   TEMPERING_MASK_C <- 0xefc60000
#   y <- Int32(seeds)
#   y <- y %^% (y %>>% 11)
#   y <- y %^% ((y %<<% 7) %&% TEMPERING_MASK_B)
#   y <- y %^% ((y %<<% 15) %&% TEMPERING_MASK_C)
#   y <- y %^% (y %>>% 18)
#   u <- y * 2.3283064365386963e-10
#   ifelse(u <= 0.0, half_i2_32m1, ifelse(u >= 1.0, 1.0 - half_i2_32m1, u))
# }

set_seed_alt <- function(seed) {
  # src/main/RNG.c:RNG_Init
  # https://github.com/adamleerich/r-source-3efd684/blob/302209f/src/main/RNG.c#L269
  
  # R does a check if all are zero, but that seems impossible unless
  # the user has overwritten .Random.seed
  
  stopifnot(length(seed) == 1)
  N <- 624
  seeds <- rep(NA_integer_, N+52)
  seeds[1] <- Int32(seed)
  
  for (i in 2:(N+52)) {
    seeds[i] <- Int32(69069 * seeds[i-1] + 1)
  }
  
  # If any all are zero, abort
  stopifnot(any(seeds != 0))
  
  # First 52 get thrown away
  rsa <- c(10403, 624, seeds[53:(N+52)])
  assign('.Random.seed.alt', rsa, .GlobalEnv)
  return(invisible(rsa))
}


.advance_Random_seed_alt <- function() {
  # src/main/RNG.c:MT_genrand
  # https://github.com/adamleerich/r-source-3efd684/blob/302209f/src/main/RNG.c#L701
  
  stopifnot(exists('.Random.seed.alt', envir = .GlobalEnv))
  
  rsa <- get('.Random.seed.alt', envir = .GlobalEnv)
  stopifnot(rsa[1] == 10403)
  seeds <- rsa[3:626]
  stopifnot(length(seeds) == 624 && all(Int32(seeds) == seeds))
  
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
  
  rsa[2] <- 0
  rsa[3:626] <- Int32(seeds)
  assign('.Random.seed.alt', rsa, .GlobalEnv)
  return(invisible(rsa))
}



runif_alt <- function (n, min = 0.0, max = 1.0) {

  stopifnot(exists('.Random.seed.alt', envir = .GlobalEnv))
  stopifnot(length(min) == 1L & length(max) == 1L)
  stopifnot(max * 1.0 > min * 1.0)
  
  ru <- function(seeds) {
    # src/main/RNG.c:MT_genrand
    # https://github.com/adamleerich/r-source-3efd684/blob/302209f/src/main/RNG.c#L715
    
    half_i2_32m1 <- 0.5 * 2.328306437080797e-10
    TEMPERING_MASK_B <- 0x9d2c5680
    TEMPERING_MASK_C <- 0xefc60000
    y <- Int32(seeds)
    y <- y %^% (y %>>% 11)
    y <- y %^% ((y %<<% 7) %&% TEMPERING_MASK_B)
    y <- y %^% ((y %<<% 15) %&% TEMPERING_MASK_C)
    y <- y %^% (y %>>% 18)
    u <- y * 2.3283064365386963e-10
    ifelse(u <= 0.0, half_i2_32m1, ifelse(u >= 1.0, 1.0 - half_i2_32m1, u))
  }
  
  output <- numeric(n)
  output_cursor <- 1
  
  # rs_cursor is the *last used* index of .Random.seed.alt[3:626]
  while(n >= 1) {
    rsa <- get('.Random.seed.alt', envir = .GlobalEnv)
    stopifnot(rsa[1] == 10403)
    seeds <- rsa[3:626]
    rs_cursor <- rsa[2]
    rs_remaining <- 624 - rs_cursor
    
    if (rs_remaining == 0) {
      .advance_Random_seed_alt()
      rsa <- get('.Random.seed.alt', envir = .GlobalEnv)
      stopifnot(rsa[1] == 10403)
      seeds <- rsa[3:626]
      rs_cursor <- rsa[2]
      rs_remaining <- 624 - rs_cursor
    }
    
    m <- min(n, rs_remaining)
    
    stopifnot(m > 0)
    
    output[output_cursor:(output_cursor + m - 1)] <- 
      ru(seeds[(rs_cursor+1):(rs_cursor+m)])
    
    output_cursor <- output_cursor + m
    rsa[2] <- rs_cursor + m
    assign('.Random.seed.alt', rsa, .GlobalEnv)
    n <- n - m
  }
  
  
  return(output*(max - min) + min)
  
}




