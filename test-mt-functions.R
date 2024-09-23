source('mt-functions.R')

set.seed(1)
set_seed_alt(1)
all(.Random.seed == .Random.seed.alt)

u1 <- runif(1)
u2 <- runif_alt(1)
all(.Random.seed == .Random.seed.alt)

for (i in 1:10) {
  N <- 100e3
  message("Iteration i = ", i)
  message("  .Random.seed[2] = ", .Random.seed[2])
  message("  .Random.seed.alt[2] = ", .Random.seed.alt[2])
  message("  sum(.Random.seed) = ", sum(.Random.seed))
  message("  sum(.Random.seed.alt) = ", sum(.Random.seed.alt))
  u3 <- runif(N)
  u4 <- runif_alt(N)
  message("  all(u3 == u4) = ", all(u3 == u4))
  message(
    "  all(.Random.seed == .Random.seed.alt) = ", 
    all(.Random.seed == .Random.seed.alt) )
  
}

system.time(runif(1e6))
system.time(runif_alt(1e6))


for (i in 1:10) {
  N <- 100e3
  min <- -1 * i
  max <- i * 10
  message("Iteration i = ", i)
  message("  .Random.seed[2] = ", .Random.seed[2])
  message("  .Random.seed.alt[2] = ", .Random.seed.alt[2])
  message("  sum(.Random.seed) = ", sum(.Random.seed))
  message("  sum(.Random.seed.alt) = ", sum(.Random.seed.alt))
  u3 <- runif(N, min, max)
  u4 <- runif_alt(N, min, max)
  message("  all(u3 == u4) = ", all(u3 == u4))
  message(
    "  all(.Random.seed == .Random.seed.alt) = ", 
    all(.Random.seed == .Random.seed.alt) )
  
}
