source('mt-functions.R')

seed <- sample(-100:100, 1)

# At set.seed R stores RNG state in .Random.seed
# However, it is considered already spent
# So, compare it pre-twist
set.seed(
  seed = seed, 
  kind = 'Mersenne-Twister', 
  normal.kind = "Inversion", 
  sample.kind = "Rejection")

R_seeds <- .Random.seed[3:626]
my_seeds <- init_seeds__R(seed)
all(R_seeds == my_seeds)

# Force R to get an unspent seed vector by asking for a random value
# Then compare to an unspent vector manually created
set.seed(seed)
invisible(runif(1))
R_seeds_twisted <- .Random.seed[3:626]
my_seeds_twisted <- init_seeds(seed)
all(R_seeds_twisted == my_seeds_twisted)





# Check unif random variables
set.seed(10)
R_runif <- runif(624)
my_seeds <- init_seeds(10)
my_runif <- runif_from_seeds(my_seeds)
all(R_runif == my_runif)

R_runif <- runif(624)
my_seeds <- twist_seeds(my_seeds)
my_runif <- runif_from_seeds(my_seeds)
all(R_runif == my_runif)

R_runif <- runif(624)
my_seeds <- twist_seeds(my_seeds)
my_runif <- runif_from_seeds(my_seeds)
all(R_runif == my_runif)



# What about fractional seeds?
seed <- 20.5
set.seed(seed)
R_seeds <- .Random.seed[3:626]
my_seeds <- init_seeds__R(seed)
all(R_seeds == my_seeds)

seed <- -891.7
set.seed(seed)
R_seeds <- .Random.seed[3:626]
my_seeds <- init_seeds__R(seed)
all(R_seeds == my_seeds)

seed <- -2.1
set.seed(seed)
R_seeds <- .Random.seed[3:626]
my_seeds <- init_seeds__R(seed)
all(R_seeds == my_seeds)

seed <- -123.5
set.seed(seed)
R_seeds <- .Random.seed[3:626]
my_seeds <- init_seeds__R(seed)
all(R_seeds == my_seeds)

