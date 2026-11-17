# Evaluates code with a fixed seed, without changing the user's random numbers
with_seed <- function(seed, code) {
  if (exists(".Random.seed", globalenv())) {
    old <- get(".Random.seed", globalenv())
    on.exit(assign(".Random.seed", old, globalenv()))
  } else {
    on.exit(rm(".Random.seed", envir = globalenv()))
  }
  set.seed(seed)
  code
}
