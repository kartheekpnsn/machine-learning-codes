# # vector addition
f1 = function() {
	x = numeric(100000);
	for(i in 1:100000) {
		x[i] = 10 + i
	}
	return(x)
}

f2 = function() {
	x = integer(100000);
	for(i in 1:100000) {
		x[i] = 10 + i
	}
	return(x)
}

f3 = function() {
	x = vector();
	for(i in 1:100000) {
		x = c(x, 10 + i)
	}
	return(x)
}

f4 = function(f) {
	x = f(1:100000, function(x) x + 10)
	return(x)
}

f5 = function() {
	x = 1:100000 + 10
	return(x)
}

system.time(f1()) # 5th
system.time(f2()) # 4th
system.time(f4(sapply)) # 3rd
system.time(f4(lapply)) # 2nd
system.time(f5()) # 1st

############################################################################################################################
############################################################################################################################
############################################################################################################################

# # MATRIX MULTIPLICATION
rm(list = ls())
gc()
N = 10000
ptm = proc.time()
d = matrix(round(runif(4 * N), 2), nrow = N)
v = c(4, 2, 3, 6)
lv = length(v)
v = diag(v, lv, lv)

d = d %*% v
print(proc.time() - ptm)


# # # APPLY
rm(list = ls())
gc()
N = 10000
ptm = proc.time()
d = data.frame(matrix(round(runif(4*N), 2), nrow = N))
v = c(4, 2, 3, 6)

d = t(apply(d, 1, function(x) x * v))
print(proc.time() - ptm)


# # # FOR LOOP
rm(list = ls())
gc()
N = 10000
ptm = proc.time()
d = data.frame(matrix(round(runif(4*N), 2), nrow = N))
v = c(4, 2, 3, 6)

for(i in 1:nrow(d)) {
	d[i, ] = d[i, ]*v
}
print(proc.time() - ptm)

# # # REP AND ADD
rm(list = ls())
gc()
N = 10000
ptm = proc.time()
d = data.frame(matrix(round(runif(4*N), 2), nrow = N))
v = c(4, 2, 3, 6)
d1 = apply(t(matrix(d[, 1],length(d[, 1]),v[1])), 2, sum)
d2 = apply(t(matrix(d[, 2],length(d[, 2]),v[2])), 2, sum)
d3 = apply(t(matrix(d[, 3],length(d[, 3]),v[3])), 2, sum)
d4 = apply(t(matrix(d[, 4],length(d[, 4]),v[4])), 2, sum)

d = matrix(c(d1,d2,d3,d4), nrow = length(d1))
print(proc.time() - ptm)

############################################################################################################################
############################################################################################################################
############################################################################################################################

# # for memory
library(pryr)
object_size(1:10)
mem_used()

# # use list instead of dataframe
x <- data.frame(matrix(runif(100 * 1e4), ncol = 100))
medians <- vapply(x, median, numeric(1))

# slower
for(i in 1:5) {
  x[, i] <- x[, i] - medians[i]
  print(c(address(x), refs(x)))
}

# better
y <- as.list(x)

for(i in 1:5) {
  y[[i]] <- y[[i]] - medians[i]
  print(c(address(y), refs(y)))
}

############################################################################################################################
############################################################################################################################
############################################################################################################################

# # boost speed
re-write the code with just what you want (all functions calculate several (measures)things that we may not use)

# # for speed
library(compiler)
rowtstat_c = compiler::cmpfun(rowtstat)

# # parallelize
library(parallel)
cores <- detectCores()
cores

pause <- function(i) {
  function(x) Sys.sleep(i)
}

# this
system.time(lapply(1:10, pause(0.25)))
# vs 
system.time(mclapply(1:10, pause(0.25), mc.cores = cores))

# in windows - You need to first set up a local cluster and then use parLapply():
cluster <- makePSOCKcluster(cores)
system.time(parLapply(cluster, 1:10, function(i) Sys.sleep(i)))

############################################################################################################################
############################################################################################################################
############################################################################################################################

# # use c++ with R
library(Rcpp)
cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')

# # comparison
sumR <- function(x) {
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  total
}
cppFunction('double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')
library(benchmark)
x <- runif(1e3)
microbenchmark(
  sum(x), 	# fastest
  sumC(x), 	# faster
  sumR(x) 	# slowest
)
