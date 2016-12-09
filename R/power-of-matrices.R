# # # MATRIX MULTIPLICATION
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

# # # DATAFRAME MULTIPLICATION
# Need to be done and tested



# # # MATRIX MULTIPLICATION
#    user  system elapsed
#    0.01    0.00    0.02
# # # APPLY
#    user  system elapsed
#    0.11    0.00    0.11
# # # FOR LOOP
#    user  system elapsed
#   12.32    0.02   12.46
# # # REP AND ADD
#    user  system elapsed
#    0.10    0.00    0.11
