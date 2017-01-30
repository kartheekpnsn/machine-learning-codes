x = numeric(100000);
system.time(
for(i in 1:100000) {
	x[i] = 10 + i
})

x = integer(100000);
system.time(
for(i in 1:100000) {
	x[i] = 10 + i
})

x = vector();
system.time(
for(i in 1:100000) {
	x = c(x, 10 + i)
})

