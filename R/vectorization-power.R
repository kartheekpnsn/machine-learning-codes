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
