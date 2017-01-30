# # ordering
x = c(1:4, NA, 5)
x[order(x, na.last=NA)] # removes NA
x[order(x)] # places NA at last
x[order(x, na.last =F)] # places NA at first

# # Sub setting
x = 1:5
x[c(T, F)] # translated as X[c(T, F, T, F, T)]

# # List subsetting
x = list(abc = 1:5)
x$a # works
x[[“a”]] # doesn’t work

# # Function calls
1)	Matches by Exact name
2)	Matches by Prefix
f = function(abcdef, bcde1, bcde2) { list(a = abcdef, b1 = bcde1, b2 = bcde2) } 
print(f(2, 3, a = 1))
3)	Matches by Position
In the above example 2, 3 values are mapped by position

# # works
`for`(i, 1:2, print(i))

# # Primitive function example
sum() # as it directly calls C-Code

# # works
df <- data.frame(x = 1:3)
df$y <- list(1:2, 1:3, 1:4)

# # doesn't work
df = data.frame(x = 1:3, y = list(1:2, 1:3, 1:4))

# # works: I() - treats the list as a single component
df = data.frame(x = 1:3, y = I(list(1:2, 1:3, 1:4)))

# # works
df = data.frame(x = 1:3, y = I(matrix(1:9, nrow = 3)))

# # check this out
f = function(a) {
                function() {
                                print("Hello")
                }
                print(a)
}
f(a = 2) # it will print 2, it won’t print Hello

# # check this out
f = function(x = 2, y) {
                print(x)
                print(y)
}
f(3) # it will take 3 for x not y - we missed this..

# # check this out
f = function(x = 2, y) {
                print(x)
                print(y)
}
f() # it will print x value and throw error – LAZY EVALUATION

# # check this out
f = function(x, y = 2) {
                print(x)
                print(y)
}
f() # it will print error and stop (it won’t print y value)

# # check this out
matrix(1, nrows = 2, ncols = 2) # it will fill with all 1's

# # check this out
diag(matrix(1:6, nrows=3)) # it will print diagonal even if it is not square matrix

# # works
sapply(c(1:5),sum,3) # Will add 3 to each element of the vector . Ans = [4,5,6,7,8]

# # works
N=function(x) {return(x/2)}
New=function(){N=10; A=N(N); print(A)}
New() # This prints exactly 5.

# # works
cumsum(c(1, 2, NA, 4, 5)) # This prints 1 2 NA NA NA
