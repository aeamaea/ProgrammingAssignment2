## Put comments here that give an overall description of what your
## functions do
##
## aeam :: April 20, 2014 -- cachematrix.R
##    This is the 2nd programming assignment for the R Programming Course by Dr. Peng @ Johns Hopkins
##    cloned from github.com/rdpeng/ProgrammingAssignment2
##
## This R file contains two functions. The code is basically modeled after the two examples
## Given in the Programming Assignment 2 instructions (makeVector() and cachemean())

###################################################################################
###################################################################################
###################################################################################
## Test Run
###################################################################################
###################################################################################
###################################################################################

# > hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# > h8 <- hilbert(8)
# > sh8 <- solve(h8)
# > specialMatrix <- makeCachedMatrix(h8)
# > cacheSolve(specialMatrix)
# [,1]      [,2]       [,3]       [,4]        [,5]        [,6]        [,7]
# [1,]      64     -2016      20160     -92400      221760     -288288      192192
# [2,]   -2016     84672    -952560    4656960   -11642400    15567552   -10594584
# [3,]   20160   -952560   11430720  -58212000   149688000  -204324119   141261119
# [4,]  -92400   4656960  -58212000  304919999  -800414996  1109908794  -776936155
# [5,]  221760 -11642400  149688000 -800414996  2134439987 -2996753738  2118916783
# [6,] -288288  15567552 -204324119 1109908793 -2996753738  4249941661 -3030050996
# [7,]  192192 -10594584  141261119 -776936154  2118916782 -3030050996  2175421226
# [8,]  -51480   2882880  -38918880  216215998  -594593995   856215351  -618377753
# [,8]
# [1,]     -51480
# [2,]    2882880
# [3,]  -38918880
# [4,]  216215998
# [5,] -594593995
# [6,]  856215352
# [7,] -618377753
# [8,]  176679358
# > cacheSolve(specialMatrix)
# getting cached inverse matrix
# [,1]      [,2]       [,3]       [,4]        [,5]        [,6]        [,7]
# [1,]      64     -2016      20160     -92400      221760     -288288      192192
# [2,]   -2016     84672    -952560    4656960   -11642400    15567552   -10594584
# [3,]   20160   -952560   11430720  -58212000   149688000  -204324119   141261119
# [4,]  -92400   4656960  -58212000  304919999  -800414996  1109908794  -776936155
# [5,]  221760 -11642400  149688000 -800414996  2134439987 -2996753738  2118916783
# [6,] -288288  15567552 -204324119 1109908793 -2996753738  4249941661 -3030050996
# [7,]  192192 -10594584  141261119 -776936154  2118916782 -3030050996  2175421226
# [8,]  -51480   2882880  -38918880  216215998  -594593995   856215351  -618377753
# [,8]
# [1,]     -51480
# [2,]    2882880
# [3,]  -38918880
# [4,]  216215998
# [5,] -594593995
# [6,]  856215352
# [7,] -618377753
# [8,]  176679358


###################################################################################
###################################################################################
## makeCachedMatrix() is a function which creates a "special" matrix object that can 
##  cache its inverse. What that means is that if the inverse of a given matrix object
##  has already been computed in a previous invocation, it will just return that, if not
##  it will create the inverse, cache it (save a copy using the function's closure environment)
##  and return an array of getter/setter functions.
##
##  I'm basically modifying the cachemean/makevector code to make the funcs below work or matrix
##  inversion.
###################################################################################
###################################################################################

makeCachedMatrix <- function(x = matrix()) {

  ## m a local variable, it will hold matrix value in the local environment
  m <- NULL
  
  ## This is a "setter" function for a "made" CachedMatrix object
  set <- function(y) {
    ## assign the value of matrix passed in to set() method of a "CacheMatrix" object
    ## to "x" which was passed into our function (see func def above)
    ## <<- operator means you are assigning a value to a variable "x" that exists
    ## in an environment ONE LEVEL UP 
    x <<- y
    ## reset the value of matrix in calling environment to NULL
    m <<- NULL
  }
  
  ## getter method for the generated object. Just return whatever matrix was passed in
  get <- function() x
  
  ## this method sets the inverse (why are we talking about methods? because it kind of "acts" like a method)
  ## the cacheSolve() function will pass in the value inverseMatrix to this setter funciton.
  ## the use of <<- operator shows that this value is being set in the matrix var "m" one environment above
  setinverse <- function(inverseMatrix) m <<- inverseMatrix
  
  ## getter for the inverted value of matrix it would have been set previously by setinverse() above
  getinverse <- function() m
  
  ## we build a list of functions that manipulate values and return it so the object can then call them
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


###################################################################################
###################################################################################
##  Return a matrix that is the inverse of 'x'
## Note that this "x" being passed ot cacheSolve has already been defined when 
##    SpecialMatrix <- makeCachedMatrix(matrix(blah,blah,blah))
## Then we just call cacheSolve(SpecialMatrix) so this SpecialMatrix is what is coming in as "x" argument.
###################################################################################
###################################################################################


cacheSolve <- function(x, ...) {
  
  ## m is just a local variable which gets its value by calling the getinverse() method of the object passed in 
  ##
  m <- x$getinverse()
  
  ## we check here if m got anything back, if it did, it means the value had already been cached in the
  ## object as it had previously been passed cacheSolve()
  ## if that is the case then the value of m will not be null, that is what if statement checks
  
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    ## The function terminates right here, the statements following the end of if() do not get executed
    ## we return the value of m that had been cached by a previous call to the cacheSolve() function.
    return(m)
  }
  
  ## We are here, this means that we are being called for the first time or the value of m is NULL
  ## x$get() will get us the value of the actual (non inversed matrix that was passed to makeCachedMatrix())
  data <- x$get()
  
  ## the local variable m gets the inverse of the matrix (solve() function does this )
  m <- solve(data, ...)
  ## we reach into the object x inside our scope (or SpecialMatrix in the outer environment)
  ## and set the value of the inverse matrix for us to use the next time around.
  x$setinverse(m)
  
  ## here we just return the computed inverse value of the matrix.
  m
}
