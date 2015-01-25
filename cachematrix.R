## Put comments here that give an overall description of what your

## Two functions are defined. The first function is responsible to create
## the their environment the variables that will be used to define and help obtain 
## the inverse of the matrix.

## The second function calls variables ("functions") that were defined in the environment
## used by the previous function call.  This second function receives the matrix
## that will be inverted, validates that there is no inversion previously calculated
## and if it the inverse is already there, returns the value without further calculations
## but if not it proceeds to calculate the inverse and return it.

## This two functions takes advantage of the lexical scoping and dynamical
## definition of functions
## 



## Write a short comment describing this function

## This function creates a list containing 4 variables and these variables are 
## to be defined with their respective functions.  Those functions will be 
## used help calculate the inverse of the matrix. 
## Also defines the null environment that test if the inverse matrix has been
## calculated or not.
##



makeCacheMatrix <- function(x = matrix()) {

     ## Inverse has not been calculated, so the parameter are set to null
     ## Once set is activated, it will reset the enviroment to null, also
     ## sets the correct matrix dimension in param1.
     ## variable x is defined in makeCacheMatrix but used until cachesolve function is run
     inv <- NULL
     set <- function(param1) {
          x <<- param1
          inv <<- NULL
     }
     
     ## sets the environents to the correct fuctions that will be calculated
     get <- function() x
     
     ## sets the inverse of the matrix, using command solve and assigning to inv
     matrixinverse <- function(solve) inv <<- solve
     
     ## returns the inverse of the matrix
     returniverse <- function() inv
     
     ## sets the list with all the functions defined
     list(set = set, get = get,
          matrixinverse = matrixinverse,
          returniverse = returniverse)
     
}



## Write a short comment describing this function

## This function receives the matrix that was created and we need to 
## compute their inverse.
## Also this function takes advantage of the lexical definitio for R using 
## the functions and variables defined in function makeCacheMatrix
##



cacheSolve <- function(x, ...) {

     ## m returns the inverse of the matrix if it was already calculated
     m <- x$returniverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     ## gets the info cotained in the matrix
     data <- x$get()
     
     ## obtaines the inverse of the matrix
     m <- solve(data, ...)
     x$matrixinverse(m)
     m

}
