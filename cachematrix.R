## Programming Assignment 2
## Programmer: Bryon Kenne Date: 7/25/2014

## These functions support solving the inverse of a square matrix. Additionally, these functions
## recognize that process to solve the matrix can be processor intensive. In this regard, if the process
## is requested, and the solution was generated using the function, the prior solution will be cached and the cache recalled.
## These functions take advantage of R's Lexical Environment, in which objects can be stored, manipulated,  and called 
## outside of the frames in which they are created.

## The makeCacheMatrix function allows the user to input a matrix. When this function is assigned to 
## an object "myCacheMatrix<-makeCacheMatrix(matrix)" passes a series of embedded functions that allow
## the user to $set the matrix value, $get the matrix value set, $setinversematrix when solved to cache, and
## $getinversematrix from cache.  These functions are assigned to the object as a list that is returned.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL								## On first execution is inverse matrix is set to NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(matrix) m <<- matrix
  getinversematrix <- function() m 
  list(set = set, get = get, setinversematrix = setinversematrix, getinversematrix = getinversematrix)
}


## The cacheSolve function allows the user solve a matrix created using the makeCacheMatrix function.  Its
## input is an makeCacheMatrix object.  Within this function, if the makeCacheMatrix's getinversematrix result
## has not be cached prior to execution, the function will calculate the inverse matrix (using solve) and store this to cache
## cache the result using the makeCacheMatrix setinversematrix function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinversematrix() 						## Retrieves the Cached Inverse if not empty
  if(!is.null(m)) {									## If the inverse exists, then the function returns the prior calculation.
    message("retrieving cached inverse matrix")
    m
  }
  data <- x$get()									## Return the input matrix 
  m <- solve(data)									## Solve the input matrix
  x$setinversematrix(m)
  m
}