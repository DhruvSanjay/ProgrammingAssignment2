## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  get <- function() x ## here the matrix is assigned to the variable in parent environment because of lexical scoping
  set <- function (y){
    x <<- y ## assigning the value of y to the variable x in the parent environment
    i <<- NULL ## assigning NULL to i in the parent environment 
  }
  # the get () is same as the  assignment in the function makeCacheMatrix argument and assigning of the i variable
  set_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i ## here the value is assinged to the vairable i in the parent environment
  list(set = set, get = get, ## here we are assigning the names  to all the function so that we can use $ to call them
       set_inverse = set_inverse,
       get_inverse = get_inverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$get_inverse() ## here we are calling the function
  if(!is.null(i)) { ## here we are checking the value of i does it exist
          message("getting cached data")
          return(i)
          }
          data <- x$get() ## here we are getting the x matrix
          i <- solve(data) ## here we are obtaining the inverse of a matrix
          x$set_inverse(i) ##here we are calling the function to assign the to the variable in the parent variable
          i
        ## Return a matrix that is the inverse of 'x'
}
