#Honestly, I wasn't entirely sure how to do this, and I'm a bit unsure if it's correct
#as I didn't see any test cases, and what the output should be, like with the last assignment.
#A lot of this is borrowing heavily from the example and applying it to the "matrix" case
#and "inverse" functions. Confusing because the entire lesson, Swirl practice, and Quiz
#dealt with "_apply" functions, and they didn't seem to appear here.
# Anyway...here they are:

#Part 1 (writing "makeCachematrix" code)
#Function to create a matrix that can cache its inverse matrix
makeCacheMatrix <- function(mat = matrix()){
  inv <- NULL
  #setting the matrix values 
    set <- function(mat2) {
    mat <<- mat2
    inv <<- NULL
  }
  #getting the matrix values
    getmat <- function() {
    mat
  }
  #setting the inverse value
    setinv <- function(inverse){
    inv <<- inverse
  }
  #getting the inverse value
    getinv <- function(){
    inv
  }
  list(set = set, get = getmat, setinv = setinv, getinv = getinv)
}

##Part 2 (writing "cachesolve" function)

cachesolve <- function(mat, ...) {
   ## Return a matrix that is the inverse of 'x'
  ##First, check to see if the inverse has already been calculated, and if so return it here
  inv <- mat$getinv()
  if(!is.null(inv)) 
    {
    message("getting cached data")
    return(inv)
  }
  #if it hasn't, go about finding the inverse matrix here and returning it.
  data <- mat$get()
  inv <- solve(data) %*% data
  mat$setinv(inv)
  inv
}
