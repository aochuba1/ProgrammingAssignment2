# defining variables and showing ways to verify the inverse of a matrix
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2) #matrix
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2) # identity matrix
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2) # inverse
# Checks:
m1 %*% n1 # gives you identity
n1 %*% m1 # gives you identity
solve(m1) # gives you n1
solve(n1) # gives you m1

#defining the cached functions
makeCacheMatrix <- function(x) {
  m <- NULL       #always set the m null to overwrite the previous inverse
  set <- function(y) { #This is the function you use to set a new value.  
    x <<- y       # to super assign the original matrix with the new matrix
    m <<- NULL    #always set the m null to overwrite the previous inverse matrix
  }
  get <- function() x # retrieves the input matrix
  setinverse <- function(inverse) m <<- inverse #super assigns the inverse to m. No computation 
  #is done here
  #"inverse" is just a place holder
  getinverse <- function() m # retrieves the inverse 
  list(set = set, get = get,   #makeCacheMatrix is a list of other functions. Very interesting
       setinverse = setinverse, #labeling each function allows us to call each function by their names
       getinverse = getinverse) # e.g. myMatrix_object$get() gets you the input matrix
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse() #cacheSolve and makecaheMatrix are related to the global enviroment. 
  #x$getinverse searches is retrieved from where it exists, in makeCacheMatrix, which is in the global env. 
  if(!is.null(m)) { #if statement to check if anything is in m
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #this is where the real computation happens. I wonder why the ellipses though
  x$setinverse(m) #sets the inverse and returns m
  m
}

myMatrix_object <- makeCacheMatrix(m1)
# and then
#cacheSolve(myMatrix_object)
# should return exactly the matrix n1
n1 == cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)
# calling cacheSolve again should retrieve (not recalculate) n1
cacheSolve(myMatrix_object)
#Another test
m1 %*% cacheSolve(myMatrix_object)
# should give you identity
# you can use the set function to "put in" a new matrix.
# For example n2
n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)
# and obtain its matrix inverse by
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)
