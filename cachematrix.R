## The two functions below
## makeCacheMatrix and cacheSolve 
## will take as input a matrix, "cache" the matrix in "memory" (sort of) then
## calculate its inverse, if it does not already exist


makeCacheMatrix <- function(x = matrix()) {
  #this function takes as input a matrix
  #the inverse of this matrix will ultimately be calcuated in the cacheSolve() function
  #matrix is assumed to be non singular and therefore an inverse can be calculated
  
  #this function outputs a list with four elements, each element is a function
  #the four elements/functions are: set, get, setinv and getinv
  
  #Exampe calls:
  #  mymat <- matrix(c(1,2,3,4), ncol=2, nrow=2)
  #  listWithmymat <- makeCacheMatrix(mymat)
  
  #the list that is the output of this function (listwithmymat in the above example) is to be used as the input into casheSolve
  #the list that is the output of this function includes the matrix for which an inverse is sought (mymat in this exampe)
  
  #note the use of the <<- assignment operator to assign these values to the global environment
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



cacheSolve <- function(x, ...) {
  #this function takes as input the list created by the makeCacheMatrix funtion
  #one of the elements of this list is the matrix for which an inverse is to be calculated
  #the function assumes that the matrix is nonsingular and that therefore an inverse exists
  #the output of the function is the inverse of that matrix
  
  #Example call
  #   cacheSolve(listWithmymat)
  
  #if the inverse of this matrix has already been calucated, the inverse will be provided from the cache/global environment
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("Inverse Exists. Getting cached inverse.")
    return(m)
  }
  data <- x$get()
  message("Calculating Inverse For 1st Time")
  m <- solve(data, ...)
  x$setinv(m)
  m
}

