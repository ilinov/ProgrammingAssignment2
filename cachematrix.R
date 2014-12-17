## Special matrix object with cache
makeCacheMatrix <- function(x = matrix()) {
  ## x and m are stored within the scope of makeCacheMatrix
  m = NULL
  ## Method assigns the provided value to the matrix and purges the cache
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  ## Method returns the matrix
  get = function() x
  ## Method sets the inverse for caching
  setcache = function(inv) m <<- inv
  ## Method gets the inverse from cache
  getcache = function() m
  list(set = set, get = get, setcache = setcache, getcache = getcache)
}

## Solving inverse, or getting from cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m = x$getcache()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data = x$get()
    m = solve(data, ...)
    x$setcache(m)
    m
}

## Testing if it works
aa = matrix(c(1,2,3,0,1,5,5,6,0), nrow = 3, ncol = 3)
aam = makeCacheMatrix(aa)
cacheSolve(aam)
## The second call should involve the cached value
cacheSolve(aam)
