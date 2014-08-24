## makeCacheMatrix is a function that returns a wrapper around the standard matrix object.
## This wrapper has get/set functions to access the matrix object, and also getcache/setcache
## functions to access the inverse of the matrix. Using these inverse caching functions, computation
## time can be reduced.

## Return the matrix wrapper object
makeCacheMatrix <- function(x = matrix()) {
        # mat is the matrix object whose inverse is being cached.
        mat <- NULL

        # "inverse" is the cached inverse matrix of the matrix "mat".
        inverse <- NULL

        # "set" is the setter function for matrix object. When the matrix object
        # changes, any cached inverse matrix is no longer valid, so it is set to NULL
        # indicating it must be recalculated.
        set <- function(new_value) { mat <<- new_value; inverse <<- NULL; }

        # get returns the current matrix object
        get <- function() { return(mat); }

        # "getcache" returns the cached inverse matrix.
        getcache <- function() { return(inverse); }

        # "setcache" sets the cached inverse matrix.
        setcache <- function(new_value) { inverse <<- new_value; }

	# Return a list object consisting of the functions set, get, getcache, and setcache.
	wrapper = list(set=set,get=get,getcache=getcache,setcache=setcache)
	wrapper$set(x)
	return(wrapper)
}


## The cacheSolve function returns the solved inverse matrix corresponding to its input matrix
## parameter. It does so by accessing the cached inverse matrix, if there is any, otherwise it
## computes the inverse matrix and caches the result for later access.
cacheSolve <- function(x, ...) {
        ## Return the cached matrix, if it exists.
	inverse <- x$getcache()
	if(!is.null(inverse)) {
                return(inverse)
	}

	## Solve the matrix for the first time and cache the result
	mat <- x$get()
        inverse <- solve(mat)
	x$setcache(inverse)
	return(inverse)
}


## This function is used for testing purposes and also demonstrates proper usage.
test <- function() {
	print("Generating 2x2 test matrix:")
        testmatrix <- matrix(1:4,2,2) 
	print(testmatrix)

        print("Generate the wrapper object with makeCacheMatrix.")
        wrapperMatrix <- makeCacheMatrix(mat)
	print("get() evaluates to: ", wrapperMatrix$get()) 
        print("getcache() evaluates to:", wrapperMatrix$getcache())

        print("Solving the inverse via cacheSolve")
	cacheSolve(wrapperMatrix)
	print("Now the cached inverse is set. getcache() evaluates to:")
        print(wrapperMatrix$getcache())

	print("The test matrix solved the typical way is:")
	print(solve(mat))

	# Change the matrix and ensure that the new inverse is returned by cacheSolve calls
	print("setting the matrix to: ")
	print(matrix(2:5,2,2))
	wrapperMatrix$set(matrix(2:5,2,2))
	print("wrapperMatrix$get() evaluates to:")
	print(wrapperMatrix$get())
	print("wrapperMatrix$getCache() evaluates to:")
	print(wrapperMatrix$getcache())
        print("cacheSolve(wrapperMatrix) evaluates to:")
        print(cacheSolve(wrapperMatrix))
        print("This should equal the inverse matrix solved the usual way:")
	print(solve(matrix(2:5,2,2)))

}
