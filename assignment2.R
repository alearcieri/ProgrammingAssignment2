makeCacheMatrix <- function(x = matrix())
{
	i <- NULL
	set <- function(y)
	{
		x <<- y
		i <<- NULL
	}
	
	get <- function () x
	setmatrix <- function(inverse) i <<- inverse
	getmatrix <- function() i
 	
	list(set=set, get=get, setmatrix=setmatrix, getmatrix =getmatrix)
}

cacheSolve <- function(x, ...)
{
	i <- x$getmatrix()
	if(! is.null(i))
	{
		message("getting cached data")
		return(i)
	}	
	message("caching the data") #displays message when data is being cached, ie only in the first run.
	data <- x$get()
	i <- solve(data, ...)
	x$setmatrix(i)
	i
}


