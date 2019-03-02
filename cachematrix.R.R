makeCacheMatrix <- function(x = matrix()) {

  #Guarda en caché la inversa de una matriz:
 
  # El calculo de la matriz inversa suele requerir de recurso maquina y puede haber algunos
  # beneficios al almacenar en caché la inversa de una matriz en lugar de hacerlo repetidamente.
  # Esta funcion crea una matriz especial, que guarda su inversa

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##CacheSolve calcula lainversa de la matriz.

CacheSolve <- function(x, ...) {
  ## devuelve la matriz de 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
