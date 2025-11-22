## Kevin Lynch HW5 Class/Methods

#' @import methods
#' @importFrom stats sd
NULL

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

setValidity('sparse_numeric', function(object) {
  if (length(object@pos) != length(object@value)) {
    return('The length of the positions and values do not match')}
  if (any(object@pos < 1) | any(object@pos > object@length)) {
    return('The positions are not within the length')}
  TRUE})

#' Add two sparse vectors together
#'
#' This function combines two sparse vectors of the same length, and then returns a sparse vector of the sum
#' @param first The first sparse vector
#' @param second The second sparse vector, of the same length as the first
#' @param ... Not Necessary
#' @return Sparse vector of the Sum
#' @export
setGeneric('sparse_add', function(first,second,...) {standardGeneric('sparse_add')})

#' Multiply two sparse vectors together
#'
#' This function combines two sparse vectors of the same length, and then returns a sparse vector of their product
#' @param first The first sparse vector
#' @param second The second sparse vector, of the same length as the first
#' @param ... Not Necessary
#' @return Sparse vector of the product of the first vector x the second vector
#' @export
setGeneric('sparse_mult', function(first,second,...) {standardGeneric('sparse_mult')})

#' subtract one sparse vector from another
#'
#' This function subtracts one sparse vector, of the same length, by another sparse vector, and then returns the remainder left of the original sparse vector
#' @param first The first sparse vector
#' @param second The second sparse vector, of the same length as the first, that is subtracting from the first
#' @param ... Not Necessary
#' @return Sparse vector of the result after subtraction
#' @export
setGeneric('sparse_sub', function(first,second,...) {standardGeneric('sparse_sub')})

#' cross product of two sparse vectors
#'
#' This function calculates the cross product of two sparse vectors, and returns the numeric sum of the sparse vector's cross product
#' @param first The first sparse vector
#' @param second The second sparse vector, of the same length as the first
#' @param ... Not Necessary
#' @return numeric vector of a length of 1
#' @export
setGeneric('sparse_crossprod', function(first,second,...) {standardGeneric('sparse_crossprod')})

#' @rdname sparse_add
setMethod('sparse_add', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  both_pos <- c(first@pos, second@pos)
  both_values <- c(first@value, second@value)

  added <- tapply(both_values, both_pos, sum)

  non_zero <- added[added != 0]
  new('sparse_numeric', value = as.numeric(non_zero), pos = as.integer(as.numeric(names(non_zero))), length = first@length) # Creates sparse numeric
})

#' @rdname sparse_mult
setMethod('sparse_mult', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  both_combined <- data.frame(pos = c(first@pos, second@pos), value = c(first@value, second@value))

  both_pos <- intersect(first@pos, second@pos)

  mult <- first@value[match(both_pos, first@pos)] *
    second@value[match(both_pos, second@pos)]

  new('sparse_numeric', value = mult, pos = both_pos, length = first@length) # Creates sparse numeric
})

#' @rdname sparse_sub
setMethod('sparse_sub', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  both_pos <- c(first@pos, second@pos)
  both_values <- c(first@value, -second@value)

  sub <- tapply(both_values, both_pos, sum)

  non_zero <- sub[sub != 0]
  new('sparse_numeric', value = as.numeric(non_zero), pos = as.integer(as.numeric(names(non_zero))), length = first@length) # Creates sparse numeric
})

#' @rdname sparse_crossprod
setMethod('sparse_crossprod', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  both_combined <- data.frame(pos = c(first@pos, second@pos), value = c(first@value, second@value))

  both_pos <- intersect(first@pos, second@pos)

  crossprod <- first@value[match(both_pos, first@pos)] *
    second@value[match(both_pos, second@pos)]

  return(c(sum(crossprod))) # Creates numeric vector
})

#' Add two sparse vectors together
#'
#' This function combines two sparse vectors of the same length, and then returns a sparse vector of the sum
#' @param e1 The first sparse vector
#' @param e2 The second sparse vector, of the same length as the first
#' @return Sparse vector of the Sum
#' @export
setMethod('+', c('sparse_numeric', 'sparse_numeric'), function(e1, e2) {sparse_add(e1, e2)})

#' Multiply two sparse vectors together
#'
#' This function combines two sparse vectors of the same length, and then returns a sparse vector of their product
#' @param e1 The first sparse vector
#' @param e2 The second sparse vector, of the same length as the first
#' @return Sparse vector of the product of the first vector x the second vector
#' @export
setMethod('*', c('sparse_numeric', 'sparse_numeric'), function(e1, e2) {sparse_mult(e1, e2)})

#' subtract one sparse vector from another
#'
#' This function subtracts one sparse vector, of the same length, by another sparse vector, and then returns the remainder left of the original sparse vector
#' @param e1 The first sparse vector
#' @param e2 The second sparse vector, of the same length as the first, that is subtracting from the first.
#' @return Sparse vector of the result after subtraction
#' @export
setMethod('-', c('sparse_numeric', 'sparse_numeric'), function(e1, e2) {sparse_sub(e1, e2)})

setAs('numeric', 'sparse_numeric', function(from) {
  if (!is.numeric(from)) {stop()}
  non_zero <- which(from != 0) # Removes 0s
  new('sparse_numeric', value = from[non_zero], pos = as.integer(non_zero), length = length(from))
})

setAs("sparse_numeric", "numeric", function(from) {
  numeric_vector <- numeric(from@length) # Makes vector of 0
  numeric_vector[from@pos] <- from@value # Replaces 0s with values
  numeric_vector})

#' shows a sparse vector
#'
#' This function outputs a sparse vector as a dataframe
#' @param object The first sparse vector
#' @return dataframe of the sparse vector
#' @export
setMethod('show', 'sparse_numeric', function(object) {
  df <- data.frame(pos = object@pos, value = object@value)
  cat(df)})

#' plots two sparse vectors
#'
#' This function plots the overlapping non-zero elements of two sparse vectors, returning the plot
#' @param x The first sparse vector
#' @param y The second sparse vector, of the same length as the first
#' @return plot
#' @export
setMethod('plot', c('sparse_numeric', 'sparse_numeric'), function(x, y) {
  if (x@length != y@length) {stop()}

  both_pos <- intersect(x@pos, y@pos)

  plot(x@value[match(both_pos, x@pos)], y@value[match(both_pos, y@pos)],
       xlab = 'first sparse numeric vector',
       ylab = 'second sparse numeric vector',
       main = 'overlapping non-zero elements of two sparse_numeric vectors')
})

#' divides a sparse vector by another sparse vector
#'
#' This function returns the product of dividing one sparse vector by another sparse vector of the same length
#' @param first The first sparse vector
#' @param second The second sparse vector, of the same length as the first
#' @param ... Not Necessary
#' @return Sparse vector
#' @export
setGeneric('sparse_div', function(first, second,...) {standardGeneric('sparse_div')})

#' @rdname sparse_div
setMethod('sparse_div', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}

  both_pos <- intersect(first@pos, second@pos)

  div <- first@value[match(both_pos, first@pos)] /
    second@value[match(both_pos, second@pos)]

  new('sparse_numeric', value = div, pos = both_pos, length = first@length) # Creates sparse numeric
})

#' Mean of a sparse vector
#'
#' This function takes the mean of a sparse vector, accounting for the 0s not listed in the sparse vector
#' @param x The sparse vector
#' @param ... Not Necessary
#' @return numeric number
#' @export
setGeneric("mean", function(x, ...) standardGeneric("mean"))

#' @rdname mean
setMethod('mean', 'sparse_numeric', function(x) {
  (sum(x@value) / x@length)
})

#' computes the norm of a sparse vector
#'
#' This function normalizes a sparse vector, by computing the squared norm of the vector
#' @param x The sparse vector
#' @param ... Not Necessary
#' @return numeric number
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' @rdname norm
setMethod('norm', 'sparse_numeric', function(x) {
  sqrt(sum((x@value * x@value)))
})

#' standardizes a sparse vector
#'
#' This function standardizes a sparse vector, by taking each element of the vector and subtracting off the vector mean and dividing by the vector standard deviation.
#' @param first The sparse vector
#' @param ... Not Necessary
#' @return sparse vector
#' @export
setGeneric("standardize", function(first, ...) {standardGeneric("standardize")})

#' @rdname standardize
setMethod('standardize', 'sparse_numeric', function(first) {
  dense_vector <- as(first, 'numeric')
  standardized <- (dense_vector - mean(dense_vector))/sd(dense_vector) # Standardizes the vector
  sparse_standardized <- standardized != 0 # True or False if the value is 0
  new('sparse_numeric', value = standardized[sparse_standardized], pos = which(sparse_standardized), length = first@length)
})
