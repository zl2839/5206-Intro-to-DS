#' HW3: Reduce
#'
#' 1. Load the `purrr` package. Create a function `my_factorial(n)` which calculates 
#' the factorial of a number using `reduce`. You can assume the input will only be a 
#' natural number.
#'Note: DO NOT use `factorial` function from the base package.
library(purrr)
my_factorial(5)
library(purrr)
my_factorial = function(n){
  if (n == 0){r = 1}
  else{
    reduce(as.double(1:n),`*`)
  }
}
my_factorial(13)
#' 2. Create a function `compare_length(vec1, vec2)` that takes as input 2 vectors and 
#' returns the maximum length of them using `max`.

compare_length = function(vec1, vec2){
  retrun(max(length(vec1),length(vec2)))
}


#' 3. Create a function `max_length(l)` that takes as input a list of vectors and returns the maximum length among the vectors using `reduce` and `compare_length`. You can assume there will be at least two vectors inside the list.


max_length = function(l){
  return(reduce(l, compare_length))
}

#' 4. Implement `arg_max(x, f)`. It should take as input a vector `x` and a 
#' function `f`, and return a vector of the elements of `x` where the function returns the highest value.
#'    Example: `arg_max(-10:5, function(x) x ^ 2)` should return -10. 
#'    `arg_max(-5:5, function(x) x ^ 2)` should return c(-5, 5).
arg_max = function(x, f){
  max = reduce(f(x),max)
  x[f(x) == max]
}


#' 5. Implement `my_rle(x, f)` that takes as input a list `x` and a predicate function `f`,
#'  and returns a `rle` object that contains the lengths and values of runs of elements 
#'  where the predicate is true. Please use `rle` and `map_lgl`.
#'    Example: Given a list `list("a", "b", "c", 1, 2, 3)` and predicate function 
#'    `is.character`, this function should return a `rle` object with first element 
#'    `lengths: 3 3` and second element `values: TRUE FALSE`.

my_rle = function(x, f){
  rle(map_lgl(x,is.character))
}

x = list("a", "b", "c","h", 1, 2, 3,4, "d", "e", "f", "g",'e','r')
my_rle(x, is.character)

#' 6. Implement `span(x, f)` using `my_rle`. Given a list `x` and a predicate function `f`, 
#' `span(x, f)` returns the location of the first longest sequential run of elements 
#' where the predicate is true.
#'    Note: If f(x) is FALSE for every x in the list, return `NA`.
## Do not modify this line!

span <- function(x, f) {
  runs <- my_rle(x, f)
  # If no true values, returns NA
  if (sum(runs$values == TRUE) == 0) {
    return(NA)
  }
  # set values in FALSE position to NA
  false_pos <- which(runs$values == FALSE)
  r_l <- runs$lengths
  r_l[false_pos] <- NA
  # Find the max position
  max_pos <- which.max(r_l)
  # Find the start position of the longest elements
  pos <- ifelse(max_pos == 1, 1, sum(runs$lengths[1:(max_pos - 1)]) + 1)
  return(pos)
}
span(x,is.character)

#' HW3: reduce4
#'
#' 1. Load library `purrr`. Set the random seed as 5206. Save the random seed vector to `seed`.
#'    Use `map` to create a list `l1` with 5 elements, where each element is a vector of 20 random integers
#'    between `1:10` sampled with replacements (i.e., use the `sample` function and let `replace` be `TRUE`).
## Do not modify this line!
library(purrr)
set.seed(5206)
seed = .Random.seed

l1 = map(1:5,function(x) sample(1:10, size = 20,replace = TRUE))
l1

#' 2. Use `reduce` to get intersection of the 5 elements of `l1`, and assign the values to a vector `r1`.
#'    Similarly, use `reduce` to get union of `l1`, and store returned values to a vector `r2`.
## Do not modify this line!
r1 = reduce(l1,intersect)
r2 = reduce(l1,union)

#' 3. Create a data frame called `df1` with first column `A` and values `letters[1:5]`,
#'    second column `B` with values `1:5`, third column `C` with values `2:6`.
#'    Note that, when using `data.frame`, the first column will be automatically converted as a factor (i.e., do NOT use `stringsAsFactors = FALSE`).
#'    Then, use the `detect` function to find which column has factor values and save its result as `df1_factor`.
## Do not modify this line!
df1 = data.frame('A' = letters[1:5],'B' = 1:5, 'C' = 2:6)
df1_factor = detect(df1,is.factor)
df1_factor

#' 4. Similarly as in 3, create a data frame called `df2` with first column `A` and values `letters[1:5]`, second column `D` with values `3:7`, third column `E` with values `4:8`.
#'    Create yet another data frame called `df3` with first column `A` and values `letters[1:5]`, second column `G` with values `5:9`, third column `H` with values `6:10`.
#'    Again, do NOT use `stringsAsFactors = FALSE` when you create the two data frames.
#'    Then, combine three data frames into a list and call it `df_list`.
#'    Finally, use `reduce` and `merge` functions to bind all columns with different names.
#'    Store the returned data frame, which should be of dimension `5x7`, into `merged_df`.
## Do not modify this line!

df2 = data.frame('A' = letters[1:5],'D' = 3:7,'E' = 4:8)
df3 = data.frame('A' = letters[1:5],'G' = 5:9,'H' = 6:10)
df_list = list(df1,df2,df3)
df_list 		 
merged_df = reduce(df_list, merge)
merged_df		 









