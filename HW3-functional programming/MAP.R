#' HW3: Map
#'
#' In this exercise, we will use the different `map` functions from the `purrr` package.
#' DO NOT use `while` loop in this exercise.
#'
#' 1. Load the `purrr` package. For each column in the `mtcars` data set, calculate the mean using `map`. Store the results in a list `m1`.
## Do not modify this line!
library(purrr)
m1 = map(mtcars, mean)
m1


#' 2. Do the same thing as above, but return a vector named `m2` using `map_dbl`.
## Do not modify this line!
m2 = map_dbl(mtcars,mean)
m2


#' 3. Calculate the mean once again, trimming the 10% maximal and minimal values using `map_dbl`. Save the result in a vector named `m3`.
## Do not modify this line!
m3 = map_dbl(mtcars, mean,trim = 0.1)
m3


#' 4. Split `mtcars` by `cyl` into a list and calculate the number of rows for each element of the list using `split` and `map_int`. Store the result in a vector named `m4`.
#' Note: Do not change column names.
## Do not modify this line!

m4 <- split( mtcars , f = mtcars$cyl )
m4
m4 = map_int(m4, nrow)
m4

#Map 2
#' 1. Use `map`, `keep` and `summary` function to calculate the summary statistics of each column containing numeric data, save the answer into a list `m1`.
#'    The length of the list `m1` should be the same as the number of numeric columns. Each element in `m1` corresponds to the summary of each numeric column.
## Do not modify this line!
library(purrr)
head(iris)
m1 = map(iris,summary)
m1 = m1 %>% keep(function(x) mean(x) != 50) # ????????????????????????
m1 <- map(keep(iris, is.numeric), summary) #??????
m1
#' 2. Use `map_dbl` , `keep` and `mean` to calculate the mean value for each numeric column with max/min 5% values trimmed, save the answer into a vector `m2`.
#'    Hint: use `trim` argument in `mean` function.
## Do not modify this line!
m2 = keep(iris, colnames(iris) != 'Species')
m2 = map_dbl(m2, mean,trim = 0.05)
m2 <- map_dbl(keep(iris, is.numeric), mean, trim = 0.05)#??????

#' 3. Now, use `map2_dbl` to calculate the trimmed mean with trim level `0.01`, `0.05`, `0.1` and `0.2` respectively for column `Sepal.Length`, `Sepal.Width`, `Petal.Length` and `Petal.Width`.
#'    Save the answer into a vector `m3`.
## Do not modify this line!
m2 = keep(iris, colnames(iris) != 'Species')
trim1 = list(0.01,0.05,0.1,0.2)				 
m3 = map2_dbl(m2,trim1,mean)

#' 4. Use `detect_index` and `map_dbl` to detect the numeric column with 
#' variance greater than `3`, save the index number into `select_var` and 
#' the corresponding column name into `m4`.
#' Try to write all the answers in one line.
## Do not modify this line!
select_var = detect_index(map_dbl(keep(iris, is.numeric),var),function(x) x>3)
m4 = colnames(iris)[select_var]


#' HW3: map3
#'
#' Consider the `diamonds` dataset from the `ggplot2` package. For this exercise, you are not allowed to use `for`, `while`, or `repeat` loops.
#'
#' 1. Find the highest price for each color in the dataset using function `map_dbl` and `split`. Save the result to an atomic vector `max_price`.
## Do not modify this line!
library(ggplot2)
library(purrr)
head(diamonds)
max_price = split(diamonds$price , f = diamonds$color ) %>% map_dbl(max)
max_price
#' 2. Describe the highest price for each color using the following format: 'The highest price of color D is 18693'. Save the sentences in a character vector `max_price_sentence`.
#'    You are required to use function `imap_chr` in your solution.
## Do not modify this line!
max_price_sentence = imap_chr(max_price, ~ paste0('The highest price of color ', .y,' is ', .x))
max_price_sentence

#' 3. Copy `diamonds` to a tibble `t1` and replace every row by the first row using function `modify`.
## Do not modify this line!
library(tibble)
library(tidyverse)
t1 = as.tibble(diamonds)
t1 = modify(t1, ~.x[1])
t1

#' 4. Copy `diamonds` to a tibble `t2` and replace all numeric columns of each row by the mean value of every numeric column.
#'    You are required to use function `modify_if` in your solution.
## Do not modify this line!
t2 = as.tibble(diamonds)
t2 = modify_if(t2, is.numeric, mean)
t2



#Map4
#' 1. Load the `purrr` package. Compute the standard deviation of every column in `mtcars` and store the result
#' in a variable `mtcars_sd` using `map_dbl()`.
## Do not modify this line!

library(purrr)
mtcars_sd = map_dbl(mtcars, sd)
mtcars_sd
#' 2. Copy data `mtcars` into `mtcars_factor`. Using `as.factor` and `modify_at`, change the column `cyl` and `gear`
#' of `mtcars_factor` from type `numeric` to type `factor`.
## Do not modify this line!

mtcars_factor = modify_at(mtcars, c("cyl", "gear"), as.factor)
mtcars_factor
#' 3. Compute the standard deviation of every `numeric` column of `mtcars_factor` and store the result into a mixed data
#' frame called `mtcars_numeric_sd` using `map_if()` and `is.numeric`.
## Do not modify this line!
mtcars_numeric_sd = map_if(mtcars_factor, is.numeric, sd)

#' 4. Compute the number of levels in each `factor` column of `mtcars_factor` and store the result into a mixed data frame
#' called `mtcars_factor_level` using `map_if()` and `is.factor`.
#' (Hint: to get of levels in factor `x`, use `levels(x)`; to get the unique elements of a list `x`, use `unique(x)`;
#' to get the length of a list `x`, use `length(x)`)

mtcars_factor_level = map_if(mtcars_factor, is.factor, nlevels)
mtcars_factor_level <- map_if(mtcars_factor, is.factor, function(x) length(unique(levels(x))))

#Map 5
#' 1. Load the `purrr` and `repurrrsive` packages. Use `map_chr` to extract all the `name` from `got_chars`, store them into a vector of characters called `name_list`.
#'    Then, use `map` to extract all the `aliases` from `got_chars`, store them into a list called `nick_name_list`.

library(purrr)
library(repurrrsive)
name_list = map_chr(got_chars,'name')
nick_name_list = map(got_chars,'aliases')

#' 2. Make a function called `hello` with argument `x` (e.g., `hello(x)`). 
#' Inside the `hello` function, use `paste` to concatenate `Hello `, `x` 
#' and `!\n`, the `sep` should be empty.
#'Use `map_chr` and `hello` functions to save sentences like `Hello ` + name in name_list + `!\n` into a vector `hello_sentences`.

hello = function(x){
  paste('Hello ', x, '!\n', sep='')
}
hello('eds')
hello_sentences = map_chr(name_list, hello)

#' 3. Use `map_chr` and `paste` functions to add ` or ` to collapse aliases of each person. Store returned values into list `nick_name_list_update`.
#'    For instance, the first element will be `"Prince of Fools or Theon Turncloak or Reek or Theon Kinslayer"`
## Do not modify this line!
nick_name_list_update = map_chr(nick_name_list,function(x) paste(x, collapse = ' or '))
map_chr(nick_name_list,~paste(.x, collapse = ' or '))
map_chr(nick_name_list,paste,collapse = ' or ')

#' 4. Use `map2_chr` on `name_list` and `nick_name_list_update` along with `paste` to insert `can be called` when mapping.
#'    For instance, if the first element of `name_list` is `"A"`, and the corresponding element of `nick_name_list_update` is `"B or C"`,
#'    you are supposed to get `"A can be called B or C"`.
#'    Store the resulting vector of characters into `sentences_list`.
## Do not modify this line!
sentences_list = map2_chr(name_list,nick_name_list_update, ~ paste0(.x,' can be called ', .y))
sentences_list


