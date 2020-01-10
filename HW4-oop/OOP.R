# HW4: Object Oriented Programming
#'
# In this exercise, we will create a class `shape` and a child class `rectangle`.
#'
# 1. Create a constructor `new_shape(x)` that takes in a string and instantiates
#    an object of class `shape` using `structure`. Don't forget to use `stopifnot` to check the input.
#    Create a helper function `shape(x)` that coerces the input
#    to a string with `as.character` and then calls the constructor.
## Do not modify this line!

new_shape <- function(x) {
  stopifnot(is.character(x))
  structure(x, 
            class = 'shape')
}
shape = function(x){
  x = as.character(x)
  new_shape(x)
}
# 2. Write a new `print` method for class `shape` that prints `"My name is <shape>."` using the function `print`.
#    The `<shape>` should be substituted by the string in the instance of `shape`.
#    Make sure that the method returns the object invisibly by adding `invisible(x)` at the end.
#    Create an instance of `shape` called `s1` of shape `unknown` and then print `s1`.
## Do not modify this line!
print.shape<-function(x,...)
{
  print(paste0("My name is ",x,'.'))
  invisible(x)
}
s1<-shape('unknown')
print(s1)


# 3. Write a generic function called `area(x)` that does nothing. Don't forget to use `UseMethod`.
#    Write a default method `area` that prints `"My area is not defined."` using the function `print`.
#    Test this new method using instance `s1`.
## Do not modify this line!

area = function(x){
  UseMethod("area", x)
}
area.default = function(x){
  print("My area is not defined.")
}

area.default(s1)
# 4. Create a constructor `new_rectangle(x, l, w)` that takes as input a string `x`,
#    and two numbers `l` and `w`, and instantiates an object `rectangle` of class `shape`.
#    Use the function `class()`. The constructor should give two attributes to `rectangle` :
#    `length`, equal to `l` and `width`, equal to `w`. Use `attr()` to create the attributes.
#    Use the line of code `rectangle <- function (x, l, w) new_rectangle(x, l, w)` to create a helper.
## Do not modify this line!
new_rectangle = function(x, l, w){
  attr(x, 'length') <- as.numeric(l)
  attr(x, 'width') <- as.numeric(w)
  class(x) <- c('rectangle', 'shape')
  x
}

rectangle <- function (x, l, w) new_rectangle(x, l, w)
attributes(new_rectangle('a',3,4))
new_rectangle('a',3,4)

# 5. Write a new method `area` for the class `rectangle` that prints `My area is <area>.` using `print`.
#    The `<area>` should be substituted by the area of a rectangle of length `length` and width `width`.
#    Use `attr()` to access these two parameters.
## Do not modify this line!
area.rectangle = function(x){
  area = attr(x,'length')*attr(x,'width') 
  print(paste0('My area is ',area, '.'))
}


# 6. Write a method `print` for the class `rectangle` that prints `"My name is <shape>.\nI am a rectangle."`
#    using `print.shape` and `print`. The `<shape>` should be substituted by the string in the instance of `rectangle`.
#    Make sure that the method returns the object invisibly by adding `invisible(x)` at the end.
## Do not modify this line!
print.rectangle = function(x,...){
  print.shape(x)
  print('I am a rectangle.')
  invisible(x)
}

# 7. Create an instance `r1` of `rectangle` with name `rect`, length 10, and width 5.
#    Call `area` and `print` method to instance `r1`.
## Do not modify this line!

r1 = rectangle('rect',10,5)
area(r1)
print(r1)


# HW4: Object Oriented Programming
#'
# In this exercise, we will create a class `distance` on top of integers and overload the function `mean()`.
#'
# 1. Create a constructor `new_distance(x, units)` where:
#     - `x` is a vector of numbers (not a vector of strings like `"ten"`),
#     - `units` is an attribute which can take values in `c("mm", "cm", "m", "km")`, with default value `"m"`,
#     - the object of class `distance` is instanciated using `structure`.
#    (Hint : Don't forget to use `stopifnot` to check the input type with `is.numeric` and `is.character`.)
#    Create a helper `distance(x, units)` that checks whether the input `unit` is in the feasible domain
#    `c("mm", "cm", "m", "km")` and throws an error if not.
#     (Hint : Use `match.arg()`.)
#    Write a new `print` method named `print.distance` for class `distance` that prints
#    `"the distance is <distance> <unit>"` when `<distance>` is a vector of length 1. 
#    It should print `"the distances are <distance> <unit>"` if 
#    `<distance>` is a vector of at least two elements.
#    For example, `print(distance(1:3, "cm"))` should print `"the distances are 1 2 3 cm"`.
#    Use the functions `cat()`, `paste0()`, and the function `attr()` to access the unit of the distance
#    and use `as.character` to convert the `distance` to a vector of characters,
#    and the function `attr()` to access the unit of the distance.
#    (Note: there is a space between `<distance>` and `<unit>` and no space at the end of the string.)
#    Make sure that the method returns the object invisibly by adding `invisible(x)` at the end.
#    The `"<distance>"` should be substituted by the number when instantiating `distance`.
#    The default unit should be `m`.
#    Create an instance of `distance` called `dist1` using `x = 10` and then print `dist1`.
## Do not modify this line!
new_distance <- function(x, units="m"){
  stopifnot(is.numeric(x))
  stopifnot(is.character(units))
  structure(x, class = "distance", units = units)
}
distance <- function(x, units="m"){
  match.arg(units, c("mm", "cm", "m", "km"))
  new_distance(x, units)
}

print.distance <- function(x, ...){
  units <- as.character(attr(x, 'units'))
  if (length(x)==1){
    print(paste0('the distance is ', x, ' ', units))
    #print(paste0(c("the distance is", as.character(x), attr(x, "units")), collapse=" "))
  }
  else{
    print(paste0('the distances are ', paste0(x, collapse=' '), ' ', units))
  }
  invisible(x)
}
dist1 <- distance(10)
dist1
print(distance(1:3, "cm"))


# 2. Create a function `to_mm(x, units)` that takes in a distance and converts it to the equivalent distance
#    in millimeters (i.e., it returns an object of class `distance`.
#    (Hint: Use `attr()` to access the units and `switch` to convert them.
#    Use `stop("Unknown unit.")` as default to specify the output when the input unit is not
#    in the feasible domain. Do not use `if` and `else`.
#    Use the helper function `distance()`to create the final object.)
#    Create an instance of `distance` called `dist1_converted` by converting `dist1` to millimeters.
## Do not modify this line!
to_mm <- function(x){
  x1 <- switch(attr(x, "units"),
               mm = x,
               cm = 10*x,
               m = 10^3*x,
               km = 10^6 *x,
               stop("Unknown unit."))
  distance(x1, "mm")
}
dist1_converted <- to_mm(dist1)
dist1_converted 

# 3. Write a new function `c.distance` for the class `distance` that combines its arguments to form a vector
#    of distances converted in millimeters.
#    (Hint: note that the argument to the `c()` generic is `...`, and you can use `list(...)`
#    to capture them. You can then use `sapply()` and `to_mm()` to first convert each distance in `list(...)`
#    the list into millimeters.)
#    Next, instantiate a new distance `dist2` of `20cm`.
#    Concatenate `dist1` and `dist2` together using `c()` and store the distance vector into `dist_both`.
## Do not modify this line!

c.distance <- function(...) {
  a <- list(...)
  a <- sapply(a, to_mm)
  distance(a, "mm")
}
dist2 <- distance(20, "cm")
dist_both <- c(dist1, dist2)
dist_both

# 4. Write a new `mean.distance` function for the class `distance`
#    that calculates the average distance of a distance vector in millimeters.
#    (Hint: in the implementation of the function, first convert all elements in the list to millimeters.
#    We can do that by simply call `to_mm(x)` because it is well defined for a distance object.
#    Then convert the distance variable to numeric by `as.numeric`.
#    Finally, since all the elements have been converted to numeric values,
#    we can simply calculate the average of the list by calling `mean(x)` and return an object of class
#    distance by calling the helper `distance()`.)
#    After you define function `mean.distance`, calculate the mean `dist_both` and store it into `avg`.
## Do not modify this line!
mean.distance <- function(x, ...) {
  distance((mean(as.numeric(to_mm(x)))), "mm")
}
avg <- mean.distance(dist_both)
avg

# HW4: oop weight
#'
# In this exercise, we will create a class `weight`.
#'
# 1. Create a constructor `new_weight(x = double(), units = "kg")` that:
#       - takes in a double `x` of length one,
#       - a `units` attribute,
#       - and initiates an object of class `weight` using `structure`.
#    Don't forget to use `stopifnot` to check that `x` has the correct type and length.
#    Because `units` might include `"lbs"` and `"kg"`, use `match.arg` to check the
#    validy of the second argument.
## Do not modify this line!

new_weight=function(x=double(),units="kg"){
  stopifnot(is.double(x))
  units=match.arg(units,c("lbs","kg"))
  structure(x,class="weight",units=units)
}
# 2. Create a helper function `weight` for `new_weight`,
#    which will convert `x` into a double using `as.double`.
## Do not modify this line!

weight=function(x=double(),units="kg"){
  x=as.double(x)
  new_weight(x,units=units)
}

# 3. Write a new `print` method for class `weight` that prints
#    `"The weight is <weight> <units>"` using `print`.
#    The `print` method should return the input invisibly.
#    Create an instance of `weight` called `d1` using `x = 50`,
#    `units = "lbs"` and then print `d1`.
## Do not modify this line!
print.weight = function(x,...){ 
  print(paste0("The weight is ",weight(x)," ",attr(x,"units")))
  invisible(x)
}
d1=weight(x=50,units="lbs")
print(d1)


# 4. Write a generic function called `convert(x)` :
#    If the units of `x` is `"lbs"`, it should return a weight object using `weight()`
#    corresponding to `x` converted to `"kg"`.
#    If the units of `x` is `"kg"`, it should return a weight object using `weight()`
#    corresponding to `x` converted to `"lbs"`.
#    For this exercise, assume that 1 kilogram corresponds to 2.2 pounds.
#    Call this new method to instance `d1`, save the return object into `d2`.
## Do not modify this line!
convert=function(x){
  UseMethod('convert')
}

convert.weight <- function(x) {
  y <- unclass(x)
  converted_unit <- ifelse(attr(x, "units") == "kg", "lbs", "kg")
  converted_weight <- ifelse(attr(x, "units") == "kg", y * 2.2, y / 2.2)
  weight(converted_weight, converted_unit)
}


d2=convert(d1)
d2

# 5. Write a `+` method for the `weight` class.
#    Note that arithmetic operators take two arguments: `x` and `y`.
#    Assume that the return units is the unit of `x`.
#    In other words, if `x` is in `"kg"`, then `x + y` should be an object of class
#    weight in `"kg"`. Take care of this by converting `y` to the right units if
#    necessary.
#    In this exercise, after `y` as been converted if necessary,
#    you should NOT use `unclass()`. Instead, delegate the work to the `+`
#    operators of the base type (`double`) using `NextMethod("+")`.
#'
## Do not modify this line!

`+.weight` <- function(x, y, z) {
  if (attr(x, "units") != attr(y, "units"))
    y <- convert(y)
  weight(NextMethod("+"), attr(x, "units"))
}
a = weight(3,'kg')
b = weight(3,'lb')
# HW4: Author and Book OOP
#'
# In this exercise we will create an `author` class to model a book's author.
#'
# 1. Create a constructor `new_author` for an `author` class to model a book's
# author.
# It should take as input two strings: `name`, `email`.
# It should check that the inputs are both strings (using `stopifnot`).
# Also check that the `email` contains a `@` sign (using `stopifnot` and
# `grepl`).
# The constructor should then create a named list with the two strings (first
# element is the name - called `"name"` and second element is the email -
# called `"email"`) and use `structure` with this list to create an object of
# class `author`.
## Do not modify this line!
new_author = function(name,email){
  stopifnot(is.character(name))
  stopifnot(is.character(email))
  stopifnot(grepl('@',email))
  author_info = list(name = name, email = email)
  structure(author_info, class = 'author')
}


# 2. Create a helper `author` that wraps `new_author`. It should have default
# values for `name` (`"John Doe"`) and `email` (`"unknown@unknown"``), return
# an error if the length of the arguments is not 1 and cast them to characters
# before calling `new_author` using `as.character`.
# Create an author object using the `author` helper with name `Susan Barker`
# and email `susan.barker@mail.com` and store it in variable `author_example`.
## Do not modify this line!

author = function(name = 'John Doe', email = 'unknown@unknown'){
  stopifnot(length(name) == 1)
  stopifnot(length(email) == 1)
  name = as.character(name)
  email = as.character(email)
  new_author(name,email)
}
author_example = author('Susan Barker','susan.barker@mail.com')
author_example
# 3. Create 2 functions `get_name` and `get_email` which take an `author`
# object named `author_object` as input: `get_name` should return the name
# of the author, `get_email` should return the email address of the author.
# Then, create a generic `change_email` which takes an `author` object named
# `author_object` along with a string `email` as inputs. Implement the method
# for objects of the class author (i.e., write `change_email.author`) so that
# the method returns a new `author` object with the name of the input `author_object`
# and the input email address.
## Do not modify this line!

get_name = function(author_object){
  author_object$name }

get_email = function(author_object){
  author_object$email
}

change_email = function(author_object, email){
  UseMethod('change_email')
}
change_email.author = function(author_object, email){
  author(name = author_object$name, email = email)	
}
change_email(author_example, 'hao@123.com')

# 4. Change the email address of `author_example` to `s.barker@mail.com`
## Do not modify this line!
author_example = change_email(author_example, 's.barker@mail.com')
author_example

# 5. Create a method `print.author` that prints the string
# `"Author <name>, e-mail: <email>"` where `<name>` and `<email>` are the
# strings corresponding to name and email of the `author` object. You should
# use the functions `print` and `paste0` to do so. Note that:
#    - the arguments of print are `x` and `...`, but `...` won't be used
#    in the body of `print.author`,
#    - print.author` method should return the first argument invisibly.
#'
## Do not modify this line!

print.author = function(x,...){
  print(paste0('Author ' ,x$name,', e-mail: ',x$email))
  invisible(x)
}
print(author_example)

# hw4_oop4
#'
# In this exercise, we will create a class `shakeshack_order`.
#'
# 1. Create a constructor `new_shakeshack_order(names, prices)` that:
#     - takes in a vector of `names`
#     - a vector of `price` attribute whose type is double.
#     - instanciates an object of class `shakeshack_order` using `structure`.
#     - and it should be a list with 2 elements: `names` and `prices`.
# Note: Use `stopifnot` to check the input.
# Use `new_shakeshack_order(names, prices)` to create a helper function `shakeshack_order`
# that coerces the arguments `names` and `prices` respectively to string and numeric
# using `as.character` and `as.double`.
## Do not modify this line!
new_shakeshack_order=function(names,prices){
  stopifnot(is.vector(names),is.double(prices),is.vector(prices))
  nini=list(names=names,prices=prices)
  nini=structure(nini,class="shakeshack_order")
  return(nini)
}
shakeshack_order=function(names,prices){
  names=as.character(names)
  prices=as.double(prices)
  new_shakeshack_order(names,prices)
}
new_shakeshack_order("ni",2)
shakeshack_order("ni",3)
# 2. Write a new `sum(..., na.rm = FALSE)` method for the class `shakeshack_order` that
# returns the sum of the prices in a given order. Note that:
#    - the `sum` generic can take more than one argument via `...`, and you can capture
#    it using `list(...)`.
#    - the `na.rm` argument should be used to provide a way to sum
#    when some prices are not available.
# For instance, the following code should work without error:
# ```
# o <- shakeshack_order(c("shack burger", "fries"), c(5, 2))
# o2 <- shakeshack_order(c("fries", "coke"), c(2, NA))
# sum(o)
# sum(o, o2)
# sum(o, o2, na.rm = TRUE)
# ```
# The first sum should be equal to 7, the second to `NA`, and the third to 9.
# Do NOT use a `for`, `while` or `repeat` loop!
# (Hint: a nice solution could use a combination of `map` and `reduce`.)
## Do not modify this line!
library(purrr)
sum.shakeshack_order=function(...,na.rm){
  sum(map_dbl(list(...),~sum(.x$prices,na.rm=na.rm)))  }
o = shakeshack_order(c("shack burger", "fries"), c(5, 2))
sum(o)
o2 <- shakeshack_order(c("fries", "coke"), c(2, NA))
# 3. Write a new `print` method for the class `shakeshack_order` that prints
# `"Your order is <names>. The total price is sum(<prices>)."` using `print`.
# If `length(names)` is larger than one (e.g., 3), the function should print
# `"Your order is <names[1]>, <names[2]>, <names[3]>. The total price is sum(<prices>)."`
# For instance, printing the order `o` describe above should output
# `"Your order is shack burger. The total price is $5.29."`.
# Note that:
#    - The `print` method should return the input invisibly.
#    - The arguments of print are `x` and `...`, but `...` won't be used in the
#    body of `print.shakeshack_order`.
## Do not modify this line!
a=as.vector(o$name)
print(a)
print.shakeshack_order=function(x,...){
  #print(paste0("Your order is ",as.character(x$name),". The total price is ",sum(x)))
  print( paste0("Your order is ",paste0(x$name,collapse=", "),". The total price is $",sum(x),"."))
  invisible(x)
}
print.shakeshack_order(o)
print.shakeshack_order(o2)
# 4. Now, you need to create a combine operator for the class `shakeshack_order`.
# For example, `c(o, o2)` should equal
# `shakeshack_order(names = c('shack burger', 'fries', 'fries', 'coke'), prices = c(5, 2, 2, NA))`.
# Similarly as for `sum.shakeshack_order`, the `...` argument of `c.shakeshack_order`
# can be captured using `list(...)`.
# Do NOT use a `for`, `while` or `repeat` loop!
# (Hint: a nice solution could use a combination of `map2` and `reduce`.)
#'
## Do not modify this line!
`c.shakeshack_order`= function(...){
  nini=list(...)
  a=unlist(map(nini,~.x$names))
  b=unlist(map(nini,~.x$prices))
  shakeshack_order(a,b)
}

c.shakeshack_order(o,o2)

# HW4: OOP_account
#'
# In this exercise, we will create a class `account`.
#'
# 1. Create a constructor `new_account(number)` that takes in a length 2 numeric
#    vector and initiates an object of class `account` using `structure`.
#    Class `account` should have an attribute `units` which is always the character vector `c('USD', 'EUR')`.
#    The value should be the amount of money in each currency.
#    Don't forget to use `stopifnot` to check if `number` is numeric or not and if the length of number is different from 2.
#    For example, `new_account(c(5, 0))` will create an account with 5 USD.
#    `new_account(c(0, 5))` will create an account with 5 EUR.
#    `new_account(c(5, 10))` will create an account with 5 EUR and 10 USD.
## Do not modify this line!

new_account = function(number){
  stopifnot(is.numeric(number))
  stopifnot(length(number) == 2)
  attr(number, 'units') = c('USD','EUR')
  structure(number, class = 'account')
}
new_account(c(5, 10))

# 2. Use `pmatch` to reate a function `get_unit_index(units)` to get the index of input `units` in `c('USD, 'EUR')`.
#    You are supposed to use this function in the following questions.
#    For example, `get_unit_index('EUR') = 2`. `get_unit_index('US') = 1`.
#    `get_unit_index('U','E') = c(1, 2)`.
## Do not modify this line!
get_unit_index = function(units){
  pmatch(units,c('USD','EUR'))
}
get_unit_index(c('E','U'))

# 3. Create a helper function `account(number, units)` that takes in a scalar or vector `number`
#    with corresponding `units` which initiates an object of class `account`.
#    `units` might include `EUR`, `USD`.
#    For example, `account(5, 'USD')` will create an account with 5 USD.
#    `account(5, 'EUR')` will create an account with 5 EUR.
#    `account(c(5, 10), c('EUR', 'USD'))` will create an account with 5 EUR and 10 USD.
#    Create an account with 100 USD and 100 EUR. Save it as `my_account`.
## Do not modify this line!
account = function(number, units){
  num = get_unit_index(units)
  if (length(num) == 2){
    if(num[1] == 2){
      number = c(number[2],number[1])
    }
    account = new_account(number)
  }
  else if(num == 1){
    account = new_account(c(number,0))
  }
  else{
    account = new_account(c(0,number))
  }
  return(account)
}
account <- function(number, units) {
  number <- as.numeric(number)
  units_index <- get_unit_index(units)
  stopifnot(!is.null(units_index))
  
  value_number <- rep(0, 2)
  value_number[units_index] <- number
  new_account(value_number)
}

my_account = account(c(100,50), c('EUR', 'USD'))
my_account

# 4. Creat two generic functions `deposit(account, number, units)` and `withdraw(account, number, units)`
#    that takes in a scalar or vector `number` with corresponding `units`.
#    `units` might include `EUR`, `USD`.
#    Methods `deposit.account` and `withdraw.account` should return an object of class `account` with correct amounts.
#    Deposit 50 USD and withdraw 50 EUR for `my_account`.
## Do not modify this line!
library(purrr)
deposit = function(account, number, units){
  UseMethod('deposit')
}

deposit.account = function(account, number, units){
  num = get_unit_index(units)
  account[num] = account[num] + number
  account
  
}
withdraw = function(account, number, units){
  UseMethod('withdraw')
}
withdraw.account = function(account, number, units){
  num = get_unit_index(units)
  account[num] = account[num] - number
  account
}
my_account = my_account %>% deposit(50, units = "USD") %>% withdraw(50, units = "EUR")

# 5. Write a new `summary` method for class `account`. `summary(account, units)` should return a named vector
#    which has the same length and names as `units`.
#    The reurn value should be the total amount of the `account` in `units`.
#    For simplicity, 1 EUR = 1.1 USD.
#    For example, `summary(my_account, c('USD', 'EUR'))` should return a named vector with values `c(150, 50)`.
#    `summary(my_account, 'USD')` should return a named vector with value `205`.
## Do not modify this line!

summary.account <- function(account, units) {
  num = get_unit_index(units)
  if(length(num) == 2){
    r = account[num]
    names(r) = units
  }
  else if(num == 1) {
    r <- account[1]+1.1*account[2]
    names(r) <- c('USD')
  }
  else {
    r <- account[1]/1.1+account[2]
    names(r) <- c('EUR')
  }
  return(r)
}
summary(my_account, 'USD')
summary(my_account, c('EUR','USD'))
# 6. Now let's consider a subclass `minimum_balance_account` which should maintain a pre-determined minimum balance.
#    Create a constructor `new_minimum_balance_account(number, minimum)` that takes in legnth 2 numeric vectors and
#    initiates an object of subclass `minimum_balance_account` using `structure`.
#    Subclass `minimum_balance_account` should have an attribute `units` which is always
#    the character vector `c('USD', 'EUR')` and an attribute `minimum` which is a numeric vector.
#    The value should be the amount of money in each currency.
#    Don't forget to use `sopifnot` to check if `number` and `minimum` are numeric or not.
#    For example, `new_minimum_balance_account(c(5, 0), c(1, 0))` will create an account
#    with 5 USD and minimum balance 1 USD.
## Do not modify this line!
new_minimum_balance_account <- function(number, minimum) {
  stopifnot(is.numeric(number))
  stopifnot(length(number)==2)
  stopifnot(is.numeric(minimum))
  stopifnot(length(minimum)==2)
  attr(number,'units') <- c('USD','EUR')
  attr(number,'minimum') <- minimum
  structure(number, class = 'minimum_balance_account')
}
new_minimum_balance_account(c(5, 0), c(1, 0))
# 7. Create a helper function `minimum_balance_account(number, units, minimum, minimum_units)`
#    that takes in a scalar or vector `number` and `minimum`
#    with corresponding `units` and `minimum_units` which initiates an object of subclass `minimum_balance_account`.
#    `units` and `minimum_units` might include `EUR`, `USD`.
#    For example, `minimum_balance_account(5, 'USD', 1, 'USD')` will create an account
#    with 5 USD and minimal balance 1 USD.
## Do not modify this line!
minimum_balance_account <- function(number, units, minimum, minimum_units) {
  n1 <- c(0, 0)
  for (i in 1:length(number))
  {
    n1[get_unit_index(units[i])] <- number[i]
  }
  n2 <- c(0,0)
  for (i in 1:length(minimum))
  {
    n2[get_unit_index(minimum_units[i])] <- minimum[i]
  }
  new_minimum_balance_account(n1, n2)
}

minimum_balance_account(5, 'USD', 1, 'USD')
# 8. Add method for `withdraw` and modify `deposit.account` if needed such that:
#    a. The returned value should be the same class of input `account`.
#    b. Function `deposit` can accpet `minimum_balance_account` and return correct object.
#    c. If withdraw will cause the balance lower than the minimum balance,
#       report an error 'Minimum balance must be maintained.'.
#    d. Use `withdraw.account` in `withdraw.minimum_balance_account`.
## Do not modify this line!

withdraw.minimum_balance_account <- function(account, number, units) {
  account <- withdraw.account(account, number, units)
  position <- map(units, get_unit_index) %>% reduce(c)
  if(any(account[position] < attr(account, 'minimum')[position])) {
    stop('Minimum balance must be maintained.')
  }
  account
}


