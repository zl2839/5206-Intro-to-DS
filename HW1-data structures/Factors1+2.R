# HW1: factor8
#
# 1. Create an ordered factor `f1` consist of letters 'a' to 'z' ordered alphabetically.
f1 = factor(letters[1:26],ordered = TRUE)
f1
# 2. Create an ordered factor `f2` consist of letters 'a' to 'z' in descending alphabetical order.
f2 = factor(f1,levels = rev(levels(f1)))
f2
# 3. Create a 30 elements, ordered factor `f3` consist of letters 'a' to 'z' followed by 4 NA. The order of `f3` is 'a'<...<'z'<NA.
f3 = factor(c(letters[1:26], NA,NA,NA,NA),levels = c(letters, NA), exclude = NULL, ordered = TRUE)
f3
# 4. Delete the element 'c' with the level 'c' and assign it to `f4`.
f4 = factor(f3[-(3)],levels = c(levels(f3)[-3]),exclude = NULL, ordered = TRUE)
f4
#solution
f4 <- f3[-3]
f4 <- droplevels(f4)

# how about drop level first?
# or built a factor without certain level?
f = factor(letters[1:26],letters[1:25])

# 1. Create two factors `f1` and `f2` containing the numbers from 1 to 100 included, and 1 to 20 included.
f1 = factor(1:100)
f2 = factor(1:20)


# 2. Concatenate these two factors in a factor `f3`.
#exclude ????????????
f3 = factor(c(f1, f2))
f3

# 3. Create a table `t1` giving the number of times each number appear in `f3`.
t1 = table(f3)
t1

# 4. Copy `f1` into a factor `f4` and rearrange it in decreasing order. (hint: make sure its levels are in decreased order too. You might want to look up the function `rev()`)
f4 = factor(rev(f1),levels = rev(levels(f1)))




