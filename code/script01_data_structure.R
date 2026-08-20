## data structure exercises 26/08/20
## scalar: single element in object. R does not distinguish between integers and numeric variables
a <- 2.0
b <- 4L
d <- "aquatic"

a
b
d

##vector: collection of same kinds of data. DO NOT MIX data types in vectors!
va <- c(1.0, 2.3, 3) # numeric vector
vb <- c("a", "b", "c") #character vector
vc <- c("1", "2", "3") #character because of the double quotes
vd <- c(1.0, "b") #this is what you should NOT do

va
vb
vc
vd

#matrices: 2-dimensional vectors 
va <- c(1.0, 2.3, 3)
vb <- c(3, 2, 5.6) #previous data is overwritten

ma <- cbind(va, vb) #cbind is combine by column. must be vectors of same length
mb <- rbind(va, vb)
mc <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

ma
mb
mc
