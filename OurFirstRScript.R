## ---- Data import ----

# using the read.csv function
y <- read.csv("data/captures.csv",sep=";") 
class(y) # this is a data frame
head(y) # we can see the first rows of our data frame

# or from an excel file:
# we need a specific package in this case, and we need to install it:
# install.packages("xlsx") # the line is commented because you just need to run it once
library(xlsx)
# read in the worksheet named mysheet
mydata <- read.xlsx("data/database_esercizio.xls", sheetName = "captures")
class(mydata)
head(mydata)


## ---- Extract a column (i.e. a variable) -----
# we can do this using '$'
y$weight_g
# or subsetting the data frame, using square brackets '[ ]'
y["weight_g"]
y[,10] # the weight of the animals is stored in the 10th column
# Let's ask R what kind of variable we are dealing with, using the 'class' function again
class(y$weight_g)

w <- y$weight_g
w

# repeat using the data frame loaded from the Excel file
mydata$weight_g
class(mydata$weight_g) # why?



## ---- Plot the data ----

# histogram
# the function to plot an histogram is 'hist'
# let's see how it works
??histogram
?hist # this is the simplest way to get help in R! just a question mark!
hist(y$weight_g, main="", xlab="Animal weight (g)") # with default break
hist(y$weight_g, breaks=30, main="", xlab="Animal weigth (g)") # we specified a single number giving
# the number of cells for the histogram

# dotplots (or stripcharts)
# op <- par(mfrow=c(1,3))
?stripchart
stripchart(y$weight_g, xlab="Animal weigth (g)")
stripchart(y$weight_g, xlab="Animal weigth (g)", method="jitter")
stripchart(y$weight_g, xlab="Animal weigth (g)", method="stack")
# par(op)

# boxplot
boxplot(y$weight_g, ylab="Animal weigth (g)")
boxplot(y$weight_g ~ y$sex + y$age, ylab="Animal weigth (g)")
# exercise (by your own): do the same with the foot lenght
boxplot(y$footlength_mm, ylab="Foot length (mm)")

# RIPRENDERE DA QUI IL 5 APRILE 
## ---- Central tendency measures ----

## the mean and the median
weight <- na.omit(y$weight_g)
# mean
mean(weight)
mean(y$weight_g)
# median
median(weight)
hist(weight,prob=T,ylim=c(0,0.05)) # prob=T for relative frequencies, probability, number over total (density),ylim -> limits of axys y since values are small, the area of the graph is 1 not sum of frequencies we read 
lines(density(rnorm(1000000,mean(weight),sd(weight))),col="red")
segments(mean(weight),0,mean(weight),0.047,col="blue")
segments(median(weight),0,median(weight),0.047,col="green")

## the mode
# R does not have a standard in-built function to calculate mode.
# So we create a user function to calculate mode of a data set in R.
# This function takes the vector as input and gives the mode value as output.
# Create the function.
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
} #same function as mode cuz it asks which value is most frequent in the weight vector
getmode(weight)


## ---- Dispersion measures ----

## range
range(weight) #this is min and max

## quantile
quantile(weight) # in R, quartiles are the default for the quantile function
median(weight) #same as 50% quantile
?boxplot # check the range argument and its default value, sends me to help
boxplot(weight, range=0)
boxplot(na.omit(y$footlength_mm))
boxplot(na.omit(y$footlength_mm), range=0) #includes outliers in the graph range (shape is same, but dotted lines go further)

# summary, lots of info in the vector (min-max,mean-median,1st and 3rd quartile)
summary(weight)

## variance, scarto quadratico medio rispetto alla media (ogni dato -media, elevato alla seconda e poi sommati)
var(weight)

## standard deviation, varianza ma alla radice quadra (i dati sono per lo più tra media +/- deviazione standard)
sd(weight)

# IGNORE why we square the differences?
m <- mean(weight)
w <- c(m+4,m+4,m-4,m-4)
op <- par(mfrow=c(1,2))
plot(w, pch=19, col="dark grey", ylim=c(22,40))
abline(h=mean(w),col="blue")
for (i in 1:length(w)) {
  segments(i,w[i],i,mean(w),col="red",lty=2)
}
differences <- w-mean(w)
differences
# sum the differences from the mean, and divide by the number of elements:
(sum(differences))/length(differences) # the negatives cancel the positives
# as the sign of differences seems to be a problem, we try and use absolute values:
sum(abs(differences))/length(differences) # this is the mean deviation
# let's try with another vector, same mean but more spread differences
w1 <- c(m+7,m+1,m-6,m-2)
plot(w1, pch=19, col="dark grey", ylim=c(22,40))
abline(h=mean(w1),col="blue")
for (i in 1:length(w1)) {
  segments(i,w1[i],i,mean(w1),col="red",lty=2)
}
par(op)
differences.w1 <- w1-mean(w1)
sum(abs(differences.w1))/length(differences.w1) # this is the mean deviation
# if we finally square the differences, the standard deviation is bigger when the differences are more spread out
sqrt(sum(differences^2)/length(differences))
sqrt(sum(differences.w1^2)/length(differences.w1))
## using R functions
var(w)
var(w1)
sd(w)
sd(w1)
# STOP IGNORING the results are slightly different because the R functions adopt a correction for finite samples

## standard error
sd(weight)/sqrt(length(weight)) #length(weight) counts number of data in vector for you
sd(weight)/sqrt(119) #119 is number of data in the vector, but you have to count it yourself


## ---- Outliers ----

# boxplot
op <- par(mfrow=c(1,2)) #default is one graph, this says 1 row but 2 colums of graphs, so two graphs visualised together
boxplot(y$footlength_mm, col = "lightgray", ylim=c(10,30))
# boxplot(log(y$footlength_mm), col = "lightgray")
mtext("80", line=-1) #because of the limit this data was eliminated, adding it manually
points(x=29) #add point in chart manually for aesthetic

# points()


# Cleveland plot/dotchart
dotchart(y$footlength_mm) #highlights outliers
par(op) #get back to only one graph
par(mfrow=c(1,1))
# identify the outlier
plot(x=y$footlength_mm, y=y$capture_id) #grafico personalizzato, plot is base graph function
identify(x=y$footlength_mm, y=y$capture_id) #interactive, you can go press on points of the graph and R tells you their position in the vector (press esc to stop interactive mode)
# press Esc to stop the identify stuff
y[102,] #read this row (adding row, blank is saying no column, just the row)

y$weight_g
(y$weight_g)[y$weight_g > 30] #seleziona sotto insieme, quadre sono selezione, ma possono avere un comando


## ---- Handling data in a data frame - common operations ----

# selecting subsets of data, according to their position or name
y[1,] # one row (the first one)
y[1:10,] # first ten rows
y[5:10,] # from 5th to 10th row
y["29",] # by row name, useful if we removed some rows from the dataset, cuz even if by counting it is not the 29th anymore, it is still called that
y[c("29","45"),] # by row name, more than a row, c is a base function (concatena, just shows them side by side)

y[,1] # one column (the first one)
y[,1:10] # first ten columns
y[,5:10] # from 5th to 10th column
y[,"chip"] # by column name
y[,c("chip","trap_id")] # by column name, more than a column, 

y[1:5,c("chip","trap_id")] #combines reading columns and rows

# selecting subsets of data, according to their values
library(dplyr)
library(tidyverse) #this has a lot more stuff than what we mainly need
filter(y, trap_id > 43) #select in your columns just certain rows BUt by value and not position
# or
y %>% # %>% can be typed as ctrl shift M, %>% says to get data from dataframe y and you can use it for more than just one command so you don't have to always specify it IN the ohter commands
  filter(trap_id > 43) 
filter(y, trap_id > 43 & occasion < 20) #add columns, & is "and"
filter(y, trap_id < 5 | trap_id > 65) #add criteria to same column, | is "or"
arrange(y, trap_id) #rows are now rearranged so that trap id is in ordine crescente

# multiple operations
y[,c("chip","trap_id")] %>% filter(trap_id > 65) %>% arrange(trap_id)
select(y, chip, trap_id) #extracts them rows and columns in said dataframe
y %>% 
  select(chip, trap_id) %>% #gotta repeat pipe %>% to tell R to keep it up
  filter(trap_id > 65) %>% 
  arrange(trap_id) # now everything is done in one click of the run button

# summarizing data within groups
names(y)
y$age
y %>% group_by(age) %>% summarise(mean.w = mean(na.omit(weight_g))) #so basically I get mean of weights for each age group, like mean for A and mean for G, summarise needs to be there to work so R knows to create a little tabella with a column named mean.w where the new info goes, "crea tabella di sintesi con questo titolo nella clonna in cui devi eseguire queste funzioni", WRITE IT
y$sex
y %>% group_by(age, sex) %>% summarise(mean.w = mean(na.omit(weight_g))) #you can add criteria to the groups, this combines them both
