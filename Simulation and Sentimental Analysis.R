# Do not remove any of the comments. These are marked by #

# HW 7 - Due Monday Apr 16, 2018 in moodle and hardcopy in class.

# (1). Please upload R code and report to Moodle 
#      with filename: HW7_IS457_YourCourseID.
# (2). Turn in hard copy of your report in class.

#################   Part 1: Simulation   ##############################################

## Let X and Y be two random variables following normal distribution.
## We will use the simulation techniques to find the distribution of X+Y.

#(a)
# As we will generate random number, to ensure reproducibility, please set the seed as 457. (1 pt)

## Your answer here
set.seed(457)


#(b) 
# Generate 1000 samples from normal distribution with mean=10, standard deviation=2 as X and 
# the other 1000 samples from normal distribution with mean=5, standard deviation=3 as Y. 
# Then find the mean and standard deviation of X+Y. (1 pt)

## Your answer here
X = rnorm(1000, mean = 10, sd = 2)
Y = rnorm(1000, mean = 5, sd = 3)
mean(X+Y)
sd(X+Y)

#(c) 
# Now use the simulation to estimate the distribution of X+Y and create the confidence intervals.

##(1) 
## Form a set of Xs and Ys by repeating individual experiment for B = 2000 times,
## each experiment has n = 1000 samples. You may want to write a for loop and create two 
## matrices "sample_X" and "sample_Y" to save those values (see class notes). (2 pts)

## Your answer here

a = replicate(2000, rnorm(1000,mean = 10, sd = 2))
b = replicate(2000, rnorm(1000,mean = 5, sd = 3))

sample_X = matrix(a, nrow = 2000, ncol = 1000) 
sample_Y = matrix(b, nrow = 2000, ncol = 1000)

##(2) 
## Calculate mean of X+Y for each experiment and save it 
## to a vector which has a length of B, and plot a histogram of 
## these means. (2 pts)

## Your answer here
d <- c()

for (i in 1:2000) {
  d[i] = mean(rnorm(1000, mean = 10, sd = 2)+rnorm(1000, mean = 5, sd = 3))
}
d
hist(d)

##(3) 
## Now as we have a simulated sampling distribution of X+Y, 
## calculate the 95% confidence interval for mean of X+Y (this 
## can be done empirically). (2 pts)

## Your answer here
upper = (mean(d)+(1.96*(sd(d)/sqrt(2000))))
lower = (mean(d)-(1.96*(sd(d)/sqrt(2000))))
CI    = c(lower,upper)

#(d)
# In the above example, we have fixed the sample size n and number of experiments B. 
# Next, we want to change B and n, and see how confidence interval will change.
# Please write a function to create a confidence interval for any B and n. (3 pts)
f = c()
MC_CI <- function(n, B){
  
  ## your answer here
  for (i in 1:B) {
   
    f[i] = mean(rnorm(n,mean = 10,sd = 2) + rnorm(n,mean = 5,sd = 3))
  }

  upper1 = (mean(f)+(1.96*(sd(f)/sqrt(n))))
  lower1 = (mean(f)-(1.96*(sd(f)/sqrt(n))))
  CI    = c(lower1,upper1)
  return(CI)
 
}

#(e)
# Suppose the sample size n varies (100, 200, 300, .... , 1000) (fix B=2000) and 
# the number of experiments B varies (1000, 2000, ... , 10000) (fix n=500). 
# Plot your confidence intervals to compare the effect of changing the sample size n and 
# changing the number of simulation replications B (2 plots).
# What do you conclude? (4 pts)
# (Hint: Check function errbar() in Hmisc package for plot)

library(Hmisc)

## Your answer here

# fix n, B vary
B1 = seq(1000,10000,by = 1000)
g = list()

for(i in 1:length(B1)){
  
  g[[i]] = MC_CI(500,B1[i])
  
}


lower2 = c()
upper2 = c()
med = c()

for (i in 1:length(B1)) {
  
  lower2[i] = g[[i]][1]
  upper2[i] = g[[i]][2]
  med[i] = c(median(g[[i]]))
}


#Plot

errbar(B1,med,upper2,lower2,
       main = "CI Vs No. of Simulations", 
             xlab = "No. of Simulations", ylab = "Confidence Interval")

# fix B, n vary
n1 = seq(100,1000,by = 100)
h = list()

for(i in 1:length(n1)){
  
  h[[i]] = MC_CI(n1[i],2000)
  
}

lower3 = c()
upper3 = c()
med1 = c()
for (i in 1:length(n1)) {
  
  lower3[i] = h[[i]][1]
  upper3[i] = h[[i]][2]
  med1[i] = c(median(g[[i]]))
  
}


#Plot
errbar(n1,med1,upper3,lower3,
       main = "CI Vs No. of Samples", 
       xlab = "No. of Samples", ylab = "Confidence Interval")

# It is observed that,for the variation in the no. of simulations  
# there is a greater variation in the median but as the number of 
# simulations increases it becomes stable.
# It is observed for the variation in the sample size, 
# the median is concentrated around the centre with less variation  
# among the median values.
# The median value is larger when we vary the simulations 
# than the median values for the variation in sample size.
# The confidence interval goes on decreasing as the samples 
# increase as well as the simulation number increases.

################    Part 2: Regular Expressions    ################################

#(a) 
# Use grep() to find which words in test set match the following requirements.
# (return locations of the matched words in test set)

##(1) 
## Words start with 's' (1 pt)

test_1 = c("introduction", "to", "data", "science", "457", "stats","snack") 

## Your answer here
grep("^s", value = TRUE,test_1)
grep("^s",test_1)

##(2) 
## A string that would be our netID (lower case letters + number) (1 pt)

test_2 = c("mxian111", "ANSA111", "Jeffery", "199", "Jeff333", "linz22",
           "wod123")

## Your answer here
grep("^[a-z]|d$", value = TRUE,test_2)
grep("^[a-z]|d$",test_2)


##(3) 
## An email address in form of example@example.xxx (1 pt)

test_3 = c("aaa@illinois.edu", "nothing@nothing", "bbb@gmail.com", 
           "maybe@now.net","data @ gmail.com", "ee123@illinois.edu",
           "abc_123@gmail.com","abc_123@gmail.com.net") 

## Your answer here
grep("(^m|@\\S+.com$|.edu$)",value = TRUE,test_3)
grep("(^m|@\\S+.com$|.edu$)",test_3)


##(4) 
## An email address that ends with .com or .edu (1 pt)

test_4 = c("aaa@illinois.edu", "nothing@nothing", "bbb@gmail.com", 
           "maybe@now.net","data @ gmail.com", "ee123@illinois.edu",
           "abc_123@gmail.com","abc_123@gmail.com.net") 
## Your answer here
grep(".edu$|@\\S+.com$",value = TRUE,test_4)
grep(".edu$|@\\S+.com$",test_4)


#(b) 
# Carry out the following exercises on the Womens Clothing E-Commerce Reviews dataset
# (available in moodle, Womens Clothing E-Commerce Reviews.txt).

##(1) 
## Use readLines() to read text data (it is in moodle) (encoding = "UTF-8") (1 pt)

## Your answer here
data1 <- readLines("c:/Users/abhis/Desktop/Womens Clothing E-Commerce Reviews.txt") 

##(2) 
## Use regular expressions to return the number of reviews in the dataset (1 pt) 
## (Hint: find the pattern for each review)
## Your answer here

rev_data = grep("Review Text:", value = TRUE,data1)
num_review = length(rev_data)
num_review


##(3) 
## extract information of "Title", "Review text", "Rating" 
## and "Department name", and save them into 4 vectors, named
## "title", "review", "rating" and "department".
## Please print the first 3 elements and the length of each vector.
## (3 pts)

##(hint: split each string by some patterns, for list manipulation 
## you may want to use funtions in apply family. and unname())

## Your answer here
title_split <- strsplit(rev_data,"\\t")
head(title_split)
# Title of Womens Clothing :
title = sapply(title_split,function(x) c(x[2]))
head(title,3)
class(title)

# Review Text of Womens Clothing :
review = sapply(title_split,function(x) c(x[4]))
head(review,3)
class(review)

# Rating of Womens Clothing :
rating = sapply(title_split,function(x) c(x[6]))
head(rating,3)
class(rating)

# Department of Womens Clothing :
department = sapply(title_split,function(x) c(x[8]))
head(department,3)
class(department)


##(4) 
## When checking the text in 'title', we find some additional symbols, for example the \ in:

## "\"Shimmer, surprisingly goes with lots\"" 
## "\"Nice, but not for my body\""  

## use regular expressions to find all the titles which have that pattern of "\"xxxxx\""
## print out the first three incorrect titles and the total number of incorrect titles.
## (2pts)

## Your answer here
pattern1 <- c("^[^A-z0-9+$~~!*?:(){}<>/&%@#;,.+-]")
incor = grep(pattern1, value = TRUE, title)
head(incor,3)
length(incor)


##(5) 
## Change the format of all the incorrect titles, follow this rule:  
##  "\"xxxxxxx\"" ----> "xxxxxxx" (2 pts)

## for example:
## "\"Shimmer, surprisingly goes with lots\"" 
## ----> "Shimmer, surprisingly goes with lots"

## Your answer here
pattern2 = "^[^A-z0-9+$~~!*?:(){}<>/&%@#;,.+-]|[^A-z0-9]+$"
title2 = gsub(pattern2 , "" , incor)
head(title2,3)


##(6) 
## Let's look at the reviews, and do some sentimental analysis.
## We want to compare the reviews between Dresses and Tops department.
## So first select reviews from these two department and save them as
## two vectors review_dress and review_top. (2 pts)
## (Hint: there are 5695 reviews in dresses department and 9656 reviews in tops department)

## Your answer here
# Dress Reviews
review_dress = na.exclude(review[department== "Dresses"])
head(review_dress,3)
length(review_dress)

# Top Reviews
review_top = na.exclude(review[department=="Tops"])
head(review_top,3)
length(review_top)


##(7) 
## Eliminate apostrophes, numbers, and change all characters into lowercase 
## for review_dress and review_top. (3 pts)

## For example:
## "Finally a dress that is not too short! i'm 5'11 and ordered two sizes."
## ----> "finally a dress that is not too short im  and ordered two sizes"

## Your answer here

pattern3 = "[0-9+[:punct:]]"

review_dress2 = tolower(gsub(pattern3 , "" , review_dress))
head(review_dress2,3)

review_top2 = tolower(gsub(pattern3 , "" , review_top))
head(review_top2,3)


##(8) 
## Split the reviews that you have cleaned in Q(7) by blanks and drop any empty words.
## Save all the split words into one list for each department,
## named them as token_top and token_dress. 
## print the length of token_top and token_dress. (3 pts)

## Your answer here

token_dress1 = unlist(strsplit(review_dress2," "))
token_dress = list(token_dress1[token_dress1 != ""])
length(token_dress)
length(token_dress[[1]][])


token_top1 = unlist(strsplit(review_top2," "))
token_top = list(token_top1[token_top1 != ""])
length(token_top)
length(token_top[[1]][])


##(9) 
## Based on results in Q(8), calculate the token frequency for each token 
## in each department.(2 pts)

## Your answer here
dress_freq = table(token_dress)
head(dress_freq,3)

top_freq = table(token_top)
head(top_freq,3)


##(10) 
## Carry out some exploratory analysis of the data and term frequencies. 
## For example, find the words associated with positive and negative reviews. What 
## are distribution patterns for the term frequencies?
## Plot and interpret your result. What are your observations? (4 pts)

## Your answer here

install.packages("syuzhet")
install.packages("ggplot2")
library(syuzhet)
library(ggplot2)

dress_uncommon = dress_freq[dress_freq < quantile(dress_freq,0.70) &
                                   dress_freq > quantile(dress_freq,0.30)]
dress_new = data.frame(a = as.numeric(dress_uncommon),b = names(dress_uncommon))
dress_char = as.character(dress_new$b)
dress_char
senti = data.frame(t(get_nrc_sentiment(dress_char)))
senti_sums = data.frame(rowSums(senti[2:1591]))
names(senti_sums)[1] = "total"
senti_sums = cbind("sentiment" = rownames(senti_sums),senti_sums)
rownames(senti_sums) = NULL
senti_sums2 = senti_sums[1:8,]

qplot(sentiment, data = senti_sums2, weight = total, 
      geom = "bar",fill = sentiment) + 
      ggtitle("Dress Response")


# It is observed from the qplot that, 
# most of the dress department are positive which have odd reviews.
# some people trust the department dresses and anticipate it.
# Some of them are surprised by the dresses provided.
# The negative reviews are less compared to the positive reviews.
# Most of the reviews are of fear and anger for the dress quality.


# tops

top_uncommon = top_freq[top_freq < quantile(top_freq,0.80) &
                               top_freq > quantile(top_freq,0.20)]
top_new = data.frame(a = as.numeric(top_uncommon),b = names(top_uncommon))
top_char = as.character(top_new$b)

senti_top = data.frame(t(get_nrc_sentiment(top_char)))
senti_sums_top = data.frame(rowSums(senti_top[2:2924]))
names(senti_sums_top)[1] = "total"
senti_sums_top = cbind("sentiment" = rownames(senti_sums_top), senti_sums_top)
rownames(senti_sums_top) = NULL
senti_top_sums2 = senti_sums_top[1:8,]

qplot(sentiment, data = senti_top_sums2, weight = total, 
      geom = "bar",fill = sentiment) + 
      ggtitle("Top Response")

# It is observed from the qplot that, 
# The highest frequency is of words is for tops is :
# Expressing trust
# Anticipation 
# Joy 
# The frequency of words for the tops department:
# It is greater for positive meanings than the negative ones
# Less words are used to express anger and disgust.
# The people are satisfied with the tops.