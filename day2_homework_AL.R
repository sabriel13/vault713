# Complete all of the items below
# Use comments where you're having trouble or questions

# 1. Read your data set into R
ax <- read.csv("ax_AL.csv", sep = ",", header=TRUE)


# 2. Peek at the top few rows
head(ax)
# or
ax[1:5, ]


# 3. Peek at the top few rows for only a few columns
ax[1:3, 1:3]
ax[1:3, c("Item" ,"SubjectID", "ACC")]

# 4. How many rows does your data have?
length(ax$Item)
length(ax$ACC)
# both return 21264, so my data has 21264 rows.

# 5. Get a summary for every column
summary(ax)

# 6. Get a summary for one column
summary(ax$phoneme_pair)


# 7. Are any of the columns giving you unexpected values?
#    - missing values? (NA)

# There is nothing too unexpected.  The minimum RT is small given that 2 short sound files were played with 100ms in between them, so there are probably some outliers.  Some columns appear to R to be numeric when they are not (e.g., Item, SubjectID) because I've only used numbers in those columns. Word1 and word2 do not have that issue because some words include a "b", which requires the vector to be a factor. There are no missing values.


# 8. Select a few key columns, make a vector of the column names
ax.colnames <- colnames(ax)[2:6] 
ax.colnames
# or if I wanted column names from columns that were not adjacent
ax.colnames2 <- colnames(ax)[c(2:5, 7)]
ax.colnames2

# 9. Create a new data.frame with just that subset of columns
#    from #7
#    - do this in at least TWO different ways
#first method
ax.small <- ax[,c(2:6)]
summary(ax.small)
#second method
ax.small2 <- ax[,c("Item", "SubjectID", "RT", "ACC", "spkr")]
summary(ax.small2)
#third method
ax.small3 <- data.frame (Item=ax[,2], SubjectID=ax[,3], RT = ax[,4], ACC = ax[, 5], spkr = ax[,6])
summary(ax.small3)

# 10. Create a new data.frame that is just the first 10 rows
#     and the last 10 rows of the data from #8
ax.small.20 <- ax.small[c(1:10, 21255:21264), ]
length(ax.small$Item)
length(ax.small.20$Item)
summary(ax.small.20)

# 11. Create a new data.frame that is a random sample of half of the rows. I found some code at stackoverflow to select random rows (http://stackoverflow.com/questions/8273313/random-rows-in-dataframe-in-r)
half <- 21264/2
ax.random <- ax[sample(nrow(ax), half), ]
#double checking
summary(ax.random)
length(ax.random$RT)
head(ax.random)

# 12. Find a comparison in your data that is interesting to make
#     (comparing two sets of numbers)
#     - run a t.test for that comparison
#     - decide whether you need a non-default test
#       (e.g., Student's, paired)
#     - run the t.test with BOTH the formula and "vector"
#       formats, if possible
#     - if one is NOT possible, say why you can't do it

# comparison: Is the overall accuracy different for native speakers and second lanuage learners? ACC is the accuracy for a trial and spkr is the grouping variable (native speaker=NS, second language learner=L2). The two groups are not paired in any ways, so an indepedent t-test can be used. I found a way to subset based on variable values used in the vector method below from Quick-R (http://www.statmethods.net/management/subset.html)

#formula t.test 
t.12.formula <- t.test(ACC~spkr, data=ax)
t.12.formula
# vector t.test
L2acc <- ax[which(ax$spkr=="L2"), 5]
NSacc <- ax[which(ax$spkr=="NS"), 5]
t.12.vector <- t.test(L2acc, NSacc)
t.12.vector

# 13. Repeat #10 for TWO more comparisons
#     - ALTERNATIVELY, if correlations are more interesting,
#       do those instead of t-tests (and try both Spearman and
#       Pearson correlations)

#First compairson: Is there a difference in accuracy between the pairs of syllables with pharyngeal fricatives (/ʕ-ħ/) and the pairs of syllalbes with uvular fricatives (/ʁ-χ/).  The pu variable can be used as the grouping variable (pharyngeal pair = p, uvular pair = u), and ACC can be used as the dependent variable.  The vector t.test method can be used for this compairson, but the formula version cannot.  As the data is now, there are three levels to the pu variable (control=0), which makes it unsuitable to a t-test, which compares the means of 2 groups, not 3.

# vector t.test
pacc <- ax[which(ax$pu=="p"), 5]
uacc <- ax[which(ax$pu=="u"), 5]
t.13.vector <- t.test (pacc, uacc)
t.13.vector

# formula t.test: will not work because the pu variable has 3 levels
t.test(ACC~pu, data=ax)

# Second compairson: Are accuracy and reaction time correalted? Was there a trade-off such that slower reaction times had greater accuracy? In addtion to both Pearson and Spearman, I will attempt a biserial correlation because accuracy only has 2 response types (I"m not if this is normally done, but I wanted to try it).

#pearson
cor.13.pearson <- cor(ax$RT, ax$ACC)
cor.13.pearson

#spearman
cor.13.spearman <- cor(ax$RT, ax$ACC, method="spearman")
cor.13.spearman

#biserial
library(psych)
cor.13.bs <- biserial(ax$RT, ax$ACC)
cor.13.bs



# 14. Save all results from #12 and #13 in an .RData file
save (t.12.formula, t.12.vector, t.13.vector, cor.13.bs, cor.13.pearson, cor.13.spearman, file="hw2_AL.RData")


# 15. Email me your version of this script, PLUS the .RData
#     file from #14
