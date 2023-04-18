---
title: "FastRTutorial"
output: html_document
date: "2023-04-06"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

# Lesson 1: Getting Started

-   Install R
-   Start up R (or RStudio terminal)

# Lesson 2: First R Steps

```{r}
1+1
```

### Example: Nile River Data

```{r}
mean(Nile)
```

```{r}
Nile
```

```{r}
length(Nile)
```

```{r}
hist(Nile)
```

```{r}
hist(Nile, breaks=20)
```

Call helper to find out more about a function

```{r}
?hist
```

# Lesson 3: Vectors and Indices

### Accessing elements

Use subscripts or indices with brackets Example: access 2nd element of Nile vector

```{r}
Nile[2]
```

**Build a Vector:**

-   **Concatenation:** c() builds from individual elements

-   **Colon:** builds by selecting a range

Example: access 2nd, 3rd, and 4th elements of Nile vector

```{r}
Nile[c(2,3,4)]
Nile[2:5]
```

```{r}
mean(Nile[81:100])
```

Make a copy of a slice of Nile data:

```{r}
n81100 <- Nile[81:100]
mean(n81100)
sd(n81100)
```

```{r}
n81100[1]
Nile[81]
```

```{r}
length(Nile)
length(n81100)
```

# Lesson 4: More on Vectors

**Sum:** sum elements of a vector

```{r}
sum(c(5,12,13))
```

Interpretation: there are 7 years in which the Nile river level exceeded 1200

```{r}
sum(Nile > 1200)
```

```{r}
x <- c(5,12,13)
x
x > 8
sum(x > 8)
```

**Which:** determine which elements of vector carry a property/meet a condition

-   Identify indices where Nile \> 1200 is TRUE

```{r}
which(Nile > 1200)
```

-   Identify and store indices where Nile \> 1200 is TRUE

-   Then find length of these indices

```{r}
which1200 <- which(Nile > 1200)
which1200
length(which1200)
```

-   Identify and store indices where Nile \> 1200 is TRUE

-   Then find values at those indices

```{r}
which1200 <- which(Nile > 1200)
Nile[which1200]
```

-   Find values of Nile which are \> 1200

-   Eliminates which1200 middle man

```{r}
Nile[Nile > 1200]
```

**Negative Indices**

```{r}
x <- c(5,12,13,8)
x
x[-1]
x[c(-1,-4)]
```

# Lesson 5: On to Data Frames

### Load Data Frame

```{r}
head(ToothGrowth)
```

### **Extract from Data Frame**

df[rows we want, columns we want]

```{r}
tg <- ToothGrowth
mean(tg$len)
tg[3,1]
tg$len[3]
mean(tg[,1])
```

Extract subsets:

```{r}
z <- tg[2:5, c(1,3)]
z
```

```{r}
y <- tg[ , c(1,3)]
y
```

```{r}
nrow(ToothGrowth)
length(tg$len)
length(tg$supp)
length(tg$dose)
```

Apply **head** to vector

```{r}
head(ToothGrowth$len)
head(ToothGrowth$len, 10)
```

### Creating Data Frames

```{r}
x <- c(5,12,13)
y <- c('abc','de','z')
d <- data.frame(x,y)
d
```

```{r}
y <- c('abc','de','z')
y[2]
```

```{r}
z <- tg[ , -2]
head(z)
```

# Lesson 6: R Factor Class

```{r}
class(tg)
class(tg$supp)
```

R factors are used when we have categorical variables. Find the list of categories in tg\$supp as follows:

```{r}
levels(tg$supp)
```

# Lesson 7: Extracting Rows/Columns from Data Frames

Extract vector elements:

```{r}
which1200 <- which(Nile > 1200)
Nile[which1200]
```

Extract data frame rows or columns:

```{r}
whichOJ <- which(tg$supp == 'OJ')
whichVC <- which(tg$supp == 'VC')
mean(tg[whichOJ,1])
mean(tg[whichVC,1])
```

```{r}
tgoj <- tg[tg$supp == 'OJ',]
tgvc <- tg[tg$supp == 'VC',]
mean(tgoj$len)
mean(tgvc$len)
```

# Lesson 8: More Examples of Extracting Rows, Columns

Multiple Conditions:

```{r}
tg[tg$supp == 'OJ' & tg$len < 8.8, ]
```

```{r}
tg[tg$len > 28 | tg$dose == 1.0,]
```

Saving Results

```{r}
w <- tg[tg$len > 28 | tg$dose == 1.0,]
head(w)
```

```{r}
lendose <- tg[,c(1,3)]
head(lendose)
```

Specify columns by name instead of number:

```{r}
lendose <- tg[,c('len','dose')]
head(lendose)
```

Logical operation on vectors:

```{r}
exts <- Nile[Nile < 800 | Nile > 1300]
head(exts)
```

```{r}
length(Nile[Nile < 800 | Nile > 1300])
```

# Lesson 9: The tapply Function

In English: "Split the vector **tg\$len** into two groups, according to the value of **tg\$supp**, then apply **mean** to each group."

```{r}
tapply(tg$len,tg$supp,mean)
```

Note that the result was returned as a vector, which we could save by assigning it to, say **z**:

```{r}
z <- tapply(tg$len,tg$supp,mean)
z[1] 
z[2]
```

```{r}
x <- c(8,5,12,13)
g <- c('M',"F",'M','M')
tapply(x,g,mean)
```

```{r}
tapply(PlantGrowth$weight,PlantGrowth$group,length)
```

Mtcars Dataset:

```{r}
row.names(mtcars)
```

```{r}
row.names(mtcars)[7]
row.names(mtcars)[7] <- 'Dustpan'
row.names(mtcars)[7]
```

# Lesson 10: Data Cleaning

```{r}
pima <- read.csv('http://heather.cs.ucdavis.edu/FasteR/data/Pima.csv',header=TRUE)
head(pima)
```

The **dim** function tells us that there are 768 people in the study, 9 variables measured on each.

```{r}

dim(pima)
```

**Table Function:**

```{r}
table(pima$glucose)
```

Remove instances where glucose level is 0

```{r}
pg <- pima$glucose
pg1 <- pg[pg > 0]
length(pg1)
```

```{r}
mean(pg)
mean(pg1)
```

Encode 0 values as NA instead

```{r}
pima$glucose[pima$glucose == 0] <- NA
sum(is.na(pima$glucose))
```

Mean function does not skip over NA values by default

```{r}
mean(pima$glucose)
mean(pima$glucose, na.rm=TRUE)
```

# Lesson 11: The R List Class

```{r}
mtmpg <- mtcars$mpg
```

Split the vector into three vectors, one for 4-cylinder cars, one for 6-cylinder cars, and one for 8-cylinder cars

Option A:

```{r}
mt4 <- mtmpg[mtcars$cyl == 4]
mt6 <- mtmpg[mtcars$cyl == 6]
mt8 <- mtmpg[mtcars$cyl == 8]
```

Option B:

```{r}
mtl <- split(mtmpg,mtcars$cyl)
mtl
```

Index mtl according to automatically-made split:

```{r}
mtl$'4'
mtl[[1]]
```

```{r}
head(mtcars$cyl)
```

Make copies:

```{r}
m4 <- mtl[[1]]
m6 <- mtl[[2]]
m8 <- mtl[[3]]
```

**Lists** are good for mixing types together in one package:

```{r}
l <- list(a = c(2,5), b = 'sky')
l
```

Change default names:

```{r}
names(mtl) <- c('four','six','eight')
mtl
```

Get MPG for third car in 6-cylinder category

```{r}
mtl[[2]][3]
mtl$six[3]
```

# Lesson 12: Another Look at the Nile Data

Plot data against time:

```{r}
plot(Nile)
```

```{r}
which(Nile < 600)
```

Plot mtcars: Weight vs. MPG

```{r}
plot(mtcars$wt,mtcars$mpg)
```

Nile is of numeric type and **time series** class

```{r}
is.numeric(Nile)
class(Nile)
```

```{r}
Nile[55]
Nile[1 + 1925 - 1871]
```

# Lesson 13: Pause to Reflect

One has various tools and materials, and the goal is to put these together in a creative way to produce the end result.

The tools here are the various functions, e.g. **mean** and **which**, and the materials are one's data. One then must creatively put them together to achieve one's goal.

# Lesson 14: Introduction to Base R Graphics

```{r}
load(url('https://github.com/matloff/fasteR/blob/master/data/prgeng.RData?raw=true'))
head(prgeng)
```
