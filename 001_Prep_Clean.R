setwd("C:/Users/Alan5/OneDrive/Desktop/Data Science/010_PP_IMDb_Change_Opinion")
library(ggplot2)
library(tidyverse)
library(tidyr)


### IMDb files -----------------------------
bas<- read.csv("Title_Basic_CSV.csv")
rat<- read.csv ("Title_Rating_CSV.csv")

### Merging files -----------------------------

dat<- merge(bas, rat, by="tconst")

##cleaning some stuff --------------
dat$runtimeMinutes <- as.numeric(dat$runtimeMinutes) ##making run times numeric
dat<- dat[dat$genre1 !="\\N",] ##dropping useless genres
dat<- dat[dat$genre1 !="/N",] ##dropping useless genres
dat<- dat[dat$runtimeMinutes != "/N",] #dropping useless runtimes
dat<- na.omit(dat)
dat$startYear <- as.numeric(dat$startYear)

### Simple LM average rating as a correlate to year of release####

###Assumption testing####
cor.test(dat$averageRating, dat$startYear) ##Not 0 but very weak, ~0.1288, significant

ggplot(dat, aes(x=startYear, y=averageRating)) + geom_point() ##Looking for outliers

##Model
simyr<- lm(formula = averageRating~startYear, data = dat)
summary(simyr)
sigma(simyr)
simyr

plot(dat$startYear, dat$averageRating)

### MLM average rating as a correlate to year of release and genres####

mlmyg<- lm(formula= averageRating~startYear+g1+g2+g3, data = dat)
summary(mlmyg)
modelsummary(mlmyg)

avPlots(mlmyg)

###Visualization####
###Visuals of aggregate data####
ggplot(data = dat) + 
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color = genre1))

ggplot(data = dat, aes(startYear, averageRating, color = genre1)) +
  geom_point() +
  geom_smooth(se=FALSE, method = lm)

###Seperate by genres####
act<- dat[dat$genre1 == 'Action',]
adu<- dat[dat$genre1 == 'Adult',]
adv<- dat[dat$genre1 == 'Adventure',]
ani<- dat[dat$genre1 == 'Animation',]
bio<- dat[dat$genre1 == 'Biography',]
com<- dat[dat$genre1 == 'Comedy',]
cri<- dat[dat$genre1 == 'Crime',]
doc<- dat[dat$genre1 == 'Documentary',]
dra<- dat[dat$genre1 == 'Drama',]
fam<- dat[dat$genre1 == 'Family',]
fan<- dat[dat$genre1 == 'Fantasy',]
noi<- dat[dat$genre1 == 'Film-Noir',]
gam<- dat[dat$genre1 == 'Game-Show',]
his<- dat[dat$genre1 == 'History',]
hor<- dat[dat$genre1 == 'Horror',]
mus1<- dat[dat$genre1 == 'Music',]
mus2<- dat[dat$genre1 == 'Musical',]
mys<- dat[dat$genre1 == 'Mystery',]
news<- dat[dat$genre1 == 'News',]
rea<- dat[dat$genre1 == 'Reality-TV',]
rom<- dat[dat$genre1 == 'Romance',]
sci<- dat[dat$genre1 == 'Sci-Fi',]
shrt<- dat[dat$genre1 == 'Short',]
sprt<- dat[dat$genre1 == 'Sport',]
talk<- dat[dat$genre1 == 'Talk-Show',]
thr<- dat[dat$genre1 == 'Thriller',]
war<- dat[dat$genre1 == 'War',]
wes<- dat[dat$genre1 == 'Western',]


###Individual graphs###
ggplot(data = act, aes(startYear, averageRating, color = runtimeMinutes)) +
  geom_point() +
  geom_smooth(se = FALSE, method = lm) ##Action films

ggplot(data = adu) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Adult films
ggplot(data = adv) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Adventure films
ggplot(data = ani) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Animation films
ggplot(data = bio) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Biography films
ggplot(data = com) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Comedy films
ggplot(data = cri) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Crime films
ggplot(data = doc) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Documentary films
ggplot(data = dra) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Drama films
ggplot(data = fam) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Family films
ggplot(data = fan) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Fantasy films
ggplot(data = gam) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Game-Show films
ggplot(data = his) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##History films
ggplot(data = hor) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Horror films
ggplot(data = mus1) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Music films
ggplot(data = mus2) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Musical films
ggplot(data = mys) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Mystery films
ggplot(data = news) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##News films
ggplot(data = noi) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Noir films
ggplot(data = rea) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Reality films
ggplot(data = rom) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Romance films
ggplot(data = sci) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Sci-Fi films
ggplot(data = shrt) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Short films
ggplot(data = sprt) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Sport films
ggplot(data = talk) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Talk show films
ggplot(data = war) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##War films
ggplot(data = wes) +
  geom_point(mapping = aes(x=startYear, y = averageRating,
                           color=runtimeMinutes)) ##Western films
