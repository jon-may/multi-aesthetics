library(psych)
library(lme4)
library(stats)
library(readr)
artdata <- read_csv("https://github.com/jon-may/multi-aesthetics/raw/master/artdatacsv.csv", 
     col_types = cols(Sculpture = col_factor(levels = c("Tree", "Plate", "Smooth", "Rough")), Sound = col_factor(levels = c("Yes","No"))))

# ctreate two subframes, one for Object only and another for Combo
x<-split(artdata, artdata$Sound)
# get descriptives for each adjective separately for Object and Combo
ComboDesc<-describeBy(x$Yes[,6:19], group = x$Yes$Sculpture, mat=FALSE)
ObjectDesc<-describeBy(x$No[,6:19], group = x$No$Sculpture, mat=FALSE)


tree <- subset(artdata, artdata$Sculpture=="Tree")
attach(tree)
# first two are OBJECT descriptors so should not be affected by Sound
boxplot(Expanding ~ Sound)
t.test(Expanding ~ Sound)
boxplot(Textured ~ Sound)
t.test(Textured ~ Sound)
# next two are SOUND descriptors so should be higher for Sound=Yes
boxplot(Metallic ~ Sound)
t.test(Metallic ~ Sound)
boxplot(Warping ~ Sound)
t.test(Warping ~ Sound)
# next two are COMBO descriptors so should be higher for Sound=Yes
boxplot(Moving ~ Sound)
t.test(Moving ~ Sound)
boxplot(Chronological ~ Sound)
t.test(Chronological ~ Sound)
detach(tree)

plate <- subset(artdata, artdata$Sculpture=="Plate")
attach(plate)
# first two are OBJECT descriptors so should not be affected by Sound
boxplot(Travelling ~ Sound)
t.test(Travelling ~ Sound)
boxplot(Mechanical ~ Sound)
t.test(Mechanical ~ Sound)
# next two are SOUND descriptors so should be higher for Sound=Yes
boxplot(Detailed ~ Sound)
t.test(Detailed ~ Sound)
boxplot(Near ~ Sound)
t.test(Near ~ Sound)
# next two are COMBO descriptors so should be higher for Sound=Yes
boxplot(Scratching ~ Sound)
t.test(Scratching ~ Sound)
boxplot(Light ~ Sound)
t.test(Light ~ Sound)
detach(plate)

smooth <- subset(artdata, artdata$Sculpture=="Smooth")
attach(smooth)
# first two are OBJECT descriptors so should not be affected by Sound
boxplot(Static ~ Sound)
t.test(Static ~ Sound)
boxplot(Moist ~ Sound)
t.test(Moist ~ Sound)
# next two are SOUND descriptors so should be higher for Sound=Yes
boxplot(Hollow ~ Sound)
t.test(Hollow ~ Sound)
boxplot(Unfinished ~ Sound)
t.test(Unfinished ~ Sound)
# next two are COMBO descriptors so should be higher for Sound=Yes
boxplot(Alive ~ Sound)
t.test(Alive ~ Sound)
boxplot(Threatening ~ Sound)
t.test(Threatening ~ Sound)
detach(smooth)

rough <- subset(artdata, artdata$Sculpture=="Rough")
attach(rough)
# first two are OBJECT descriptors so should not be affected by Sound
boxplot(Violent ~ Sound)
t.test(Violent ~ Sound)
boxplot(Engulfing ~ Sound)
t.test(Engulfing ~ Sound)
# next two are SOUND descriptors so should be higher for Sound=Yes
boxplot(Spiky ~ Sound)
t.test(Spiky ~ Sound)
boxplot(Chaotic ~ Sound)
t.test(Chaotic ~ Sound)
# next two are COMBO descriptors so should be higher for Sound=Yes
boxplot(Aggressive ~ Sound)
t.test(Aggressive ~ Sound)
boxplot(Dynamic ~ Sound)
t.test(Dynamic ~ Sound)
detach(rough)
