library(psych)
library(lme4)
library(stats)
library(readr)
artdata <- read_csv("https://github.com/jon-may/multi-aesthetics/raw/master/artdatacsv.csv", 
     col_types = cols(Piece = col_factor(levels = c("Tree", "Plate", "Smooth", "Rough")), Condition = col_factor(levels = c("Combo","Sound","Object"))))


# create function to do boxplots, anova and posthocs for any variable
myAnalysis <- function(x, g){
  varname <-deparse(substitute(x)) # obtain variable name for titling plot
  boxplot(x ~ g, notch=TRUE, main=paste(varname))
  model<-aov(x ~ g) # oneway anova of variable by Condition
  print(summary(model)) # anova table output for main effect of Condition
  print(describeBy(x, group=g))
  print(TukeyHSD(model)) # posthocs for levels of Condition
  }


tree <- subset(artdata, artdata$Piece=="Tree")
# first two are OBJECT descriptors so should be LOWER for sound
myAnalysis(tree$Expanding, tree$Condition)
myAnalysis(tree$Textured, tree$Condition)
# next two are SOUND descriptors so should be LOWER for object
myAnalysis(tree$Metallic, tree$Condition)
myAnalysis(tree$Warping  , tree$Condition)
# next two are COMBO descriptors so should be HIGHER for combo
myAnalysis(tree$Moving, tree$Condition)
myAnalysis(tree$Chronological  , tree$Condition)


plate <- subset(artdata, artdata$Piece=="Plate")

# first two are OBJECT descriptors so should be LOWER for sound
myAnalysis(plate$Travelling, plate$Condition)
myAnalysis(plate$Mechanical, plate$Condition)
# next two are SOUND descriptors so should be LOWER for object
myAnalysis(plate$Detailed, plate$Condition)
myAnalysis(plate$Near  , plate$Condition)
# next two are COMBO descriptors so should be HIGHER for combo
myAnalysis(plate$Scratching, plate$Condition)
myAnalysis(plate$Light  , plate$Condition)


smooth <- subset(artdata, artdata$Piece=="Smooth")

# first two are OBJECT descriptors so should be LOWER for sound
myAnalysis(smooth$Static, smooth$Condition)
myAnalysis(smooth$Moist, smooth$Condition)
# next two are SOUND descriptors so should be LOWER for object
myAnalysis(smooth$Hollow, smooth$Condition)
myAnalysis(smooth$Unfinished  , smooth$Condition)
# next two are COMBO descriptors so should be HIGHER for combo
myAnalysis(smooth$Alive, smooth$Condition)
myAnalysis(smooth$Threatening  , smooth$Condition)


rough <- subset(artdata, artdata$Piece=="Rough")

# first two are OBJECT descriptors so should be LOWER for sound
myAnalysis(rough$Violent, rough$Condition)
myAnalysis(rough$Engulfing, rough$Condition)
# next two are SOUND descriptors so should be LOWER for object
myAnalysis(rough$Spiky, rough$Condition)
myAnalysis(rough$Chaotic  , rough$Condition)
# next two are COMBO descriptors so should be HIGHER for combo
myAnalysis(rough$Aggressive, rough$Condition)
myAnalysis(rough$Dynamic, rough$Condition)



