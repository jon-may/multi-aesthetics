library(psych)
library(lme4)
library(stats)
library(readr)
artdata <- read_csv("https://github.com/jon-may/multi-aesthetics/raw/master/artdatacsv.csv", 
     col_types = cols(Piece = col_factor(levels = c("Tree", "Plate", "Smooth", "Rough")), Condition = col_factor(levels = c("Combo","Sound","Object"))))

# create three subframes, one for each condition
Cond<-split(artdata, artdata$Condition)
# get descriptives for each adjective separately for Object and Combo
ComboDesc<-describeBy(Cond$Combo[,6:19], group = Cond$Combo$Piece, mat=FALSE)
SoundDesc<-describeBy(Cond$Sound[,6:19], group = Cond$Sound$Piece, mat=FALSE)
ObjectDesc<-describeBy(Cond$Object[,6:19], group = Cond$Object$Piece, mat=FALSE)

# create function to do boxplots, anova and posthocs for any variable
myAnalysis <- function(x){
  varname <-deparse(substitute(x)) # obtain variable name for titling plot
  boxplot(x ~ Condition, notch=TRUE, main=paste(varname))
  model<-aov(x ~ Condition) # oneway anova of variable by Condition
  summary(model) # anova table output for main effect of Condition
  TukeyHSD(model) # posthocs for levels of Condition
  }


tree <- subset(artdata, artdata$Piece=="Tree")
attach(tree)
# first two are OBJECT descriptors so should be LOWER for sound
myAnalysis(Expanding)
myAnalysis(Textured)
# next two are SOUND descriptors so should be LOWER for object
myAnalysis(Metallic)
myAnalysis(Warping  )
# next two are COMBO descriptors so should be HIGHER for combo
myAnalysis(Moving)
myAnalysis(Chronological  )
detach(tree)

plate <- subset(artdata, artdata$Piece=="Plate")
attach(plate)
# first two are OBJECT descriptors so should be LOWER for sound
myAnalysis(Travelling)
myAnalysis(Mechanical)
# next two are SOUND descriptors so should be LOWER for object
myAnalysis(Detailed)
myAnalysis(Near  )
# next two are COMBO descriptors so should be HIGHER for combo
myAnalysis(Scratching)
myAnalysis(Light  )
detach(plate)

smooth <- subset(artdata, artdata$Piece=="Smooth")
attach(smooth)
# first two are OBJECT descriptors so should be LOWER for sound
myAnalysis(Static)
myAnalysis(Moist)
# next two are SOUND descriptors so should be LOWER for object
myAnalysis(Hollow)
myAnalysis(Unfinished  )
# next two are COMBO descriptors so should be HIGHER for combo
myAnalysis(Alive)
myAnalysis(Threatening  )
detach(smooth)

rough <- subset(artdata, artdata$Piece=="Rough")
attach(rough)
# first two are OBJECT descriptors so should be LOWER for sound
myAnalysis(Violent)
myAnalysis(Engulfing)
# next two are SOUND descriptors so should be LOWER for object
myAnalysis(Spiky)
myAnalysis(Chaotic  )
# next two are COMBO descriptors so should be HIGHER for combo
myAnalysis(Aggressive)
myAnalysis(Dynamic)
detach(rough)


