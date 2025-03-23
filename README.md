# datafest24-submission
Do the Types of Formative Assignments Affect a Student’s End of Chapter Score?

#Data Fest 2024
#Team: Matt Amatics
#Penelope Overholt and Chante Graham

#load in libraries
library(readr)
library(ggplot2)

setwd("C:/Users/prove/Documents/R_DataSets/")

#read in data with end of chapter score
eoc <- read_csv("checkpoints_eoc.csv")
#keep only what we need
eoc <- data.frame(eoc$student_id, eoc$n_possible, eoc$n_correct)
names(eoc) <- c("student_id","n_possible","n_correct")
eoc <- aggregate(eoc[,c("n_possible","n_correct")],
 by = list(eoc$student_id), FUN= sum)
names(eoc) <- c("student_id","n_possible","n_correct")
scoreEOC <- eoc$n_correct/eoc$n_possible
eoc <- data.frame(eoc, scoreEOC)
eoc$n_correct = NULL
eoc$n_possible = NULL
#eoc is ready to go, it contains a student with the average of their eoc scores

#read in response, this has lrn_type
response <- read_csv("responses.csv")
#keep what we need
response <- data.frame(response$student_id, response$points_earned,
 response$points_possible, response$lrn_type)
names(response) = c("student_id","points_earned",
 "points_possible","lrn_type")

 #remove questions without points
response <- response[!is.na(response$points_possible),]
#replace null scores with zero
response$points_earned[is.na(response$points_earned)] <- 0
#make NA lrn_type "other"
response$lrn_type[is.na(response$lrn_type)] <- "other"

#aggregate by lrntype and student id
response <- aggregate(response[,c("points_earned","points_possible")],
 by = list(response$lrn_type,response$student_id), FUN= sum)

 score <- response$points_earned/response$points_possible
response <- data.frame(response, score)
response$points_earned = NULL
response$points_possible = NULL
names(response) <- c("lrn_type","student_id","score")
#response now has lrn_type by student and score

#need to pivot response
#should have columns like: student id, lrn_type1, lrn_type2...
response2 <- response #need duplicate that is NOT pivoted for later
response <- reshape(data = response,
 idvar = "student_id",
 v.names = c("score"),
 timevar = "lrn_type",
 direction = "wide")

 #merge eoc ans response by student
#should have columns like: student id, eocScore, lrn_type1, lrn_type2...
allscores <- merge(eoc, response, by = "student_id")

#look at correlation coefficients
allscorecor <- cor(allscores[,2:12], use = "complete.obs")

#run models to predict EOC score by lrn_type
modelAssoc <- lm(scoreEOC ~ score.association , data = allscores,na.action=na.omit)
modelChoice <- lm(scoreEOC ~ score.choicematrix , data = allscores,na.action=na.omit)
modelClozeassoc <- lm(scoreEOC ~ score.clozeassociation , data = allscores,na.action=na.omit)
modelformula <- lm(scoreEOC ~ score.formulaV2 , data = allscores,na.action=na.omit)
modelimageclozeassoc <- lm(scoreEOC ~ score.imageclozeassociation , data =
allscores,na.action=na.omit)
modelmcq <- lm(scoreEOC ~ score.mcq , data = allscores,na.action=na.omit)
modelother <- lm(scoreEOC ~ score.other , data = allscores,na.action=na.omit)
modelplaintext <- lm(scoreEOC ~ score.plaintext ,data = allscores, na.action=na.omit)
modelshorttext <- lm(scoreEOC ~ score.shorttext ,data = allscores, na.action=na.omit)
modelsortlist <- lm(scoreEOC ~ score.sortlist , data = allscores,na.action=na.omit)

#view models
summary(modelAssoc)
summary(modelChoice)
summary(modelClozeassoc)
summary(modelimageclozeassoc)
summary(modelmcq)
summary(modelother)
summary(modelplaintext)
summary(modelshorttext)
summary(modelsortlist)

#look at all scatter plots of predicting EOC score by lrn_type
#just to get a feel for things
allplots <- plot(scoreEOC ~ score.association + score.choicematrix +
 score.clozeassociation+score.formulaV2+score.imageclozeassociation+
 score.mcq+score.other+score.plaintext+score.shorttext+score.sortlist,
 data = allscores)

 #make plots to compare eoc score by lrn_type###

 #look at the linear ones
plotassoc <- ggplot(allscores, aes(x = score.association, y = scoreEOC)) +
 geom_point(color = "maroon1") +
 labs(x="lrn_type: Association Score", y= "End of Chapter Score",
 title = "EOC Score by Association lrn_type")
#view
plotassoc

plotmcq <- ggplot(allscores, aes(x = score.mcq, y = scoreEOC)) +
 geom_point(color = "turquoise") +
 labs(x="lrn_type: Multiple Choice Score", y= "End of Chapter Score",
 title = "EOC Score by Multiple Choice lrn_type")
#view
plotmcq

plotother <- ggplot(allscores, aes(x = score.other, y = scoreEOC)) +
 geom_point(color = "goldenrod1") +
 labs(x="lrn_type: Other Score", y= "End of Chapter Score",
 title = "EOC Score by Other lrn_type")
#view
plotother

#scatter plot of non linear ones
response2 <- merge(response2, eoc, by = "student_id")
response2 <- subset(response2, lrn_type != "association" &
 lrn_type !="mcq" & lrn_type !="other")
plotweird <- ggplot(response2, aes(x = score,
 y = scoreEOC)) +
 geom_point(aes(color = lrn_type)) +
 labs(x="Score by lrn_type", y= "End of Chapter Score",
 title = "EOC Score by lrn_type")
#view
plotweird
