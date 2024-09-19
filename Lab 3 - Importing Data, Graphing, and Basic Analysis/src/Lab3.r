############################################################
##  Lab 3
##  Introduction to Statistical Analysis in R
##  Data Prep and Basic Analysis
##  Time in Life data originally prepared by Dr. David Keellings
############################################################



###################################
##	Load and prepare required data:
###################################

lifedata <- read.table(file.choose(), sep="\t", header=FALSE, stringsAsFactors = T)
lifedata
head(lifedata, 10)

## Set column names
colnames(lifedata) <- c("part", "time", "sex", "age", 
"diff", "bodytemp", "heartrate", 
"marriage", "job", "family", "edu", 
"support", "health")

##  Check the dataset for errors
summary(lifedata)
boxplot(lifedata$age)
boxplot(lifedata$bodytemp)
lifedata[rowSums(is.na(lifedata)) > 0,]


##  Fix the dataset errors
na.omit(lifedata)
lifedata <- lifedata[rowSums(is.na(lifedata)) < 2,]
summary(lifedata)
lifedata[rowSums(is.na(lifedata)) > 0,]
lifedata$bodytemp[is.na(lifedata$bodytemp)] <- median(lifedata$bodytemp, na.rm = TRUE)
outs <- boxplot.stats(lifedata$bodytemp)$out
lifedata$bodytemp[lifedata$bodytemp %in% outs] <- median(lifedata$bodytemp, na.rm = TRUE)
summary(lifedata)
lifedata$age[lifedata$age == 1430] <- median(lifedata$age, na.rm = TRUE)
summary(lifedata)
lifedata <- droplevels(lifedata, exclude = "")
summary(lifedata)


###########################################################
##	Explore continuous variables and run sample statistics:
###########################################################

## Set up storage of all following figures in a pdf doc and tabular info in a text file
## Change these file directories to location of your output folder for lab 3!!!
pdf(file="/Volumes/KINGSTON UF/KINGSTON COPY/R Class/IntroR /Lab 3 - Importing Data, Graphing, and Basic Analysis/output/convars.pdf")
sink("/Volumes/KINGSTON UF/KINGSTON COPY/R Class/IntroR /Lab 3 - Importing Data, Graphing, and Basic Analysis/output/convars.txt")

## Set the graphics device to plot two figures in a 1 row by 2 column configuration
par(mfrow=c(1,2))
## Create histogram of dependent variable (the difference measure)
hist(lifedata$diff, xlab="Difference Measure", main=NA, col="skyblue")
## Add a one dimensional representation of the data along the x axis
rug(lifedata$diff)
## Draw a bounding box around the histogram plot
box()
## Create a box plot of the dependent variable
boxplot(lifedata$diff, ylab="Difference Measure", main=NA, 
		col="skyblue")
## Restore the graphics device to single plot configuration
par(mfrow=c(1,1))
## Add a title to the plot
title(main="Difference Measure")

## Create a data frame with dependent variable summary statistics
diff<-data.frame(n=length(lifedata$diff), min=min(lifedata$diff), 
		max=max(lifedata$diff), mean=mean(lifedata$diff), 
		median=median(lifedata$diff), var=var(lifedata$diff), 
		sd=sd(lifedata$diff))

## Print the data frame, rounded to three digits 
print("Diff")
print(diff, digits=3)

#########################################################################
##  Let's test your coding skills a little!
##  Create a histogram, boxplot, and statistical summary for the age independent variable
##  HINT:  This should be very similar to the code above for the diff variable
#########################################################################















## Stop the sink and close the graphics device
sink()
dev.off()
## Remove age and diff objects from workspace
rm(age)
rm(diff)

###########################################################
##	Explore categorical variables and run sample statistics:
###########################################################


## Set up storage of all following figures in a pdf doc and tabular info in a text file
## Change these file directories to location of your output folder for lab 3!!!
pdf(file="/Volumes/KINGSTON UF/KINGSTON COPY/R Class/IntroR /Lab 3 - Importing Data, Graphing, and Basic Analysis/output/catvars.pdf")
sink("/Volumes/KINGSTON UF/KINGSTON COPY/R Class/IntroR /Lab 3 - Importing Data, Graphing, and Basic Analysis/output/catvars.txt")


## cat is simpler alternative to print
cat("Sex of Participants")
## table performs counts at each factor level of the variable
table(lifedata$sex)

cat("Marital Status")
table(lifedata$marriage)
cat("Employment Status")
table(lifedata$job)
cat("Surviving Family Members")
table(lifedata$family)
cat("Education Level")
table(lifedata$edu)

## Set up graphics device to display a 2 row by 3 column configuration of plots
par(mfrow=c(2,3))

## Create bar plots of proportion of subjects in each factor of the categorical variable
barplot(table(lifedata$sex)/length(lifedata$sex), col="skyblue", ylim=c(0,1), 
xlab="sex", ylab="Proportion", main="Sex of Participants", names=c("Female", "Male"))
## Add bounding box
box()

barplot(table(lifedata$marriage)/length(lifedata$marriage), col="skyblue", ylim=c(0,1), 
xlab="Married", main="Marital Status", names=c("No", "Yes"))
box()
barplot(table(lifedata$job)/length(lifedata$job), col="skyblue", ylim=c(0,1), 
xlab="Employed", main="Employment Status", names=c("No", "Yes"))
box()
barplot(table(lifedata$family)/length(lifedata$family), col="skyblue", ylim=c(0,1), 
xlab="Surviving Family", ylab="Proportion", main="Surviving Family Members", names=c("No", "Yes"))
box()
barplot(table(lifedata$edu)/length(lifedata$edu), col="skyblue", ylim=c(0,1), 
xlab="Education", main="Education Level", names=c("H", "U", "G"))
box()
## Restore the graphics device to single plot configuration
par(mfrow=c(1,1))

## Stop the sink and close the graphics device
sink()
dev.off()


####################################################################################
##	Above we explored the variables descriptively, but let's now get inferential
## 	and look for statistical differences within the explanatory (IV) variables
##  Descriptive Stats, Boxplots, T-test, ANOVA, User function(), if/else(), cleaning 
##  output(mtext() & labels)
####################################################################################


###  Categorical Variables

##  Attach the lifedata (so we can use variable names without having to reference the column)
attach(lifedata)

## Apply Factors to Variables (so that we can use more detailed descriptors as labels)
sex = factor(sex, labels=c("Female", "Male"))
marriage = factor(marriage, labels=c("Not Married", "Married"))
job = factor(job, labels=c("Unemployed", "Employed"))
family = factor(family, labels=c("No Family", "Have Family"))
edu = factor(edu, labels=c("HS", "UGrad", "Grad"))

## Create data frames for the dependent variable and categorical IVs
lifedata.dv<-diff
lifedata.cat <- data.frame(sex, marriage, job, family, edu)

##Create vector of Variable Names
vars.cat<-colnames(lifedata.cat)

##  Function for analysis of categorical IVs against the continuous DV
##  Functions can come as part of packages you install or you can create your own
##  Here the function is to be named concatfun and it takes 3 arguments
##  Here the function is defined, then it can be called and passed the 3 arguments
concatfun<-function(eachvar, name, response){
			## Print summary stats of the DV by each IV and factor, use ?tapply to read about what it does 
			print(tapply(response, eachvar, summary))
			## Create box plot of DV by each IV and factor
			boxplot(response~eachvar, main=name, xlab="", ylab="Difference Measure")
			## if statement adds a conditional branch to the program flow
			## here if the variable has two levels (factors i.e. male/female)
			## the first branch is executed, if more than two levels the second 
			## branch (after the else if) is executed
			if(length(levels(eachvar))== 2) {
				## if 2 levels we can perform a two sample t test
				ttest<-t.test(response~eachvar)
				print(ttest)
				## Here we have a nested if else to deal with the t test p value
				## if the p value is <0.001 (very significant) we store that as a
				## text string to variable pval, else we round the value and store to pval
				if(ttest$p.value<0.001){
					pval<-"<0.001"
				} else{
					pval<-round(ttest$p.value, digits=4)
				}
				## Here we are thinking about the output figure and display of the pval
				## paste together "p-value" with the t test p value
				pvallab<-paste("p-value:", pval)
				## Add that text to the box plot
				mtext(pvallab)
				} 
			else if(length(levels(eachvar))> 2) {
				## if more than two levels perform an ANOVA
				fit<-aov(response ~ eachvar, data=lifedata)
				fitsum<-summary(fit)
				print("ANOVA")
				print(fitsum)
				## same p value treatment as above
				if(fitsum[[1]][, 5][1]<0.001){
					pval<-"<0.001"
				} else{
					pval<-round(fitsum[[1]][, 5][1], digits=4)
				}
				pvallab<-paste("p-value:", pval)
				mtext(pvallab)
				}	
}

## Set up storage of all following figures in a pdf doc and tabular info in a text file
## Change these file directories to location of your output folder for lab 3!!!
pdf(file="/Volumes/KINGSTON/R Class/Lab 3 - Importing Data, Graphing, and Basic Analysis/output/catvarstats.pdf")
sink("/Volumes/KINGSTON/R Class/Lab 3 - Importing Data, Graphing, and Basic Analysis/output/catvarstats.txt")

##  Loop for sending categorical variables to categorical analysis function defined above
##  loop iterates through the length of the cat variable data frame, pulling out one 
##  variable per iteration

for (i in 1:length(lifedata.cat)) {
	eachvar <- lifedata.cat[,i]
	name <- vars.cat[i]
	concatfun(eachvar, name, lifedata.dv)
}

## Stop the sink and close the graphics device
sink()
dev.off()

##########################################################################
## Continuous Variables 	 
## Descriptive Stats, scatter plots, correlation, regression, diagnostics, 
## User function(), if/else(), cleaning output(mtext() & labels)
##########################################################################

## Create data frame for the continuous IVs
lifedata.con <- data.frame(age, bodytemp, heartrate, support, health)

## Create vector of Variable Names
vars.con<-colnames(lifedata.con)

## Plot all continuous DVs vs. IV
pdf(file="/Volumes/KINGSTON UF/KINGSTON COPY/R Class/IntroR /Lab 3 - Importing Data, Graphing, and Basic Analysis/output/convarscatter.pdf")
## Set up graphics device to display a 2 row by 3 column configuration of plots
par(mfrow=c(2, 3))

## Loop through con variables, plotting each against DV
for (i in 1:length(lifedata.con)) {
	eachvar <- lifedata.con[,i]
	name <- vars.con[i]
	plot(eachvar, lifedata.dv, xlab=name, ylab="Difference Measure")
}

## Restore the graphics device to single plot configuration
par(mfrow=c(1,1)) 

dev.off()


## Function for analysis of continuous IVs against continuous DV
conconfun<-function(eachvar, name, response){
		## Create nice text header for stats output
		header<-paste("##########", name, "##########")
		## print the nice header
		print(header)
		## print the results of a Pearson's Correlation
		print(cor.test(response, eachvar))
		## Plot IV vs DV as scatter plot
		plot(eachvar, response, xlab=name, ylab="Difference Measure", type="p", pch=19, col="black")
		## Fit a linear model
		fit<-lm(response~eachvar)
		## Add red line of model fit to plot
		abline(fit, col="red", lwd=2)
		## Create summary of model fit
		fitsum<-summary(fit)
		## print model summary
		print(fitsum)
		## if else similar to previous cat function to parse out p values
		if(fitsum$coef[2,4]<0.001){
			pval<-"<0.001"
		} else{
			pval<-round(fitsum$coef[2,4], digits=4)
		}
		## create text string for adjusted r squared and p value
		labtext<-paste("Adj. R^2:", round(fitsum$adj.r.squared, digits=4), "p-value:", pval)
		## add text string to plot
		mtext(labtext, side=3) 
		## Set graphics device using layout command to a 2x2 grid that will be populated by 4 figures
		layout(matrix(c(1,2,3,4),2,2))
		## Plot the model diagnostics
		plot(fit)
		## Reset graphics device
		par(mfrow=c(1,1))
}

## Set up storage of all following figures in a pdf doc and tabular info in a text file
## Change these file directories to location of your output folder for lab 3!!!
pdf(file="/Volumes/KINGSTON/R Class/Lab 3 - Importing Data, Graphing, and Basic Analysis/output/convarstats.pdf")
sink("/Volumes/KINGSTON/R Class/Lab 3 - Importing Data, Graphing, and Basic Analysis/output/convarstats.txt")

#########################################################################
##  Let's test your looping skills a little!
##  Create a for loop that iterates through the continuous variables data frame
##  sending one variable at a time to the conconfun()
##  HINT:  We did this previously for categorical variables
#########################################################################

##Loop for sending continuous variables to continuous analysis function	





## Stop the sink and close the graphics device
sink()
dev.off()



