# a, read data
college = read.csv("College.csv")

# b, data sanitizing and inspection, renaming rows
fix(college)
rownames(college) = college[,1]
fix(college)
college = college[,-1]
fix(college)
attach(college)

# c, initial data inspection
summary(college)
?pairs
pairs(college[,1:10])
?plot
plot(Private, Outstate)
?rep
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
?as.factor # from string to "factor"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college)
plot(Elite, Outstate)
# Oohh, I had the > sign the other way round and realized 
#by looking at the plots!
?hist
par(mfrow=c(2,2))
summary(college)
hist(Outstate)
hist(Grad.Rate)
hist(PhD)
hist(Terminal)

