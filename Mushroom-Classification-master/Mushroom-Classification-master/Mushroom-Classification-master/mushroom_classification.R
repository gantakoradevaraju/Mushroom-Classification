
#            MUSHROOM CLASSIFICATION THROUGH R


#         DATA LOADING

data =read.csv(file.choose())
names(data)
head(data)
sapply(data,class)

# ------ DATA ANALYSIS

nrow(data)
length(data)
str(data)
summary(data)

# visualisation  misssing values in the data through graphically and practically 

library(visdat)

sapply(data,function(x)sum(is.na(x)))
vis_dat(data)#displays the missing data contained column and
vis_miss(data)


# Data Division into train and testing parts

library(caTools)
set.seed(123)
s= sample.split(data,SplitRatio = 0.7)
train=subset(data,s==TRUE)
test=subset(data,s==FALSE)



#    modeling using decission tree

library(rpart)

m=rpart(class~.,data = train,method = "class")

#--------- prediction-----------

p=predict(m,test,type = "class")

# DATA VISUALISATION

library(rattle)
fancyRpartPlot(m)


head(p,10)
head(test$class,10)


#CROSS VALIDATION AND ACCURACY

library(caret)
confusionMatrix(p,test$class)

plot(test$class,p ,col=rainbow(2) ,xlab="test$class",ylab="predicted")

# DATA WRITING INTO ANOTHER FILE 

d =test[,-1]
names(d)
result <- data.frame(d,target=p)
names(result)

write.csv(result,file="output.csv")


