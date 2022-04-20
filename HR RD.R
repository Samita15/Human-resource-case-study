setwd("C:/Users/Samita Gawas/OneDrive/Desktop/Data science course/R studio/Project/Human Resource")

hr_train=read.csv("hr_train.csv",stringsAsFactors = FALSE)
hr_test=read.csv("hr_test.csv",stringsAsFactors = FALSE)

hr_test$left=NA

hr_train$data='train'
hr_test$data='test'

library(dplyr)

hr_all=rbind(hr_train,hr_test)

glimpse(hr_all)

hr_all=hr_all %>%
  mutate(left=as.numeric(gsub("%","",left))
         
  )     

glimpse(hr_all)

hr_all=hr_all %>%
  na.omit()

for(i in 1:12){
  if(class(hr_all[,i])=="character")
  {
    hr_all[,i]=as.factor(hr_all[,i])
  }
  
}

glimpse(hr_all)

set.seed(2)
s=sample(1:nrow(hr_train),0.8*nrow(hr_train))
hr_train1=hr_train[s,]
hr_train2=hr_train[-s,]

library(randomForest)
fit_hr=randomForest(left~.,data=hr_train1)
fit_hr
importance(fit_hr)

varImpPlot(fit_hr)


predict_left=predict(fit_hr,newdata=hr_train2,type="class")
predict_left

library(pROC)
auc(roc(hr_train2$left,predict_left))

test.prob.score1= predict(fit_hr,newdata = hr_test,type='response')
write.csv(test.prob.score1,"Samita_Gawas_P4_part2.csv",row.names = F)






