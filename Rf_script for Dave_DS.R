library(randomForest)

?randomForest

rfdat2 <- read.csv(file = "rfdat2_biorem.csv")


#with bio variables dropped based on VICor threshold
rf4 <- rfdat2 %>%
  mutate_at(c(4:11), funs(c(scale(.))))%>% 
  dplyr::select(-longitude, -latitude) %>% 
  mutate_if(is.character, as.factor) #*** Added this line to make the rf work...it didn't work for me as character

## attempt an RF model with selected climate variables and site factors

set.seed(1)
 #myRF<-randomForest(pop ~., data = train_data, 
                    #importantance = T, flag=0)
 
 index_row <- sample(2,
                     
                     nrow(rf4),
                     
                     replace = T,
                     
                     prob = c(0.7, 0.3)
                     
 )                 #assign values to the rows (1: Training, 2: Test)
 
 train_data <- rf4[index_row == 1,]
 
 test_data <- rf4[index_row == 2,]
 
 fon_classifier <- randomForest(pop ~.,
                                 
                                 data = train_data, #train data set
                                 
                                 importance = T)
 
 plot(fon_classifier)
 importance(fon_classifier)
 
 varImpPlot(fon_classifier)
 
 #*** Dave messing
 fon_classifier
 # 10.4% error...not bad
 predTrain <- predict(fon_classifier, train_data, type = "class")
 table(predTrain, train_data$pop) # Works nearly perfectly, EXCEPT all WA_Bs are incorrectly classified as WA_A
 predTest <- predict(fon_classifier, test_data, type = "class") # How well does it do the for test set?
 table(predTest, test_data$pop) # Works well, EXCEPT that it can't distinguish between CRY_A & CRY_B, LNP_A & LNP_B and WA_A & WA_B...in each case, Bs were incorrectly classified as A
 
 # Really pretty good, given your fairly limited dataset...?