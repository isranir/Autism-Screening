library(readr) 

getwd()

setwd("C:/Old laptop/Data/CA/Data Science/autism-screening-for-toddlers")

Autism_screening <- read_csv("ToddlerAutismdatasetJuly2018CleanedUp.csv")

View(Autism_screening)

install.packages("ggplot2")

library("ggplot2")

#Filter for Autism traits = Yes
Autism_Traits_Yes <- dplyr::filter(Autism_screening, Autism_screening$`Class/ASD Traits` == "Yes")
View(Autism_Traits_Yes)

**Median age of Autism detection by Ethnicity**
ggplot(data=Autism_Traits_Yes, aes(Autism_Traits_Yes$Ethnicity))+
  stat_summary_bin(aes(y=Autism_Traits_Yes$Age_Mons), fun.y = "median", geom = "bar")+
  theme( axis.line = element_line(colour = "darkblue", 
                                  size = 1, linetype = "solid"), legend.key = element_rect(fill = "white", colour = "black"))

![alt text](https://user-images.githubusercontent.com/45016625/54564301-6da71080-4999-11e9-8b68-dfd3527fccc6.png "Median age of Autism detection by Ethnicity")

**From the chart above, we can see that the earliest age Autism is detected in Hispanics and the latest age Autism is detected is in Native Indian**


**Median age of Autism detection when a famliy member has autism vs when a family member does not have Autism**
ggplot(data=Autism_Traits_Yes, aes(Autism_Traits_Yes$Family_mem_with_ASD), fill="green")+ 
  stat_summary_bin(aes(y=Autism_Traits_Yes$Age_Mons), fun.y = "median", geom = "bar")+
  theme( axis.line = element_line(colour = "darkblue", 
                               size = 1, linetype = "solid"), legend.key = element_rect(fill = "white", colour = "black"))
                               
![alt text](https://user-images.githubusercontent.com/45016625/54631869-d9958180-4a4a-11e9-8202-6ab7b5c9e4ee.png "Median age of Autism detection when a family member has autism vs when a family member does not have Autism")

#find correlation between the questions and the Autism traits. Note - added a new column mapping for autism trait = 1/0 for Yes/No to make the column numeric 
cor(Autism_screening[,c(2:13,20)], use="pairwise", method="spearman")


  
#Split train and test   
install.packages("tree")
  library(tree)
  set.seed(101)
  train=sample(1:nrow(Autism_screening), nrow(Autism_screening)/2)
  test= -train
  training_data=Autism_screening[train,]
  testing_data=Autism_screening[test,]
  ASDTraits= Autism_screening$ClassASDTraits[test]
  
  #Update the training_data features as factors for using classification tree 
  names(Autism_screening)
  names<-c(2:12,14:17,19)
  training_data[,names]<-lapply(training_data[,names], factor)
  str(training_data)
  
  tree_model = tree(ClassASDTraits~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+Age_Mons+Sex+Ethnicity+Jaundice+Family_mem_with_ASD, data=training_data)
 
  plot(tree_model)
  text(tree_model, pretty=0)
![alt text](https://user-images.githubusercontent.com/45016625/54632089-51fc4280-4a4b-11e9-920e-a6fddedf1193.png "Plot tree model")

  #Update the training_data features as factors for using classification tree 
    testing_data[,names]<-lapply(testing_data[,names], factor)
  str(testing_data)
 
  #check how the model is doing using the test data
  tree_pred=predict(tree_model, testing_data, type="class")
  mean(tree_pred!=ASDTraits) #9.7%
  
  #Mean of the difference between actual and predicted is 9.7%. No need to prune the tree. 

