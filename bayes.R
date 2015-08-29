#-------------------------------------------
# This script tries to guess the most likely pin 
# using Bayes models. 
#
# Author: Omar Farooq
# Date: 8-22-15
#-------------------------------------------


#Loading the data
data   = read.csv("output.txt", head=T, sep=',', stringsAsFactors=FALSE);
distance_matrix = read.csv("distance_matrix.csv", head=T, sep=',', stringsAsFactors=FALSE);

#Normalizing

for(i in 1:nrow(data)){
  
  #Converting to sumeric
  row = as.numeric(data[i,c('T1','T2','T3')]);
  
  #Sum the time data and divide each entry
  #Multiply by 10 to scale it
  data[i,c('T1','T2','T3')] = (data[i,c('T1','T2','T3')] / sum(row))*10;
  data[i, "D1"] = distance_matrix[ data[i,"N1"]+1, 
                                   paste("X", data[i,"N2"]  ,sep='')];
  
  data[i, "D2"] = distance_matrix[ data[i,"N2"]+1, 
                                   paste("X", data[i,"N3"]  ,sep='')];
  
  data[i, "D3"] = distance_matrix[ data[i,"N3"]+1, 
                                   paste("X", data[i,"N4"]  ,sep='')];
  
  #use absolute distances- assumes left and right finger movements 
  #take the same time
  data[i,c('D1','D2','D3')] = abs(data[i,c('D1','D2','D3')]);
}


#load the library
library(e1071);


#Seperate indices for test and training data
index = 1:nrow(data);  #The whole dataset index

#Sample randomly from the indices to get test and training sets
testIndex = sample(index, trunc(length(index)* percentTest ));
testSet = data[testIndex ,  ];
trainSet = data[ -testIndex , ];

#Adding scaling
#trainSet = scale(trainSet);


classifier = naiveBayes( as.factor( D3  ) ~ D1+ T1+ D2+T2+T3 , trainSet );


#From the testing set we consider the features 
#our model was based on
testSet_Features = subset(testSet,select=c("D1","T1",'D2','T2','T3'));

#Predict
result = as.character(predict (classifier , testSet_Features ));


table(result,testSet$D3 );

finalResults = data.frame(Actual = testSet$D3,
                          Prediction = result);