#-------------------------------------------
# This script tries to guess the most likely pin 
# using ANN. 
#
# Author: Omar Farooq
# Date: 8-22-15
#-------------------------------------------


# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}



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

#Install the package
#install.packages("neuralnet")

#Starting the ANN
library(neuralnet);

percentTest= 0.4;
HLNodes = 5;


#Seperate indices for test and training data
index = 1:nrow(data);  #The whole dataset index

#Sample randomly from the indices to get test and training sets
testIndex = sample(index, trunc(length(index)* percentTest ));
testSet = data[testIndex ,  ];
trainSet = data[ -testIndex , ];

#Adding scaling
#trainSet = scale(trainSet);

#building the magnificent neural network creature
#The first parameter is the model <what you are tyring to predict> ~ <All your features seperated by + signs>
NNet =  neuralnet(D3 ~ D1+ T1+D2+T2+T3, trainSet, 
                  hidden = HLNodes ,
                  lifesign="minimal",threshold = 0.01,
                  learningrate=0.1);


#Plot the network(the best part)
plot(NNet);

#From the testing set we consider the features 
#our model was based on
testSet_Features = subset(testSet,select=c("D1","T1",'D2','T2','T3'));


#Using the testData compute output based on Neural network prediction
NNresults = compute(NNet , testSet_Features );

NNresults$net.result

#Definitely round the results
if(F){
  NNresults$net.result = round(NNresults$net.result);
}


#Compile the output in a presentable form
finalOutput = data.frame(Actual = testSet$D3,
                         Prediction = NNresults$net.result);


# Calculate error
error <- finalOutput$Actual - finalOutput$Prediction;

rmse(error)
mae(error)
