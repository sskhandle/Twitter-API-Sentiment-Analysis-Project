# Ebola data
#============================================================================
ds = read.table("ebola_dataset_main1.csv",sep=" ")
# ds

#================================== 

ds_x = as.matrix(ds[,-(length(ds))])
# ds_x
#================================== 

# require ("ggplot2")
# ggplot (ds, aes (x = V1, y = V2, colour = as.factor(V8))) + stat_density2d (h=0.1)

attach(ds);plot(V1,V12, pch=20, col=c("red","blue")[unclass(as.numeric(V18)+1)]);detach(ds)

#================================== 

x0 = as.matrix(seq(1,1,length = length(ds_x[,1])))
ds_x = as.matrix(cbind(x0, ds_x))
#================================== 


y_generator = function(y_label){
  ds_y <<- data.matrix(integer(0))
  for(i in 1:length(ds$V1)){
    if(ds[i,length(ds)] == y_label){
      ds_y <<- rbind(ds_y, 1)
    }
    else{
      ds_y <<- rbind(ds_y, 0)
    }
  }
}
y_generator(y_label = 1)

#================================== 

# Formulating the Logistic Regression Algorithm
#---------------------------------------------------

theta_list = (as.matrix(rep(0.1, length(ds))))
theta_list

y_hat_list = data.matrix(numeric(0))
y_hat = 0

# Initial y prediction 
for(i in 1:length(ds_y)){
  y_hat = 1/1+(exp(-(ds_x[i,]%*%((theta_list)))))
  y_hat_list = rbind(y_hat_list, y_hat)  
}            

#Gradient descent and optimization

temp = data.matrix(numeric(0))
temp_list = matrix(numeric(0),nrow=length(ds))
y_temp = 0
y_temp_list = data.matrix(numeric(0))
n = 0.001

for(x in 1:1000){ # Upto 50000 iterations, takes approx 1 hr to run
  temp = t(theta_list) + (1/length(ds_y))*(n * (t(y_hat_list - ds_y) %*% ds_x)) 
  theta_list = t(as.matrix(temp))
  
  y_temp = 0
  y_temp_list = data.matrix(numeric(0))
  for(i in 1:length(ds_y)){
    y_temp =  ((1/length(ds_y))*(1/1+(exp(-(ds_x[i,]%*%((theta_list)))))))
    y_temp_list = rbind(y_temp_list, y_temp)        
  }
  y_hat_list = y_temp_list * 100
}

#Decision boundary
y_pred = data.matrix(numeric(0))
for(i in 1:length(y_hat_list)){
  if(y_hat_list[i]>=0.5){
    y_pred = rbind(y_pred, 1)
  }
  else{
    y_pred = rbind(y_pred, 0)
  }
}

mse = function(){
  sample <- ds  
  op_mean_list = list()
  length_list = list()
  for(i in 1:9){   
    size1 = i/10
    t.idx<-sample(1:2000, size = size1 * 2000)
    sample.tr<-sample[t.idx,]
    sample.te<-sample[-t.idx,]   
    logit1<-glm(V18~., sample.tr, family=binomial())
    logit1
    summary(logit1)    
    prob<<-predict(logit1, sample.te, type="response")
    length = length(sample.tr$V18)
    length_list = c(length_list, length)   
    pred_class = (matrix(c(ifelse(prob>0.5, 1, 0))))    
    table(Class=sample.te$V18, pred=pred_class)
    op_mean = mean(sample.te$V18 != pred_class)
    op_mean_list = c(op_mean_list, op_mean)
  }
  x = as.numeric(op_mean_list)
  y = as.numeric(length_list)
  smoothingSpline = smooth.spline(y, x, spar=0.35, tol = 0.0001)
  plot(y,x, pch=".", col="blue", main = "Logistic Regression Error \n Multiple Sentiment Scores", xlab = "Training data length", ylab = "Mean Error")
  lines(smoothingSpline, col = 'red', lwd = 2)
  
}

# ================================================================================================================================
#Function for performance evaluation
perf_eval = function(y_hat, y_real){
  # Confusion matrix 
  conf_matrix = table(y_hat, y_real)
  print(conf_matrix)
  tp = conf_matrix[1,1]
  fp = conf_matrix[1,2]
  fn = conf_matrix[2,1]
  tn = conf_matrix[2,2]
  
  # accuracy
  acc <<- ((tp-fp)-(fn-tn))/length(y_real)
  
  # precision
  prec_1 <<- tp/(tp+fp)
  
  prec_2 <<- tn/(tn+fn)
  
  # recall
  rec_1 <<- tp/(tp+fn)
  
  rec_2 <<- tn/(tn+fp)
  
  # f-measure
  f_1 <<- 2*((prec_1*rec_1)/(prec_1+rec_1))
  
  f_2 <<- 2*((prec_2*rec_2)/(prec_2+rec_2))
}

# ================================================================================================================================



perf_eval(y_hat=y_pred, y_real=ds_y)
mse()
st = system.time(replicate(5, mse()))
st

eval_score = function(){
  cat("\nAccuracy: ",acc)
  cat("\nPrecision 1: ",prec_1)
  cat("\nPrecision 2: ",prec_2)
  cat("\nRecall 1: ",rec_1)
  cat("\nRecall 2: ",rec_2)
  cat("\nF1 score: ",f_1)
  cat("\nF2 score: ",f_2)
}
eval_score()

rm(list=ls())
