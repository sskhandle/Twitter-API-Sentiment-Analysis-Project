sample1 <- read.csv("ebola_data1.csv", header = FALSE, sep = "")
len <- length(sample1)
row <- nrow(sample1)
tp <- 0
fp <- 0
fn <- 0
tn <- 0
ycap <- 0
acc <- 0
prec_1 <- 0
prec_2 <- 0
rec_1 <- 0
rec_2 <- 0
f_1 <- 0
f_2 <- 0
alphac1 <- 0
alphac2 <- 0
intc1 <- 0
intc2 <- 0
g1 <- 1
g2 <- 1
plot(sample1$V1,sample1$V8, pch=21, bg=c("red","green3","blue")[unclass(ycap)], main=" Ebola only")
for(i in 1:7) {  alphac1[i] <- (sum(sample1[1:1000,i])/1000) }
for(i in 1:7) {  alphac2[i] <- (sum(sample1[1001:2000,i])/1000) } 
diff_func <- 0

for(j in 1:2000)
{
  for(i in 1:7) 
  { 
    intc1[i] <- ((alphac1[i])^sample1[j,i] * (1-alphac1[i])^(1-sample1[j,i])   ) 
    g1 <- g1 + intc1[i]
    
    
    intc2[i] <- ((alphac2[i])^sample1[j,i] * (1-alphac2[i])^(1-sample1[j,i]))
    g2 <- g2 + intc2[i]        
    
  }
  g1 <- g1 
  g2 <- g2
  diff_func[j] <- (g1-g2)
  if(diff_func[j]>0) { ycap[j] <- 1 } else { ycap[j] <- 0 }
  g1 <- 0
  g2 <- 0

}
ycap


# Confusion matrix
conf_matrix = table(sample1$V8, ycap)
print(conf_matrix)
tp = conf_matrix[1,1]
fp = conf_matrix[1,2]
fn = conf_matrix[2,1]
tn = conf_matrix[2,2]
# accuracy
acc = ((tp+tn)/(fn+fp))
print(acc)

# precision
prec_1 = tp/(tp+fp)
print(prec_1)

prec_2 = tn/(tn+fn)
print(prec_2)

# recall
rec_1 = tp/(tp+fn)
print(rec_1)

rec_2 = tn/(tn+fp)
print(rec_2)

# f-measure
f_1 = 2*((prec_1*rec_1)/(prec_1+rec_1))
print(f_1)

f_2 = 2*((prec_2*rec_2)/(prec_2+rec_2))
print(f_2)



ycap <- ycap + 1
plot(sample1$V1,ycap, pch=21, bg=c("red","green3","blue")[unclass(ycap)], main="Prediction of Ebola only")
plot(diff_func, sample1$V1, pch=21, bg=c("red","green3","blue")[unclass(ycap)], main="Prediction of Ebola only")
