library(corrplot)
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(reshape2)
library(GGally)
library(MASS)
library(plotly)
library(scatterplot3d)
library(sparsediscrim)
library(cluster)
library(raster)
library(factoextra)
library(purrr)

#load data
image.train=read.csv(file='D:/STATS503/project/training.csv')
image.test=read.csv(file='D:/STATS503/project/testing.csv')

#data exploration---------------------------------------------------------------------------------
summary(image.train)
##correlation plot
corrplot(cor(image.train[,seq(2,128,by=21)]))
corrplot(cor(image.train[,seq(3,129,by=21)]))
corrplot(cor(image.train[,seq(4,130,by=21)]))
corrplot(cor(image.train[,seq(5,131,by=21)]))
corrplot(cor(image.train[,seq(6,132,by=21)]))
corrplot(cor(image.train[,seq(7,133,by=21)]))
corrplot(cor(image.train[,seq(8,134,by=21)]))
corrplot(cor(image.train[,seq(9,135,by=21)]))
corrplot(cor(image.train[,seq(10,136,by=21)]))
corrplot(cor(image.train[,seq(11,137,by=21)]))
corrplot(cor(image.train[,seq(12,138,by=21)]))
corrplot(cor(image.train[,seq(13,139,by=21)]))
corrplot(cor(image.train[,seq(14,140,by=21)]))
corrplot(cor(image.train[,seq(15,141,by=21)]))
corrplot(cor(image.train[,seq(16,142,by=21)]))
corrplot(cor(image.train[,seq(17,143,by=21)]))
corrplot(cor(image.train[,seq(18,144,by=21)]))
corrplot(cor(image.train[,seq(19,145,by=21)]))
corrplot(cor(image.train[,seq(20,146,by=21)]))
corrplot(cor(image.train[,seq(21,147,by=21)]))
corrplot(cor(image.train[,seq(22,148,by=21)]))
##relationship between variables and classes
class_var=function(var_column){
  ggplot(data=all_data, aes(y=var_column,x=class,fill=class))+
    geom_boxplot()+
    theme(axis.text=element_text(size=5),legend.position='none')
}
a=class_var(all_data[,2])+ylab('Bright Index')
b=class_var(all_data[,3])+ylab('Area in m2')
c=class_var(all_data[,4])+ylab('Roundness')
d=class_var(all_data[,5])+ylab('Brightness')
e=class_var(all_data[,6])+ylab('Compactness')
f=class_var(all_data[,7])+ylab('Shape Index')
g=class_var(all_data[,8])+ylab('Green')
h=class_var(all_data[,9])+ylab('Red')
i=class_var(all_data[,10])+ylab('Near Infrared')
j=class_var(all_data[,11])+ylab('Standard Deviation of Green')
k=class_var(all_data[,12])+ylab('Standard Deviation of Red')
l=class_var(all_data[,13])+ylab('Standard Deviation of Near Infrared')
m=class_var(all_data[,14])+ylab('Length/Width')
n=class_var(all_data[,15])+ylab('Gray-Level Co-occurrence Matrix 1')
o=class_var(all_data[,16])+ylab('Rectangularity')
p=class_var(all_data[,17])+ylab('Gray-Level Co-occurrence Matrix 2')
q=class_var(all_data[,18])+ylab('Density')
r=class_var(all_data[,19])+ylab('Asymmetry')
s=class_var(all_data[,20])+ylab('Normalized Difference Vegetation Index')
t=class_var(all_data[,21])+ylab('Border Length')
u=class_var(all_data[,22])+ylab('Gray-Level Co-occurrence Matrix 3')
grid.arrange(j,k,l,n,p,u, ncol=2)
grid.arrange(d,g,h,i,s, ncol=2)
grid.arrange(b,ncol=1)
grid.arrange(a,c,e,f,m,o,q,r,t, ncol=2)
##in terms of different scales
var_scale=function(i){
  sub_data=all_data%>%select(c(1,seq(i,i+126,by=21)))
  sub_data=melt(sub_data,id.vars='class')
  ggplot(sub_data,aes(x=value,color=class,fill=class))+geom_density(alpha=0.2)+facet_wrap(~variable)
}
var_scale(2)
var_scale(3)
var_scale(4)
var_scale(5)
var_scale(6)
var_scale(7)
var_scale(8)
var_scale(9)
var_scale(10)
var_scale(11)
var_scale(12)
var_scale(13)
var_scale(14)
var_scale(15)
var_scale(16)
var_scale(17)
var_scale(18)
var_scale(19)
var_scale(20)
var_scale(21)
var_scale(22)
#

#pre-process data-----------------------------------------------------------------
##apply pca
all_data=rbind(image.train,image.test)
standard=scale(all_data[,-1],scale=T,center=T)
pca_results=princomp(standard,cor=F)
all_var=round(matrix(cumsum(pca_results$sdev^2 / sum(pca_results$sdev^2))[1:14],nrow=1),3)
colnames(all_var)=c('Comp.1','Cmp.2','Comp.3','Comp.4','Comp.5','Comp.6','Comp.7','Comp.8','Comp.9','Comp.10','Comp.11','Comp.12','Comp.13','Comp.14')
rownames(all_var)='Variation'
plot(1:length(pca_results$sdev[1:14]), pca_results$sdev[1:14], type = "l",xlab='Component',ylab='Variation explained')

pca_data=cbind(princomp(standard,cor=F)$scores[,1:7],all_data[,1])
colnames(pca_data)[8]='class'
##separate into training and test data
set.seed(1234)
test_obs=sample(1:nrow(pca_data),0.25*nrow(pca_data))
pca_train=as.data.frame(pca_data[-test_obs,])
category_train=as.factor(pca_train[,8])
pca_train=pca_train%>%dplyr::mutate(category_train)%>%dplyr::select(-class)
pca_test=as.data.frame(pca_data[test_obs,])
category_test=as.factor(pca_test[,8])
pca_test=pca_test%>%dplyr::mutate(category_test)%>%dplyr::select(-class)
summary(pca_train)
summary(pca_test)
##data based on different scales
scaleid = seq(1, 7)
generate_id = function(id){
  start = (id - 1) * 21 + 2
  stop = (id - 1) * 21 + 22
  return(seq(start, stop))
}
scaleids = sapply(scaleid, function(s) generate_id(s))
scaleids_df = as.data.frame(scaleids)
colnames(scaleids_df) = c("s20", "s40", "s60", "s80", "s100", "s120", "s140")
scaleids_list = as.list(scaleids_df)
scale_data=function(scaleidd){
  scale_train=cbind(scale(all_data[-test_obs,scaleidd]),category_train)
  scale_test=cbind(scale(all_data[test_obs,scaleidd]),category_test)
  all=list(scale_train,scale_test)
  return(all)
}
scale20=scale_data(scaleids_list$s20)
scale40=scale_data(scaleids_list$s40)
scale60=scale_data(scaleids_list$s60)
scale80=scale_data(scaleids_list$s80)
scale100=scale_data(scaleids_list$s100)
scale120=scale_data(scaleids_list$s120)
scale140=scale_data(scaleids_list$s140)


#LDA\QDA------------------------------------------------------------------------------------
error=function(data){
  pred_train=predict(data, pca_train)
  error_train=1-sum(diag(table(pca_train$category_train,pred_train$class)))/sum(table(pca_train$category_train,pred_train$class))
  pred_test=predict(data, pca_test)
  error_test=1-sum(diag(table(pca_test$category_test,pred_test$class)))/sum(table(pca_test$category_test,pred_test$class))
  errors=as.data.frame(cbind(error_train,error_test))
  colnames(errors)=c('training error','test error')
  return(errors)
}
error_s=function(data,scaless){
  scaless[[1]]=data.frame(scaless[[1]])
  scaless[[2]]=data.frame(scaless[[2]])
  pred_train=predict(data, scaless[[1]])
  error_train=1-sum(diag(table(scaless[[1]]$category_train,pred_train$class)))/sum(table(scaless[[1]]$category_train,pred_train$class))
  pred_test=predict(data, scaless[[2]])
  error_test=1-sum(diag(table(scaless[[2]]$category_test,pred_test$class)))/sum(table(scaless[[2]]$category_test,pred_test$class))
  errors=as.data.frame(cbind(error_train,error_test))
  colnames(errors)=c('training error','test error')
  return(errors)
}
##qda
###training and test error
image_qda=qda(data=pca_train,category_train~.)
qda_error=error(image_qda)
qda_20=qda(data=data.frame(scale20[[1]]),category_train~.)
error_20=error_s(qda_20,scale20)
qda_40=qda(data=data.frame(scale40[[1]]),category_train~.)
error_40=error_s(qda_40,scale40)
qda_60=qda(data=data.frame(scale60[[1]]),category_train~.)
error_60=error_s(qda_60,scale60)
qda_80=qda(data=data.frame(scale80[[1]]),category_train~.)
error_80=error_s(qda_80,scale80)
qda_100=qda(data=data.frame(scale100[[1]]),category_train~.)
error_100=error_s(qda_100,scale100)
qda_120=qda(data=data.frame(scale120[[1]]),category_train~.)
error_120=error_s(qda_120,scale120)
qda_140=qda(data=data.frame(scale140[[1]]),category_train~.)
error_140=error_s(qda_140,scale140)
###cv error
folds=cv_partition(pca_data[,8],num_folds=10)
train_test_qda=function(fold,data) {
  pca_data=as.data.frame(data)
  cv_qda=qda(data=pca_data[fold$training,],factor(class)~.)
  pred=predict(cv_qda, pca_data[fold$test,])
  error=sum(pred$class!=pca_data[fold$test,][,8]) / length(fold$test)
  return(error)
}
cv_qda=function(fold,data) {
  pca_data=as.data.frame(data)
  cv_qda=qda(data=pca_data[fold$training,],factor(class)~.)
  pred=predict(cv_qda, pca_data[fold$test,])
  error=sum(pred$class!=pca_data[fold$test,][,1]) / length(fold$test)
  return(error)
}
qda_cv_error=mean(unlist(lapply(folds,train_test_qda,data=pca_data)))
all_data_s=cbind(all_data[1],scale(all_data[,-1]))
scale20_cv=mean(unlist(lapply(folds,cv_qda,data=all_data_s[,c(1,scaleids_df$s20)])))
scale40_cv=mean(unlist(lapply(folds,cv_qda,data=all_data_s[,c(1,scaleids_df$s40)])))
scale60_cv=mean(unlist(lapply(folds,cv_qda,data=all_data_s[,c(1,scaleids_df$s60)])))
scale80_cv=mean(unlist(lapply(folds,cv_qda,data=all_data_s[,c(1,scaleids_df$s80)])))
scale100_cv=mean(unlist(lapply(folds,cv_qda,data=all_data_s[,c(1,scaleids_df$s100)])))
scale120_cv=mean(unlist(lapply(folds,cv_qda,data=all_data_s[,c(1,scaleids_df$s120)])))
scale140_cv=mean(unlist(lapply(folds,cv_qda,data=all_data_s[,c(1,scaleids_df$s140)])))

#K-means---------------------------------------------------------------------------------------------
kmeans_within=function(k) {
  kmeans(pca_data[,-8], k, nstart = 10 )$tot.withinss
}
k.values=1:15
values_within=map_dbl(k.values,kmeans_within)
plot(k.values, values_within,type="b", pch = 19, frame = FALSE, xlab="Number of clusters K",ylab="Total within-clusters sum of squares")
kmean_pca=kmeans(pca_data[,-8], 9, nstart = 10, iter.max=100)
kmean_all=kmeans(scale(all_data[,-1]), 9, nstart = 10, iter.max=100)
distance_pca=dist(pca_data[,-8],method='euclidean')
distance_all=dist(scale(all_data[,-1]),method='euclidean')
mds_pca=as.data.frame(cmdscale(distance_pca, k=2))
mds_all=as.data.frame(cmdscale(distance_all, k=2))
mds_k9_pca=cbind(mds_pca, as.factor(kmean_pca$cluster),as.factor(all_data[,1]))
names(mds_k9_pca)=c('V1', 'V2', 'clust','original_class')
mds_k9_all=cbind(mds_all, as.factor(kmean_all$cluster),as.factor(all_data[,1]))
names(mds_k9_all)=c('V1', 'V2', 'clust','original_class')
ggplot(mds_k9_pca, aes(x=V2, y=V1, color=clust,shape=original_class))+geom_point()+scale_shape_manual(values=1:nlevels(mds_k9_pca$original_class))
ggplot(mds_k9_all, aes(x=V2, y=V1, color=clust,shape=original_class))+geom_point()+scale_shape_manual(values=1:nlevels(mds_k9_all$original_class))

plot(silhouette(kmean_pca$cluster, distance_pca), border=NA)
plot(silhouette(kmean_all$cluster, distance_all), border=NA)

kmean6=kmeans(pca_data[,-8], 4, nstart = 10, iter.max=100)
kmean6_all=kmeans(scale(all_data[,-1]), 6, nstart = 10, iter.max=100)
plot(silhouette(kmean6$cluster, distance_pca), border=NA)
abline(v=0.24)
plot(silhouette(kmean6_all$cluster, distance_all), border=NA)
abline(v=0.15)
