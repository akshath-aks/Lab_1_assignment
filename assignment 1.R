name<-'Akshath Srinivas'
liuid<-'akssr921'
install.packages("devtools")
devtools::install_github("MansMeg/markmyassignment")

library(markmyassignment)
lab_path <-
  "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)


# my_num_vector
options(digits = 22)
my_num_vector <- function(){
  vector<- c(log10(11),cos(pi/5),exp(pi/3),(1173 %% 7)/19)
  return(vector)
}
my_num_vector()
mark_my_assignment(tasks="my_num_vector")


#Filter my vector
filter_my_vector<-function(x,leq){
  ifelse(x<leq,x,NA)
}
filter_my_vector(x = c(2, 9, 2, 4, 102), leq = 4)
mark_my_assignment(tasks="filter_my_vector")


#Dot Product
dot_prod<-function(a,b){
  vector<- c(a%*%b)
  return(vector)
}
dot_prod(a = c(-1,3), b = c(-3,-1))
mark_my_assignment(tasks="dot_prod")


#approx_e
options(digits = 5)
approx_e<-function(N)
{
  e<-0
  for (n in 0:N){
    e<-e+1/factorial(n)
  }
  return(e)
}
approx_e(4)
mark_my_assignment(tasks="approx_e")


#magic matrix
my_magic_matrix<- function(){
  m<-c(4,3,8,9,5,1,2,7,6)
  dim(m)=c(3,3)
  return(m)
}
my_magic_matrix()
mark_my_assignment(tasks="my_magic_matrix")
# my_magic_matrix<- function(){
#   my_data<-c(4,9,2,3,5,7,8,1,6)
#   A<-matrix(my_data,3,3,byrow=TRUE)
#   return(A)
# }
# my_magic_matrix()
# mark_my_assignment(tasks="my_magic_matrix")


#calculate number of elements
calculate_elements<-function(A){
  number_of_elements<-length(A)
  return(number_of_elements)
}
calculate_elements(my_magic_matrix())
mark_my_assignment(tasks="calculate_elements")


#row_to_zero
row_to_zero<-function(A,i){
  A[i,]<-matrix(0)
  return(A)
}
row_to_zero(my_magic_matrix(),i=3)
mark_my_assignment(tasks="row_to_zero")


#add_elements_to_matrix
add_elements_to_matrix<-function(A,x,i,j){
  A[i,j]<-A[i,j]+x
  return(A)
}
add_elements_to_matrix(my_magic_matrix(),x=-2, i=1:3, j=2:3)
mark_my_assignment(tasks="add_elements_to_matrix")


#magic_list
my_magic_list<-function(){
  x<-list(info='my own list', my_num_vector(), my_magic_matrix())
  return(x)
}
my_magic_list()
mark_my_assignment(tasks="my_magic_list")


#change_info
change_info<-function(x,text){
  x['info']<-text
  return(x)
}
change_info(my_magic_list(),text='Some new info')
mark_my_assignment(tasks="change_info")


#add_note
add_note<-function(x,note){
  x<-append(x,note)
  names(x)[length(x)]<-'note'
  return(x)
}
add_note(my_magic_list(),note = "This is a magic list!")
mark_my_assignment(tasks="add_note")


#sum_numeric_parts
sum_numeric_parts<-function(x){
  vector<-unlist(x)
  c<-as.numeric(vector)
  return(sum(c,na.rm=FALSE))
}
sum_numeric_parts(my_magic_list())
mark_my_assignment(tasks="sum_numeric_parts")


#my_data.frame()
my_data.frame<-function(){
  id<-c(1,2,3)
  name<-c('John','Lizra','Asra')
  income<-c(7.30,0.00,15.21)
  rich<-c(FALSE,FALSE,TRUE)
  my_df<-data.frame(id,name,income,rich,stringsAsFactors = FALSE)
  return(my_df)
}
my_data.frame()
mark_my_assignment(tasks="my_data.frame")


#sort_head
sort_head<-function(df,var.name,n){
  r<-order(df[,var.name],decreasing = TRUE)
  v<-df[r,]
  return(head(v,n))
}
sort_head(df=iris, var.name='Petal.Length',n=5)
mark_my_assignment(tasks="sort_head")


#add_median_variable
add_median_variable<-function(df,j){
  m<-median(df[,j])
  comparision<-c(ifelse(df[,j]>m,'Greater',ifelse(df[,j]<m,'Smaller','Median')))
  df[,'compared_to_median']<-comparision
  return(df)
}
data("faithful")
head(add_median_variable(df = faithful, 1))
mark_my_assignment(tasks="add_median_variable")


#analyze columns
analyze_columns<-function(df,j){
  mean_x<-apply(df[,c(j[1],j[2])],2,function(x)mean(x))
  median_y<-apply(df[,c(j[1],j[2])],2,function(x)median(x))
  sd_z<-apply(df[,c(j[1],j[2])],2,function(x)sd(x))
  corelation<-cor(df[,c(j[1],j[2])])
  vector1<-c(mean_x[1],median_y[1],sd_z[1])
  vector2<-c(mean_x[2],median_y[2],sd_z[2])
  names(vector1)<-c('mean','median','sd')
  names(vector2)<-c('mean','median','sd')
  
  combining_list<-list(vector1,vector2,corelation)
  names(combining_list)<-colnames(df[,c(j[1],j[2])])
  names(combining_list)[3]<-'correlation_matrix'
  return(combining_list) 
}
analyze_columns(df = faithful, 1:2)
mark_my_assignment(tasks="analyze_columns")

mark_my_assignment()

