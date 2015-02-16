#Merges the training and the test sets to create one data set.
##the test sets
subject_test<-read.table(".\\UCI HAR Dataset\\test\\subject_test.txt")
X_test<-read.table(".\\UCI HAR Dataset\\test\\X_test.txt")
y_test<-read.table(".\\UCI HAR Dataset\\test\\y_test.txt")
test<-data.frame(cbind(subject_test,y_test,X_test))
##the training sets
subject_train<-read.table(".\\UCI HAR Dataset\\train\\subject_train.txt")
X_train<-read.table(".\\UCI HAR Dataset\\train\\X_train.txt")
y_train<-read.table(".\\UCI HAR Dataset\\train\\y_train.txt")
train<-data.frame(cbind(subject_train,y_train,X_train))
all<-rbind(test,train)

#Appropriately labels the data set with descriptive variable names. 
names<-read.table(".\\UCI HAR Dataset\\features.txt",stringsAsFactors=FALSE)
names(all)[1:2] <- c("subject_train", "test_labels")
names(all)[3:563]<-names[,2]
names(all)<-gsub("-", "_", names(all))
names(all)<-gsub("\\(\\)", "", names(all))


#Extracts only the measurements on the mean and standard deviation for each measurement. 
i<-grep("mean\\(\\)",names(all))
j<-grep("std\\(\\)",names(all))
all<-all[,sort(c(1,2,i,j))]
        
#Uses descriptive activity names to name the activities in the data set
lable<-read.table(".\\UCI HAR Dataset\\activity_labels.txt",stringsAsFactors=FALSE)
for(i in (1:6)){
        all$test_labels<-sub(lable$V1[i],lable$V2[i],all$test_labels)}

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
result<-aggregate( all[,3] ~ test_labels+subject_train, data = all, FUN= "mean" )
for(i in (4:68)){
        res<-aggregate( all[,i] ~ test_labels+subject_train, data = all, FUN= "mean" )
        result[,i]<-res[,3]
}
##correct names of columns 
names(result)[3:68]<-names(all)[3:68]

write.table(result,file="result.txt",row.name=FALSE)
