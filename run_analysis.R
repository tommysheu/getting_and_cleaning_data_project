
## 1. Merges the training and the test sets to create one data set.
data_train <- read.table('train/X_train.txt');
data_test <- read.table('test/X_test.txt');
data_all <- rbind(data_train,data_test);

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
col = ncol(data_all);
data_all.mean = vector('numeric',length=0);
data_all.sd = vector('numeric',length=0);
for (i in 1:col) {
  data_all.mean = c(data_all.mean, mean(data_all[,i]));
  data_all.sd = c(data_all.sd, sd(data_all[,i]));
}
data_all.mean
data_all.sd

## 3. Uses descriptive activity names to name the activities in the data set
activity_train <- read.table('train/y_train.txt');
activity_test <- read.table('test/y_test.txt');
activity_all <- rbind(activity_train,activity_test);
activity_labels <- read.table("activity_labels.txt");
label_name <- as.character(activity_labels$V2);
row = nrow(activity_all);
activity_all.name = vector('character',length=0);
for (i in 1:row) {
  activity_all.name <- c(activity_all.name,label_name[activity_all[i,1]]);
}
activity_all.name2 <- factor(activity_all.name,levels = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"));
activity_all.name2;

## 4. Appropriately labels the data set with descriptive activity names. 
data_all2 <- cbind(activity=activity_all.name2,data_all);
head(data_all2);

## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#by activity
data_activity.mean <- vector('numeric',length=0);
for (i in label_name) {
  data_by_activity <- data_all2[data_all2$activity==i,];
  data_by_activity.mean = vector('numeric',length=0);
  for (j in 2:ncol(data_by_activity)) {
    data_by_activity.mean = c(data_by_activity.mean, mean(data_by_activity[,j]));
  }
  data_activity.mean = rbind(data_activity.mean,data_by_activity.mean)
}
data_activity.mean <- data.frame(activity=label_name,data_activity.mean);
head(data_activity.mean)

#by subject
subject_train <- read.table('train/subject_train.txt');
subject_test <- read.table('test/subject_test.txt');
subject_all <- rbind(subject_train,subject_test);
subject.max = max(subject_all);
data_subject.mean <- vector('numeric',length=0);
for (i in 1:subject.max) {
  data_by_subject = data_all2[subject_all==i,];
  data_by_subject.mean = vector('numeric',length=0);
  for (j in 2:ncol(data_by_subject)) {
    data_by_subject.mean = c(data_by_subject.mean, mean(data_by_subject[,j]));
  }
  data_subject.mean = rbind(data_subject.mean,data_by_subject.mean)
}
data_subject.mean <- data.frame(subject=1:subject.max,data_subject.mean);
head(data_subject.mean)

# by activity and subject
data_activity_subject.mean <- vector('numeric',length=0);
for (i in label_name) {
  for (k in 1:subject.max) {
    data_by_activity_subject <- data_all2[data_all2$activity==i & subject_all==k,];
    data_by_activity_subject.mean = vector('numeric',length=0);
    for (j in 2:ncol(data_by_activity)) {
      data_by_activity_subject.mean = c(data_by_activity_subject.mean, mean(data_by_activity_subject[,j]));
    }
    data_activity_subject.mean = rbind(data_activity_subject.mean,data_by_activity_subject.mean)
  }
}

activity1 <- vector('character',length=0);
subject1 <- vector('numeric',length=0);
for (i in label_name) {
  for (k in 1:subject.max) {
    activity1 <- c(activity1,i);
    subject1 <- c(subject1,k);
  }
}
activity1
subject1

data_activity_subject.mean <- data.frame(activity=activity1,subject=subject1,data_activity_subject.mean);
head(data_activity_subject.mean)
write.table(data_activity_subject.mean,file="data_activity_subject_average.txt")
