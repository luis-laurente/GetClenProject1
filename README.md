GetClenProject1
===============
run_analysis.R does the following:

    Reads the following files into R objects.
        features.txt -> features
        activity_labels.txt -> activity_labels
        X_test.txt -> X_test
        y_test.txt -> y_test
        subject_test.txt -> subject_test
        X_train.txt -> X_train
        y_train.txt -> y_train
        subject_train.txt -> subject_train
    Merges the R objects to create data sets test and train with subject number, descriptive activity names and variable names.
    Merges test and train to create one data set dat.
    Extracts only the measurements on the mean and standard deviation (variables whose names containing mean() or std()) for each measurement. tidy1
    Creates a second, independent tidy data set tidy2 with the average of each variable for each activity and each subject, and exports it to a text file tidy2.txt.
