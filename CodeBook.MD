# CodeBook


### Input Data

For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.


### Transformation

**run_analysis.R** does the following:

1. Merges the training and the test sets to create one data set with names taken from *features.txt*
2. Extracts only the measurements on the mean and standard deviation for each measurement via regex
3. Uses descriptive activity names to name the activities in the data set taken from *activity_labels.txt*
4. Gathers the sensor measurements and separates by *doman, sensor, summary, and direction* of the signal value.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


### Output Data

For each record it is provided:

- A description of the activity.
- An identifier of the subject who carried out the experiment.
- A description of the domain (time or frequency) of the signal.
- A description of the sensor the measurement was taken from.
- The type of summary (mean or std) that was calculated from the sensor.
- The direction (X,Y,Z, or NA) of the signal.
- The mean value of the measurement for a given set of variables. Note that all values are normalized and bounded within [-1,1].


### References

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
