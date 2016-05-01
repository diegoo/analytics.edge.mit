grade: The grade in school of the student (most 15-year-olds in America are in 10th grade)
male: Whether the student is male (1/0)
raceeth: The race/ethnicity composite of the student
preschool: Whether the student attended preschool (1/0)
expectBachelors: Whether the student expects to obtain a bachelor's degree (1/0)
motherHS: Whether the student's mother completed high school (1/0)
motherBachelors: Whether the student's mother obtained a bachelor's degree (1/0)
motherWork: Whether the student's mother has part-time or full-time work (1/0)
fatherHS: Whether the student's father completed high school (1/0)
fatherBachelors: Whether the student's father obtained a bachelor's degree (1/0)
fatherWork: Whether the student's father has part-time or full-time work (1/0)
selfBornUS: Whether the student was born in the United States of America (1/0)
motherBornUS: Whether the student's mother was born in the United States of America (1/0)
fatherBornUS: Whether the student's father was born in the United States of America (1/0)
englishAtHome: Whether the student speaks English at home (1/0)
computerForSchoolwork: Whether the student has access to a computer for schoolwork (1/0)
read30MinsADay: Whether the student reads for pleasure for 30 minutes/day (1/0)
minutesPerWeekEnglish: The number of minutes per week the student spend in English class
studentsInEnglish: The number of students in this student's English class at school
schoolHasLibrary: Whether this student's school has a library (1/0)
publicSchool: Whether this student attends a public school (1/0)
urban: Whether this student's school is in an urban area (1/0)
schoolSize: The number of students in this student's school
readingScore: The student's reading score, on a 1000-point scale

--------------------------------------------------------------------------------

1.1

pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")

--------------------------------------------------------------------------------

1.2

tapply(pisaTrain$readingScore, pisaTrain$male, mean)
    0        1 
512.9406 483.5325

--------------------------------------------------------------------------------

1.3

colnames(pisaTrain)[colSums(is.na(pisaTrain)) > 0]

 [1] "raceeth"               "preschool"             "expectBachelors"      
 [4] "motherHS"              "motherBachelors"       "motherWork"           
 [7] "fatherHS"              "fatherBachelors"       "fatherWork"           
[10] "selfBornUS"            "motherBornUS"          "fatherBornUS"         
[13] "englishAtHome"         "computerForSchoolwork" "read30MinsADay"       
[16] "minutesPerWeekEnglish" "studentsInEnglish"     "schoolHasLibrary"     
[19] "schoolSize"           

--------------------------------------------------------------------------------

1.4

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

--------------------------------------------------------------------------------

2.2

dummy variables

To include unordered factors in a linear regression model, we define one level as the "reference level" and add a binary variable for each of the remaining levels. In this way, a factor with n levels is replaced by n-1 binary variables. The reference level is typically selected to be the most frequently occurring level in the dataset.
As an example, consider the unordered factor variable "color", with levels "red", "green", and "blue". If "green" were the reference level, then we would add binary variables "colorred" and "colorblue" to a linear regression problem. All red examples would have colorred=1 and colorblue=0. All blue examples would have colorred=0 and colorblue=1. All green examples would have colorred=0 and colorblue=0.
Now, consider the variable "raceeth" in our problem, which has levels "American Indian/Alaska Native", "Asian", "Black", "Hispanic", "More than one race", "Native Hawaiian/Other Pacific Islander", and "White". Because it is the most common in our population, we will select White as the reference level.

--------------------------------------------------------------------------------

3.1

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore <- lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)

--------------------------------------------------------------------------------

3.2

predictions.training <- predict(lmScore, newdata = pisaTrain)
SSE.training <- sum((predictions.training - pisaTrain$readingScore)^2)
RMSE.training <- sqrt(SSE.training / nrow(pisaTrain))

73.36555

--------------------------------------------------------------------------------

3.3

factores de grade = 8 9 10 11 12

2 * 29.54

--------------------------------------------------------------------------------

3.4

The only difference between an Asian student and white student with otherwise identical variables is that the former has raceethAsian=1 and the latter has raceethAsian=0. The predicted reading score for these two students will differ by the coefficient on the variable raceethAsian.

--------------------------------------------------------------------------------

3.5

(Intercept)                                   0.0000223707046024 ***
grade                                                    < 2e-16 ***
male                                          0.0000044166010175 ***
raceethAmerican Indian/Alaska Native          0.0000631881085539 ***
raceethAsian                                             0.65578    
raceethBlack                                             < 2e-16 ***
raceethHispanic                               0.0000000000000729 ***
raceethMore than one race                                0.04651 *  
raceethNative Hawaiian/Other Pacific Islander            0.76421    
preschool                                                0.20052    
expectBachelors                                          < 2e-16 ***
motherHS                                                 0.32001    
motherBachelors                                          0.00108 ** 
motherWork                                               0.42517    
fatherHS                                                 0.47147    
fatherBachelors                               0.0000234647671874 ***
fatherWork                                               0.18393    
selfBornUS                                               0.60331    
motherBornUS                                             0.18182    
fatherBornUS                                             0.49178    
englishAtHome                                            0.24153    
computerForSchoolwork                         0.0000818877616071 ***
read30MinsADay                                           < 2e-16 ***
minutesPerWeekEnglish                                    0.23264    
studentsInEnglish                                        0.20846    
schoolHasLibrary                                         0.18749    
publicSchool                                             0.01226 *  
urban                                                    0.97783    
schoolSize                                               0.00294 ** 

--------------------------------------------------------------------------------

4.1
    
predTest <- predict(lmScore, newdata = pisaTest)
summary(predTest)
max(predTest) - min(predTest)

--------------------------------------------------------------------------------

4.2

SSE.testing <- sum((predTest - pisaTest$readingScore)^2)
RMSE.testing <- sqrt(SSE.testing / nrow(pisaTest))

--------------------------------------------------------------------------------

4.3

predictions.training <- predict(lmScore, newdata = pisaTrain)
mean(predictions.training)
[1] 517.9629

SST.testing <- sum((516.7103 - pisaTest$readingScore)^2).
7802354

--------------------------------------------------------------------------------

4.4

The test-set R^2 is defined as 1-SSE/SST, where SSE is the sum of squared errors of the model on the test set and SST is the sum of squared errors of the baseline model. For this model, the R^2 is then computed to be 1-5762082/7802354.

SSE.testing <- sum((predTest - pisaTest$readingScore)^2)
SST.testing <- sum((516.7103 - pisaTest$readingScore)^2)
r.squared.test.set <- 1 - (SSE.testing / SST.testing)

--------------------------------------------------------------------------------
