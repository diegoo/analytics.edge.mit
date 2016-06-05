set.seed(1234)
train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

relevant.variables <- c("USER_ID", "EducationLevel","Income","YOB","HouseholdStatus","Gender","Q98197","Q98059","Q108343","Q109244","Q109367","Q115611")
train.relevant <- train[c(relevant.variables, "Party")]
test.relevant <- test[relevant.variables]

## --------------------------------------------------------------------------------

train.relevant[4830,]$YOB = 1980
train.relevant[4864,]$YOB = 1981
train.relevant[5054,]$YOB = 1996

train.relevant <- subset(train.relevant, YOB <= 2016)

train.relevant$YOB <- cut(train.relevant$YOB, breaks = c(0, 1900, 1960, 1970, 1980, 1990, 2000, 2020), labels = c("UNKNOWN", "1900-1960", "1960-1970", "1970-1980", "1980-1990", "1990-2000", "2000-2020"))

test.relevant[which(is.na(test.relevant$YOB))]$YOB <- 0

test.relevant$YOB <- cut(test.relevant$YOB, breaks = c(0, 1900, 1960, 1970, 1980, 1990, 2000, 2020), labels = c("UNKNOWN", "1900-1960", "1960-1970", "1970-1980", "1980-1990", "1990-2000", "2000-2020"))

test.relevant[which(is.na(test.relevant$YOB)), c("YOB")] <- as.factor("UNKNOWN")

## --------------------------------------------------------------------------------

model.log = glm(Party ~ . -USER_ID, data=train.relevant, family=binomial)
model.log.predictions.test <- predict(model.log, newdata=test.relevant, type="response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(model.log.predictions.test < threshold, "Democrat", "Republican"))

submission = data.frame(USER_ID = test.relevant$USER_ID, Predictions = PredTestLabels)
write.csv(submission, "submission.3.log.csv", row.names=FALSE)

## --------------------------------------------------------------------------------

## NAs por columna
sapply(test.relevant, function(x) sum(is.na(x)))

sum(is.na(train.relevant$Income))

## --------------------------------------------------------------------------------

## subset(train.relevant, YOB < 1900)
##           EducationLevel              Income  YOB             HouseholdStatus
## 4830 High School Diploma  $75,000 - $100,000 1880           Married (no kids)
## 4864     Master's Degree $100,001 - $150,000 1881            Married (w/kids)
## 5054        Current K-12   $25,001 - $50,000 1896 Domestic Partners (no kids)
##      Gender Q109244 Q115611 Q98197      Party
## 4830   Male                        Republican
## 4864 Female      No                Republican
## 5054 Female                          Democrat

## remove NA
## dfOk = df[!is.na(df$YOB), ]

## t-test
## Q109244 = as.numeric(train$Q109244) - 1
## t.test(Q109244[train$Party == 'Democrat'], Q109244[train$Party == 'Republican'])

## "Q109244" "Q115611" "Q98197" "Q113181" "Q98869" "Q101163" "Q99480" "Q105840" "Q120379" "Q116881" "Q106272" "Q120472" "Q115899" "Q102089" "Q110740" "Q119851" "Q121699" "Q115195" "Q106042" "Q118232" "Q100680" "Q118892"

## (Q109244) Are you a feminist?
## (Q115611) Do you personally own a gun?
## (Q98197) Do you pray or meditate on a regular basis?
## (Q113181) Do you meditate or pray on a regular basis?
## (Q98869) Does life have a purpose?
## (Q101163) Which parent "wore the pants" in your household?
## (Q99480) Did your parents spank you as a form of discipline/punishment?
## (Q105840) Do you ever treat yourself to "retail therapy"?
## (Q120379) Do you have (or plan to pursue) a Masters or Doctoral degree?
## (Q116881) Would you rather be happy or right?
## (Q106272) Do you own any power tools? (power saws, drills, etc.)
## (Q120472) Science or Art?
## (Q115899) Would you say most of the hardship in your life has been the result of circumstances beyond your own control, or has it been mostly the result of your own decisions and actions?
## (Q102089) Do you rent or own your primary residence?
## (Q110740) Mac or PC?
## (Q119851) Are you in the middle of reading a good book right now?
## (Q121699) 2013: did you drink alcohol?
## (Q115195) Do you live within 20 miles of a major metropolitan area?
## (Q106042) Are you taking any prescription medications?
## (Q118232) Are you more of an idealist or a pragmatist?
## (Q100680) Have you cried in the past 60 days?
## (Q118892) Do you wear glasses or contact lenses?
## (Q107869) Do you feel like you're "normal"?

## subset(test.relevant, EducationLevel == "Current K-12")$YOB <- as.factor("1990-2000")
## [1] ""                      "Associate's Degree"    "Bachelor's Degree"    
## [4] "Current K-12"          "Current Undergraduate" "Doctoral Degree"      
## [7] "High School Diploma"   "Master's Degree"

## addNA(train.relevant$EducationLevel)
## addNA(train.relevant$Income)
## addNA(train.relevant$HouseholdStatus)
## addNA(train.relevant$Gender)
## addNA(train.relevant$Q98197)
## addNA(train.relevant$Q98059)
## addNA(train.relevant$Q108343)
## addNA(train.relevant$Q109244)
## addNA(train.relevant$Q109367)
## addNA(train.relevant$Q115611)

## addNA(test.relevant$EducationLevel)
## addNA(test.relevant$Income)
## addNA(test.relevant$HouseholdStatus)
## addNA(test.relevant$Gender)
## addNA(test.relevant$Q98197)
## addNA(test.relevant$Q98059)
## addNA(test.relevant$Q108343)
## addNA(test.relevant$Q109244)
## addNA(test.relevant$Q109367)
## addNA(test.relevant$Q115611)
