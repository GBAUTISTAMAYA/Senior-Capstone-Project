# University of North Georgia
# Math Department
# Professor Ping Ye
# Gildardo Bautista-Maya

# Boy with a Ball Questionaire Analysis

# Importing data from refined dataset
df2 <- read.csv('newHEMINGWAY.csv')

# Setting values to Hemingway Standard
df2[df2 == 2.5] <- 3

# Reversing scores of questions marked 'reverse scored'
rsq <- df2[,c('VHemA2', 'VHemA7', 'VHemA13', 'VHemA18', 'VHemA26', 'VHemA30', 'VHemA34', 'VHemA45', 'VHemA55')]
rsq[rsq == 5] <- 6
rsq[rsq == 1] <- 5
rsq[rsq == 6] <- 1
rsq[rsq == 4] <- 6
rsq[rsq == 2] <- 4
rsq[rsq == 6] <- 2

# Inserting reversed values 
cv <- df2
cv$VHemA2 <- rsq$VHemA2
cv$VHemA7 <- rsq$VHemA7
cv$VHemA13 <- rsq$VHemA13
cv$VHemA18 <- rsq$VHemA18
cv$VHemA26 <- rsq$VHemA26
cv$VHemA30 <- rsq$VHemA30
cv$VHemA34 <- rsq$VHemA34
cv$VHemA45 <- rsq$VHemA45
cv$VHemA55 <- rsq$VHemA55

# Factoring
str(cv)
factor(cv$Program)

# Seperating data based on program completion
# Completed program
com <- subset(cv, subset = Program == 1)
# Did not complete program
dnc <- subset(cv, subset = Program == 0)

# Creating dataframe of score averages
q1 = c(mean(com$VHemA1), mean(dnc$VHemA1))
q2 = c(mean(com$VHemA2), mean(dnc$VHemA2))
q3 = c(mean(com$VHemA3), mean(dnc$VHemA3))
q4 = c(mean(com$VHemA4), mean(dnc$VHemA4))
q5 = c(mean(com$VHemA5), mean(dnc$VHemA5))
q6 = c(mean(com$VHemA6), mean(dnc$VHemA6))
q7 = c(mean(com$VHemA7), mean(dnc$VHemA7))
q8 = c(mean(com$VHemA8), mean(dnc$VHemA8))
q9 = c(mean(com$VHemA9), mean(dnc$VHemA9))
q10 = c(mean(com$VHemA10), mean(dnc$VHemA10))
q11 = c(mean(com$VHemA11), mean(dnc$VHemA11))
q12 = c(mean(com$VHemA12), mean(dnc$VHemA12))
q13 = c(mean(com$VHemA13), mean(dnc$VHemA13))
q14 = c(mean(com$VHemA14), mean(dnc$VHemA14))
q15 = c(mean(com$VHemA15), mean(dnc$VHemA15))
q16 = c(mean(com$VHemA16), mean(dnc$VHemA16))
q17 = c(mean(com$VHemA17), mean(dnc$VHemA17))
q18 = c(mean(com$VHemA18), mean(dnc$VHemA18))
q19 = c(mean(com$VHemA19), mean(dnc$VHemA19))
q20 = c(mean(com$VHemA20), mean(dnc$VHemA20))
q21 = c(mean(com$VHemA21), mean(dnc$VHemA21))
q22 = c(mean(com$VHemA22), mean(dnc$VHemA22))
q23 = c(mean(com$VHemA23), mean(dnc$VHemA23))
q24 = c(mean(com$VHemA24), mean(dnc$VHemA24))
q25 = c(mean(com$VHemA25), mean(dnc$VHemA25))
q26 = c(mean(com$VHemA26), mean(dnc$VHemA26))
q27 = c(mean(com$VHemA27), mean(dnc$VHemA27))
q28 = c(mean(com$VHemA28), mean(dnc$VHemA28))
q29 = c(mean(com$VHemA29), mean(dnc$VHemA29))
q30 = c(mean(com$VHemA30), mean(dnc$VHemA30))
q31 = c(mean(com$VHemA31), mean(dnc$VHemA31))
q32 = c(mean(com$VHemA32), mean(dnc$VHemA32))
q33 = c(mean(com$VHemA33), mean(dnc$VHemA33))
q34 = c(mean(com$VHemA34), mean(dnc$VHemA34))
q35 = c(mean(com$VHemA35), mean(dnc$VHemA35))
q36 = c(mean(com$VHemA36), mean(dnc$VHemA36))
q37 = c(mean(com$VHemA37), mean(dnc$VHemA37))
q38 = c(mean(com$VHemA38), mean(dnc$VHemA38))
q39 = c(mean(com$VHemA39), mean(dnc$VHemA39))
q40 = c(mean(com$VHemA40), mean(dnc$VHemA40))
q41 = c(mean(com$VHemA41), mean(dnc$VHemA41))
q42 = c(mean(com$VHemA42), mean(dnc$VHemA42))
q43 = c(mean(com$VHemA43), mean(dnc$VHemA43))
q44 = c(mean(com$VHemA44), mean(dnc$VHemA44))
q45 = c(mean(com$VHemA45), mean(dnc$VHemA45))
q46 = c(mean(com$VHemA46), mean(dnc$VHemA46))
q47 = c(mean(com$VHemA47), mean(dnc$VHemA47))
q48 = c(mean(com$VHemA48), mean(dnc$VHemA48))
q49 = c(mean(com$VHemA49), mean(dnc$VHemA49))
q50 = c(mean(com$VHemA50), mean(dnc$VHemA50))
q51 = c(mean(com$VHemA51), mean(dnc$VHemA51))
q52 = c(mean(com$VHemA52), mean(dnc$VHemA52))
q53 = c(mean(com$VHemA53), mean(dnc$VHemA53))
q54 = c(mean(com$VHemA54), mean(dnc$VHemA54))
q55 = c(mean(com$VHemA55), mean(dnc$VHemA55))
q56 = c(mean(com$VHemA56), mean(dnc$VHemA56))
q57 = c(mean(com$VHemA57), mean(dnc$VHemA57))
qavg <- data.frame(Q1 = q1, Q2 = q2, Q3 = q3, Q4 = q4, Q5 = q5, Q6 = q6, Q7 = q7, Q8 = q8, Q9 = q9, Q10 = q10, Q11 = q11, Q12 = q12, Q13 = q13, Q14 = q14, Q15 = q15, Q16 = q16, Q17 = q17, Q18 = q18, Q19 = q19, Q20 = q20, Q21 = q21, Q22 = q22, Q23 = q23, Q24 = q24, Q25 = q25, Q26 = q26, Q27 = q27, Q28 = q28, Q29 = q29, Q30 = q30, Q31 = q31, Q32 = q32, Q33 = q33, Q34 = q34, Q35 = q35, Q36 = q36, Q37 = q37, Q38 = q38, Q39 = q39, Q40 = q40, Q41 = q41, Q42 = q42, Q43 = q43, Q44 = q44, Q45 = q45, Q46 = q46, Q47 = q47, Q48 = q48, Q49 = q49, Q50 = q50, Q51 = q51, Q52 = q52, Q53 = q53, Q54 = q54, Q55 = q55, Q56 = q56, Q57 = q57)
row.names(qavg) = c('Completed', 'Did Not Complete')
colnames(qavg) = c('Q1 Avg', 'Q2 Avg', 'Q3 Avg', 'Q4 Avg', 'Q5 Avg', 'Q6 Avg', 'Q7 Avg', 'Q8 Avg', 'Q9 Avg', 'Q10 Avg', 'Q11 Avg', 'Q12 Avg', 'Q13 Avg', 'Q14 Avg', 'Q15 Avg', 'Q16 Avg', 'Q17 Avg', 'Q18 Avg', 'Q19 Avg', 'Q20 Avg', 'Q21 Avg', 'Q22 Avg', 'Q23 Avg', 'Q24 Avg', 'Q25 Avg', 'Q26 Avg', 'Q27 Avg', 'Q28 Avg', 'Q29 Avg', 'Q30 Avg', 'Q31 Avg', 'Q32 Avg', 'Q33 Avg', 'Q34 Avg', 'Q35 Avg', 'Q36 Avg', 'Q37 Avg', 'Q38 Avg', 'Q39 Avg', 'Q40 Avg', 'Q41 Avg', 'Q42 Avg', 'Q43 Avg', 'Q44 Avg', 'Q45 Avg', 'Q46 Avg', 'Q47 Avg', 'Q48 Avg', 'Q49 Avg', 'Q50 Avg', 'Q51 Avg', 'Q52 Avg', 'Q53 Avg', 'Q54 Avg', 'Q55 Avg', 'Q56 Avg', 'Q57 Avg')

# Creating a function to determine significant differences in averages
significant <- function(i){
  s <- sqrt((((nrow(com) - 1) * (sd(com[,6 + i]))^2) + ((nrow(dnc) - 1) * (sd(dnc[,6 + i]))^2))/(nrow(com) + nrow(dnc) - 2))
  se <- s * sqrt((1/nrow(com)) + (1/nrow(dnc)))
  t <- round((qavg[1,i] - qavg[2,i])/se, digits = 3)
  p <- round(2*pt(-abs(t), df = nrow(com) + nrow(dnc) - 2), digits = 3)
  sig <- if(p < 0.05){
    'Yes'
  } else{
    'No'
  }
  print(paste0(sig, ", ", t, ", ", p))
}

# Using function to determine significant questions
x <- 57
output <- matrix(ncol = 1, nrow = x)
for (i in 1:x) {
  output[i,] <- significant(i)
}
output <- data.frame(output)

# Using tidyr to seperate data
output <- (output %>% separate(output, c('Significant', 't', 'p-value'), sep = ','))
sigq <- order(output$`p-value`)
sigq <- output[sigq,]
sigq
#    Significant       t p-value
# 6          Yes   4.277       0
# 8          Yes   5.415       0
# 10         Yes   4.264       0
# 16         Yes   5.611       0
# 20         Yes    4.28       0
# 22         Yes   4.003       0
# 30         Yes    3.92       0
# 38         Yes   3.976       0
# 39         Yes   4.955       0
# 40         Yes   5.142       0
# 49         Yes   3.666       0
# 52         Yes   4.061       0
# 26         Yes   3.485   0.001
# 35         Yes   3.468   0.001
# 36         Yes   3.318   0.001
# 3          Yes    3.04   0.003
# 19         Yes   2.964   0.003
# 29         Yes    2.98   0.003
# 28         Yes   2.899   0.004
# 23         Yes   2.868   0.005
# 48         Yes   2.862   0.005
# 50         Yes   2.825   0.005
# 32         Yes   2.634   0.009
# 17         Yes   2.582    0.01
# 46         Yes   2.546   0.012
# 12         Yes   2.366   0.019
# 37         Yes   2.292   0.023
# 42         Yes   2.215   0.028
# 47         Yes   2.117   0.035
# 57         Yes   2.084   0.038
# 54         Yes   2.023   0.044
# 24         Yes   1.995   0.047
# 34          No   1.971    0.05
# 56          No   1.903   0.058
# 18          No   1.898   0.059
# 4           No    1.82    0.07
# 53          No    1.81   0.072
# 7           No  -1.786   0.075
# 14          No   1.725   0.086
# 43          No   1.563    0.12
# 1           No   -1.46   0.146
# 15          No   1.437   0.152
# 9           No   1.423   0.156
# 45          No   1.325   0.187
# 44          No    1.18   0.239
# 13          No  -1.088   0.278
# 25          No   1.047   0.296
# 27          No   0.994   0.321
# 2           No   0.921   0.358
# 41          No   0.913   0.362
# 33          No   0.617   0.538
# 5           No   0.573   0.567
# 55          No   0.545   0.586
# 31          No  -0.403   0.687
# 21          No    0.37   0.712
# 51          No   0.352   0.725
# 11          No    0.07   0.944

# Start of Hemingway Scoring (com is for completed program, dnc is for did not complete program)
# Hemingway Scoring - Neighborhood
comHemCommunity <- sum(com$VHemA1, com$VHemA11, com$VHemA21, com$VHemA31, com$VHemA41, com$VHemA51) / (6 * nrow(com) * 5)
comHemCommunity
# 0.5964912
dncHemCommunity <- sum(dnc$VHemA1, dnc$VHemA11, dnc$VHemA21, dnc$VHemA31, dnc$VHemA41, dnc$VHemA51) / (6 * nrow(dnc) * 5)
dncHemCommunity
# 0.5961538

# Hemingway Scoring - Friends
comHemFriends <- sum(com$VHemA2, com$VHemA12, com$VHemA22, com$VHemA32, com$VHemA42, com$VHemA52) / (6 * nrow(com) * 5)
comHemFriends
# 0.8701754
dncHemFriends <- sum(dnc$VHemA2, dnc$VHemA12, dnc$VHemA22, dnc$VHemA32, dnc$VHemA41, dnc$VHemA52) / (6 * nrow(dnc) * 5)
dncHemFriends
# 0.7278388

# Hemingway Scoring - Present-Self
comHemPresentSelf <- sum(com$VHemA3, com$VHemA13, com$VHemA23, com$VHemA33, com$VHemA43, com$VHemA53) / (6 * nrow(com) * 5)
comHemPresentSelf
# 0.7824561
dncHemPresentSelf <- sum(dnc$VHemA3, dnc$VHemA13, dnc$VHemA23, dnc$VHemA33, dnc$VHemA43, dnc$VHemA53) / (6 * nrow(dnc) * 5)
dncHemPresentSelf
# 0.7333333

# Hemingway Scoring - Parents
comHemParents <- sum(com$VHemA4, com$VHemA14, com$VHemA24, com$VHemA34, com$VHemA44, com$VHemA54) / (6 * nrow(com) * 5)
comHemParents
# 0.8026316
dncHemParents <- sum(dnc$VHemA4, dnc$VHemA14, dnc$VHemA24, dnc$VHemA34, dnc$VHemA44, dnc$VHemA54) / (6 * nrow(dnc) * 5)
dncHemParents
# 0.7344322

# Hemingway Scoring - Siblings
comHemSiblings <- sum(com$VHemA5, com$VHemA15, com$VHemA25, com$VHemA35, com$VHemA45) / (5 * nrow(com) * 5)
comHemSiblings
# 0.7852632
dncHemSiblings <- sum(dnc$VHemA5, dnc$VHemA15, dnc$VHemA25, dnc$VHemA35, dnc$VHemA45) / (5 * nrow(dnc) * 5)
dncHemSiblings
# 0.7158242

# Hemingway Scoring - School
comHemSchool <- sum(com$VHemA6, com$VHemA16, com$VHemA26, com$VHemA36, com$VHemA46, com$VHemA56) / (6 * nrow(com) * 5)
comHemSchool
# 0.7982456
dncHemSchool <- sum(dnc$VHemA6, dnc$VHemA16, dnc$VHemA26, dnc$VHemA36, dnc$VHemA46, dnc$VHemA56) / (6 * nrow(dnc) * 5)
dncHemSchool
# 0.674359

# Hemingway Scoring - Peers
comHemPeers <- sum(com$VHemA7, com$VHemA17, com$VHemA27, com$VHemA37, com$VHemA47, com$VHemA57) / (6 * nrow(com) * 5)
comHemPeers
# 0.7473684
dncHemPeers <- sum(dnc$VHemA7, dnc$VHemA17, dnc$VHemA27, dnc$VHemA37, dnc$VHemA47, dnc$VHemA57) / (6 * nrow(dnc) * 5)
dncHemPeers
# 0.6985348

# Hemingway Scoring - Teachers
comHemTeachers <- sum(com$VHemA8, com$VHemA18, com$VHemA28, com$VHemA38, com$VHemA48, com$VHemA50) / (6 * nrow(com) * 5)
comHemTeachers
# 0.822807
dncHemTeachers <- sum(dnc$VHemA8, dnc$VHemA18, dnc$VHemA28, dnc$VHemA38, dnc$VHemA48, dnc$VHemA50) / (6 * nrow(dnc) * 5)
dncHemTeachers
# 0.6934066

# Hemingway Scoring - Future-Self
comHemFutureSelf <- sum(com$VHemA9, com$VHemA19, com$VHemA29, com$VHemA39, com$VHemA49, com$VHemA55) / (6 * nrow(com) * 5)
comHemFutureSelf
# 0.8315789
dncHemFutureSelf <- sum(dnc$VHemA9, dnc$VHemA19, dnc$VHemA29, dnc$VHemA39, dnc$VHemA49, dnc$VHemA55) / (6 * nrow(dnc) * 5)
dncHemFutureSelf
# 0.7305861

# Hemingway Scoring - Reading
comHemReading <- sum(com$VHemA10, com$VHemA20, com$VHemA30, com$VHemA40) / (4 * nrow(com) * 5)
comHemReading
# 0.7105263
dncHemReading <- sum(dnc$VHemA10, dnc$VHemA20, dnc$VHemA30, dnc$VHemA40) / (4 * nrow(dnc) * 5)
dncHemReading
# 0.5043956

# Determining significance of score catagories
# Setting individual scoring of catagories
coms <- com[,-c(1, 2, 3, 4, 5, 6)]
dncs <- dnc[,-c(1, 2, 3, 4, 5, 6)]
# Creating new score columns (Completed)
coms[['Neighborhood']] <- (coms$VHemA1 + coms$VHemA11 + coms$VHemA21 + coms$VHemA31 + coms$VHemA41 + coms$VHemA51)/(6 * 5)
coms[['Friends']] <- (coms$VHemA2 + coms$VHemA12 + coms$VHemA22 + coms$VHemA32 + coms$VHemA42 + coms$VHemA52)/(6 * 5)
coms[['PresentSelf']] <- (coms$VHemA3 + coms$VHemA13 + coms$VHemA23 + coms$VHemA33 + coms$VHemA43 + coms$VHemA53)/(6 * 5)
coms[['Parents']] <- (coms$VHemA4 + coms$VHemA14 + coms$VHemA24 + coms$VHemA34 + coms$VHemA44 + coms$VHemA54)/(6 * 5)
coms[['Siblings']] <- (coms$VHemA5 + coms$VHemA15 + coms$VHemA25 + coms$VHemA35 + coms$VHemA45)/(5 * 5)
coms[['School']] <- (coms$VHemA6 + coms$VHemA16 + coms$VHemA26 + coms$VHemA36 + coms$VHemA46 + coms$VHemA56)/(6 * 5)
coms[['Peers']] <- (coms$VHemA7 + coms$VHemA17 + coms$VHemA27 + coms$VHemA37 + coms$VHemA47 + coms$VHemA57)/(6 * 5)
coms[['Teachers']] <- (coms$VHemA8 + coms$VHemA18 + coms$VHemA28 + coms$VHemA38 + coms$VHemA48 + coms$VHemA50)/(6 * 5)
coms[['FutureSelf']] <- (coms$VHemA9 + coms$VHemA19 + coms$VHemA29 + coms$VHemA39 + coms$VHemA49 + coms$VHemA55)/(6 * 5)
coms[['Reading']] <- (coms$VHemA10 + coms$VHemA20 + coms$VHemA30 + coms$VHemA40)/(4 * 5)
coms <- coms[,c('Neighborhood', 'Friends', 'PresentSelf', 'Parents', 'Siblings', 'School', 'Peers', 'Teachers', 'FutureSelf', 'Reading')]
# Creating new score columns (Did not complete)
dncs[['Neighborhood']] <- (dncs$VHemA1 + dncs$VHemA11 + dncs$VHemA21 + dncs$VHemA31 + dncs$VHemA41 + dncs$VHemA51)/(6 * 5)
dncs[['Friends']] <- (dncs$VHemA2 + dncs$VHemA12 + dncs$VHemA22 + dncs$VHemA32 + dncs$VHemA42 + dncs$VHemA52)/(6 * 5)
dncs[['PresentSelf']] <- (dncs$VHemA3 + dncs$VHemA13 + dncs$VHemA23 + dncs$VHemA33 + dncs$VHemA43 + dncs$VHemA53)/(6 * 5)
dncs[['Parents']] <- (dncs$VHemA4 + dncs$VHemA14 + dncs$VHemA24 + dncs$VHemA34 + dncs$VHemA44 + dncs$VHemA54)/(6 * 5)
dncs[['Siblings']] <- (dncs$VHemA5 + dncs$VHemA15 + dncs$VHemA25 + dncs$VHemA35 + dncs$VHemA45)/(5 * 5)
dncs[['School']] <- (dncs$VHemA6 + dncs$VHemA16 + dncs$VHemA26 + dncs$VHemA36 + dncs$VHemA46 + dncs$VHemA56)/(6 * 5)
dncs[['Peers']] <- (dncs$VHemA7 + dncs$VHemA17 + dncs$VHemA27 + dncs$VHemA37 + dncs$VHemA47 + dncs$VHemA57)/(6 * 5)
dncs[['Teachers']] <- (dncs$VHemA8 + dncs$VHemA18 + dncs$VHemA28 + dncs$VHemA38 + dncs$VHemA48 + dncs$VHemA50)/(6 * 5)
dncs[['FutureSelf']] <- (dncs$VHemA9 + dncs$VHemA19 + dncs$VHemA29 + dncs$VHemA39 + dncs$VHemA49 + dncs$VHemA55)/(6 * 5)
dncs[['Reading']] <- (dncs$VHemA10 + dncs$VHemA20 + dncs$VHemA30 + dncs$VHemA40)/(4 * 5)
dncs <- dncs[,c('Neighborhood', 'Friends', 'PresentSelf', 'Parents', 'Siblings', 'School', 'Peers', 'Teachers', 'FutureSelf', 'Reading')]

# Creating a function to determine significant differences in averages
significantc <- function(i){
  s <- sqrt((((nrow(coms) - 1) * (sd(coms[,i]))^2) + ((nrow(dncs) - 1) * (sd(dncs[,i]))^2))/(nrow(coms) + nrow(dncs) - 2))
  se <- s * sqrt((1/nrow(coms)) + (1/nrow(dncs)))
  t <- round((mean(coms[,i]) - mean(dncs[,i]))/se, digits = 3)
  p <- round(2*pt(-abs(t), df = nrow(coms) + nrow(dncs) - 2), digits = 3)
  sig <- if(p < 0.05){
    'Yes'
  } else{
    'No'
  }
  print(paste0(sig, ", ", t, ", ", p))
}

# Checking signficance between Hemingway catagories
# catagories <- c('Neighborhood', 'Friends', 'PresentSelf', 'Parents', 'Siblings', 'School', 'Peers', 'Teachers', 'FutureSelf', 'Reading')
# Signficant?, t value, p value
significantc('Neighborhood')
# "No, 0.014, 0.989"
significantc('Friends')
# "Yes, 4.139, 0"
significantc('PresentSelf')
# "Yes, 2.344, 0.02"
significantc('Parents')
# "Yes, 2.571, 0.011"
significantc('Siblings')
# "No, 1.956, 0.052"
significantc('School')
# "Yes, 5.422, 0"
significantc('Peers')
# "Yes, 2.096, 0.037"
significantc('Teachers')
# "Yes, 4.782, 0"
significantc('FutureSelf')
# "Yes, 4.778, 0"
significantc('Reading')
# "Yes, 5.211, 0"
  
# Correlation between Questions
num.cols <- sapply(df2, is.numeric)
cor.data <- cor(df2[,num.cols])
cor.data
corrplot(cor.data, method = 'color')
