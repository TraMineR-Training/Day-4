library(TraMineR)
#help(biofam)
data(biofam)
#fix(biofam)

#CREATE STATE SEQUENCE OBJECT
biofam.shortlab <- c("P", "L", "M", "L+M", "C", "L+C", "L+M+C", "D")
biofam.lab <- c("Parent", "Left", "Married", "Left+Marr","Child", "Left+Child", "Left+Marr+Child", "Divorced")
biofam.coln <-c("15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30")
biofam.seq <- seqdef(biofam, 10:25, states=biofam.shortlab, labels=biofam.lab, xtlab=biofam.coln)

#1. BUILDING TABLE WITH SEQUENCE LENGTH, NO. OF TRANSITIONS, NO. OF SUBSEQUENCES,  
#LONGITUDINAL ENTROPY, TURBULENCE, AND COMPLEXITY INDEX
tab <- data.frame(seqlength(biofam.seq), seqtransn(biofam.seq),
seqsubsn(biofam.seq), seqient(biofam.seq), seqST(biofam.seq), seqici(biofam.seq))
tab[1:5, ]

#2. DESCRIPTIVES OF LONGITUDINAL CHARACTERISTICS
summary(tab)

#3. HISTOGRAM OF NO. OF TRANSITIONS, NO. OF SUBSEQUENCES, LONGITUDINAL ENTROPY, TURBULENCE, AND COMPLEXITY INDEX
par(mfrow = c(3, 2))
hist(tab$Trans., col="red", main="No. of transitions")
hist(tab$Subseq., col="orange", main="No. of subsequences")
hist(tab$Entropy, col="lightblue", main="Entropy")
hist(tab$Turbulence, col="blue", main="Turbulence")
hist(tab$C, col="darkblue", main="Complexity")

#4. GENERATE SEQUENCE OF DISTINCT SUCESSIVE STATES (DSS) & TABLE WITH DURATION IN DISTINCT SUCCESSIVE STATES
dss <- seqdss(biofam.seq)
dss[1994:2000, ]
duration <- seqdur(biofam.seq)
duration[1994:2000, ]

#5. MEAN AND VARIANCE OF TIME SPENT IN SUCCESSIVE STATES
mt <- apply(duration, 1, mean, na.rm=T)
summary(mt)
variation <- apply(duration, 1, mean, na.rm=T)
summary(variation)

#6. SCATTERPLOT MATRIX COMPARING ENTROPY WITH TURBULENCE AND COMPLEXITY INDEX
plot(tab[,c("Entropy","Turbulence","C")])

#7. COMPARE DISTRIBUTIONS OF COMPLEXITY INDEX BY BIRTH COHORTS USING BOXPLOTS
#GENERATE COHORT VARIABLE
biofam$cohort <- cut(biofam$birthyr, c(1900,1930,1940,1950,1960), labels=c("1900-1929", "1930-1939", "1940-1949", "1950-1959"), right=FALSE)
#GENERATE BOXPLOTS
boxplot(tab$C ~ biofam$cohort, col = "red", main = "Complexity", xlab = "Cohorts")

#8. REGRESS COMPLEXITY INDEX ON BIRTH COHORT, SEX, LANGUAGE OF QUESTIONNAIRE
lm.complexity <- lm(tab$C ~ cohort + sex + plingu02, data = biofam)
summary(lm.complexity)
# All covariates, except for filling out a German language questionnaire is significantly related to the complexity index.
# Being a member of the two younger cohorts is positively related to sequence complexity, in other words the findings indicate
# that the oldest cohort seemed to have more stable trajectories with regard to marriage and childbearing patterns.
# Women tend to have more complex trajectories than men.