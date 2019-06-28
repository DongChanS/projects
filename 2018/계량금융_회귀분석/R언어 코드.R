library(tidyverse)
train = read.csv('Q1_FEMALE_DIRECTOR.csv')

# pipeline
train %>% head(5)

# data transformation
train['LOGCOMP'] = log(train['REALCEOCOMP'])

# data removing
train[c('GVKEY','COMPNAME','CEOGENDER')] = NULL

# data sorting (descending)
# attach(train)
train_sort_BM <- train[order(train['BM']),]
train_sort_BM %>% head(5)

# quotient and remainder
train['SIC2DIG'] <- train['SIC2D'] %/% 10
train['SIC1DIG'] <- train['SIC2D'] %% 10

# factorization
train$SIC2DIG <- as.factor(train$SIC2DIG)
train$SIC1DIG <- as.factor(train$SIC1DIG)

# factor to dummy variable
# install.packages('dummies')
library(dummies)
train <- cbind(train, dummy(train$SIC2DIG,sep = "_2_"))
train <- cbind(train, dummy(train$SIC1DIG,sep = "_1_"))

# data removing
train[c('SIC2DIG','SIC1DIG','SIC2D','REALCEOCOMP',
        'train_1_1','train_2_0')] <- NULL

# interaction term
train['FEMALE_RET1Y'] <- train$FEMALEID * train$RET1Y
train['FEMALE_ROA'] <- train$FEMALEID * train$ROA

# make model
model <- lm(formula = LOGCOMP ~ ., data = train)
model %>% summary()

# summary variables
train %>% summary()

# BOARDSIZE에 대해서 FEMALEID의 효과가 다르지 않을까?
train_bu = train %>% filter(train$BOARDSIZE >= 10)
train_bl = train %>% filter(train$BOARDSIZE < 10 )

# regression

result_bu <- lm(formula = LOGCOMP ~ .-CEOAGE-BOARDSIZE-train_2_8, data = train_bu)

result_bl <- lm(formula = LOGCOMP ~ .-CEOAGE-BOARDSIZE-train_2_8, data = train_bl) 

# breush pegan test
# install.packages("lmtest")
library(lmtest)

# bptest
bptest(formula = LOGCOMP ~ .-CEOAGE-BOARDSIZE-train_2_8,data = train_bu)
# p-value 0.04212 - 등분산성 확보!
bptest(formula = LOGCOMP ~ .-CEOAGE-BOARDSIZE-train_2_8,data = train_bl,studentize = T)
# p-value 0.7806 - 등분산성 확보안됨 : white robuse ste


# heteroskedasticity robust standarized errors
HetCov <- vcovHC(result_bl,type="HC1")

print(coeftest(result_bl,HetCov),digits=1)


#restriction regression test
#install.packages('car')
library(car)

linearHypothesis(result_bu,c("FEMALEID = 0","FEMALE_RET1Y = 0","FEMALE_ROA = 0"),test="F")
# p-value 0.01587
linearHypothesis(result_bl,c("FEMALEID = 0","FEMALE_RET1Y = 0","FEMALE_ROA = 0"),vcov = HetCov)
# p-value 0.06264

# correlation matrix

vector = c('BOARDSIZE','DUALITY','BUSYBOARD','FEMALEID','IDPC','CEOTENURE','BM','SIZE','MKTLEV','FCF','SALESGROWTH','RET1Y','ROA')

cor_u = cor(train_bu[vector])

cor_l = cor(train_bl[vector])
# FEMALEID - BOARDSIZE 0.45, CEOTENURE - CEOAGE 0.34, BOARDSIZE - SIZE 0.61
# FEMALEID - SIZE 0.37, MKTLEC - SIZE 0.3, ROA - FCF 0.69 

#install.packages('corrplot')
library(corrplot)

corrplot(cor_u,type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.5, tl.col = 'black',
         order = "hclust", diag = FALSE,addrect = 15)

corrplot(cor_l,type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.5, tl.col = 'black',
         order = "hclust", diag = FALSE,addrect = 20)





