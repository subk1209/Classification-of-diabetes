library(tidyverse)
library(scales)
library(ROSE)
library(cutpointr)
library(ROCR)
library(glue)
library(rpart)
library(rpart.plot)
library(class)
library(vip)
library(randomForest)
library(caret)
library(caTools)
# remotes::install_github("cran/DMwR")


df_raw <- read_csv("D:/Projects/Self Project/SSS/data/diab.csv")
df_raw %>% head(5)
View(df)


# Checking for NA value:
df_raw %>% is.na() %>% sum()

# Data type:
glimpse(df_raw)
glimpse(df)


# Changing the nature of diabetes column:
#  (since 'pre-diabetics' are very few in number)
# 0: No Diabetes | 1: Diabetes or pre-diabetes.
df_raw$Diabetes_012[df_raw$Diabetes_012 == 2] <- 1


# Classification of variables:
nm <- names(df_raw) 
catnm <- nm[nm != 'BMI']
df_raw %>% mutate_at(catnm, as.factor) -> df


  # Target variable:
df %>% count(Diabetes_012) %>% 
  ggplot(aes(x = Diabetes_012, y = n)) +
  geom_bar(stat = 'identity', width = 0.3,
           colour = 'black', fill = 'yellow') +
  labs(title = 'Frequency distribution of diabetic patients',
       x = 'Diabetes status', y = 'Count')


# Plots of categorical variables:
catnm2 <- catnm[catnm != "Diabetes_012"]
par(mar = c(rep(2.5,4)))
par(mfrow = c(4,5))
for(i in catnm2){
  table(df[i]) %>% plot(main = paste(i),
                               xlab = '', ylab = '',
                               col = 'red')
}



# BMI ~ Categorical(particular)
catnm3 <- c('Sex', 'Age', 'Smoker', 'Stroke', 'HighChol',
            'HighBP', 'Veggies', 'Fruits', 'MentHlth', 'PhysHlth')

    # Use any one or both of the following functions:

f1 <- function(var){       # density plot
  df %>% ggplot(aes(BMI, fill = {{var}})) + 
    geom_density(alpha = 0.5, colour = NA)
}

f2 <- function(var){       # violin plot
  df %>% ggplot(aes(x = {{var}}, y = BMI)) +
    geom_violin(draw_quantiles = c(0.25,0.5,0.75),
                fill = 'red', alpha = 0.4,
                linewidth = 1) +
    scale_y_continuous(n.breaks = 10) + theme_bw()
}

attach(df)
f1(Sex)
f2(Sex)
detach(df)




# Removing outliers from 'BMI':
out <- boxplot(df$BMI, plot = F)$out
glue::glue('Proportion of outliers in BMI: {o}', 
           o = round(length(out)/nrow(df),3))

df_outfree <- df %>% filter(!(df$BMI %in% out))

# To work without outliers, proceed with 'df_outfree'.


# Income ~ Some particular categorical:
f3 <- function(var1, var2, option){
  
  if(option == 'prop'){
    df %>% group_by({{var1}}, {{var2}}) %>% 
      count() %>% group_by({{var1}}) %>%
      mutate(percentage = n/sum(n)) %>% 
      ggplot(aes(x = {{var1}}, y = percentage, fill = {{var2}})) +
      geom_bar(stat = 'identity', position = position_dodge2()) +
      scale_y_continuous(labels = percent) +
      theme_light()
  }else if(option == 'count'){
    df %>% group_by({{var1}}, {{var2}}) %>% 
      count() %>% ggplot(aes(x = {{var1}}, 
                             y = n, fill = {{var2}})) +
      geom_bar(stat = 'identity', position = position_dodge2()) +
      scale_y_continuous(n.breaks = 10) + theme_light()
  }
}

attach(df)
f3(Income, GenHlth, option = 'prop')
f3(Income, GenHlth, option = 'count')
detach(df)


# Categorical with diabetes:
f3(Fruits, Diabetes_012, option = 'prop') -> p1
f3(Fruits, Diabetes_012, option = 'count') -> p2
gridExtra::grid.arrange(p1,p2,ncol = 2)
#===============================================


# Heart Disease ~ Smoking status:
f3(HeartDiseaseorAttack, Smoker, option = 'prop')




# Association between target and features:
M <- data.frame(matrix(ncol = 3, nrow = 0))

for(i in catnm2){
  df %>% select('Diabetes_012', all_of(i)) %>% 
    table() %>% chisq.test() -> ch
  M %>% rbind(c(paste('Diabetes -', i),
                ch$statistic %>% round(3), 
                ch$p.value)) -> M
} %>% suppressWarnings()

colnames(M) <- c('Pairs','Statistic','p-value')
print(M)


#===================================================================
# Chi-Sq function for association: (lagbe na)
chi_func <- function(var1, var2){
  
  len <- function(x) 
    (x %>% unique() %>% length() %>% return())
  
  k <- len(var1); l <- len(var2)
  t <- table(var1, var2)
  
  # important quantities:
  r_tot <- t %>% apply(1, sum) %>% as.numeric()
  c_tot <- t %>% apply(2, sum) %>% as.numeric()
  n <- sum(t)
  
  
  Chi_sq <- 0
  for(i in 1:k)
    for(j in 1:l){
      s = ((t[i,j] - r_tot[i]*c_tot[j]/n)^2)/
        (r_tot[i]*c_tot[j]/n)
      Chi_sq = Chi_sq + s
    }
  
  data.frame("ChiSq" = Chi_sq,
             'p-value' = pchisq(Chi_sq, (k-1)*(l-1), 
                                lower.tail = F)) %>% return()
}
#===================================================================
# Do this before model building:
df_u <- ovun.sample(Diabetes_012~., data = df,
                    p = 0.5, method = 'under', seed = 1)$data %>% 
  as_tibble()

nrow(df_u)


# Before under sampling:
df %>% count(Diabetes_012) %>% 
  mutate("percentage" = percent(n/sum(n)))
# After under sampling:
df_u %>% count(Diabetes_012) %>% 
  mutate("percentage" = percent(n/sum(n)))


########################
# Function for PRECISION & RECALL:
stats2 <- function(C, model){      # C := Confusion Matrix and Statistics
  t <- C$table
  
  acc <- C$overall[[1]]
  pre <- t[2,2]/(t[2,2]+t[2,1])
  rec <- t[2,2]/(t[2,2]+t[1,2])
  f1 <- 2*(rec*pre)/(rec+pre)
  
  matrix(c(acc,pre,rec,f1), byrow = T,
         dimnames = list(c('Accuracy','Precision',
                           'Recall','F1-Score'),
                         paste(model))) -> M
  return(list('Confusion Matrix' = t,
              'Metrics' = M))
}
########################


##############################################################
## Fitting of models: 1 - LOGISTIC                        ||||
##############################################################
  ## Selection of good model:
link <- c('probit','logit','cauchit')
M <- matrix(ncol = 2, nrow = 3,
            dimnames = list(link, c("AIC","Deviance")))

for(i in 1:3){
  glm(Diabetes_012 ~ ., data = df_u,
      family = binomial(link = link[i])) %>% 
    summary() -> s
  M[i,] <- c(s$aic, s$deviance)
}

# "logit" seems good.

    #### Fitting (INITIAL) :
glm(Diabetes_012 ~ ., data = df_u,
    family = binomial(link = 'logit')) %>% 
  summary() -> s1

print(s1)

    # Observations:
# we can see that many of the predictors are not statistically
# significant. we will remove them & re-fit the model again.
# Also, among those which are significant, many of them 
# have too many levels, which can make the model more complex
# or difficult to work with, so we will reduce the number of
# levels of the statistically significant factor variables.

insig_vars <- c('Fruits','Veggies','MentHlth','PhysHlth',
                'AnyHealthcare','Education') # to be removed from the model
df_u1 <- df_u %>% select(!all_of(insig_vars))

  # Re-formatting GenHlth:
u1 <- df_u %>% pull(GenHlth) %>% unique(); u1
df_u1 %>% mutate(GenHlth = case_when(
  GenHlth %in% 1:3 ~ "<= 3",
  GenHlth %in% 4:5 ~ "> 3"
)) -> df_u1

  # Re-formatting Age:
u2 <- df_u %>% pull(Age) %>% unique(); u2
df_u1 %>% mutate(Age = case_when(
  Age %in% 1:4 ~ "< 5",
  Age %in% 5:9 ~ "5-9",
  Age %in% 10:13 ~ "> 9"
)) -> df_u1

  # Re-formatting Age:
u3 <- df_u %>% pull(Income) %>% unique(); u3
df_u1 %>% mutate(Income = case_when(
  Income %in% 1:4 ~ "Low",
  Income %in% 5:8 ~ "High"
)) -> df_u1



## Train - Test SPLIT:
n <- nrow(df_u1); set.seed(42)
rs <- sample(1:n, size = 0.75*n, replace = F)
train_data <- df_u1[rs,]
test_data <- df_u1[-rs,]


    #### Fitting (FINAL) ::
glm(Diabetes_012 ~ ., data = train_data,
    family = binomial(link = 'probit')) -> g

g %>% summary() %>% print()  # seems good!



   ## Optimum cut-off selection:
metric_func <- function(data, phat){  # function to store the 
  cut_points <- seq(0.01,0.99,0.01)    # necessary metrics
  
  d <- data.frame(matrix(nrow = length(cut_points),
                         ncol = 4, dimnames = list(
                           paste(1:length(cut_points)),
                           c('p_opt','Accuracy',
                             'Sensitivity','Specificity')
                         )))
  
  for(i in 1:length(cut_points)){
    C <- confusionMatrix(
      if_else(phat >= cut_points[i], 1, 0) %>% as.factor(),
                         data$Diabetes_012)
    
    d[i,] <- c(cut_points[i], C$overall[[1]],
               C$byClass[[1]],C$byClass[[2]])
  }
  
  d$sens_spec <- d[,3]*d[,4]
  return(d)
}

phat_train <- g %>% predict.glm(type = 'response')

m_train <- metric_func(train_data,phat_train) 

p1_opt <- m_train[which.max((m_train$Accuracy)),]$p_opt
p2_opt <- m_train[which.max((m_train$sens_spec)),]$p_opt

glue('Optimum cut point (Accuracy): {c1}',
     'Optimum cut point (Sens*Spec): {c2}',
     .sep = '\n', c1 = p1_opt, c2 = p2_opt)

# Considering p1_opt.




    ### On test data:
phat_test <- predict.glm(g, newdata = test_data,
                      type = 'response')

confusionMatrix(ifelse(phat_test >= p1_opt, 1, 0) %>% 
                  as.factor(), test_data$Diabetes_012) -> C

stats2(C,'Logistic')



    ### ROC curves:
m_test <- metric_func(test_data, phat_test)

ROC_func <- function(m, type){
  plot(1 - m$Specificity, m$Sensitivity, type = 'l',
       main = paste('ROC curve ||',type,'data'), 
       ylab = 'Specificity (TPR)',
       xlab = '1-Sensitivity (FPR)', lwd = 2, las = 1)
  abline(a = 0, b = 1, h = 0:1, v = 0:1, lty = 2)
}

par(mfrow = c(1,2))
ROC_func(m_train, 'Train')
ROC_func(m_test, 'Test')


##############################################################
## Fitting of models: 2 - DECISION TREE                   ||||
##############################################################

  # Splitting the data:
n <- nrow(df_u)
rs <- sample(1:nrow(df_u), size = 0.75*n, replace = F)
train_data <- df_u[rs,]
test_data <- df_u[-rs,]

  # fitting of model:
dtree <- rpart(Diabetes_012 ~ ., data = train_data)
rpart.plot(dtree, extra = 4)  # visualize the tree


# Finding out the important variables in the model:
imp <- vip(dtree)
print(imp)


# Picking up the important variables :
imp_vars <- c('HighBP','GenHlth','Age','BMI','HighChol','Income',
              'PhysHlth','DiffWalk','PhysActivity')
train_data <- train_data %>% select(Diabetes_012,
                                    all_of(imp_vars))
test_data <- test_data %>% select(Diabetes_012,
                                  all_of(imp_vars))

  # To overcome the problem of over-fitting,
  # we will use k-fold cross-validation:
folds <- createMultiFolds(train_data$Diabetes_012, k = 5, times = 5)
control <- trainControl(method = "repeatedcv", index = folds)
classifier_cv <- train(Diabetes_012 ~ ., data = train_data, 
                       method = "rpart", trControl = control)

classifier_cv$finalModel -> dtree_f # final model
rpart.plot(dtree_f, extra = 4) # visualizing tree


  # Accuracy on test data:
diab_pred <- predict(classifier_cv, newdata = test_data)
confusionMatrix(test_data$Diabetes_012,diab_pred) -> C
stats(C, 'Decision Tree')  # Line 200

##############################################################
## Fitting of models: 3 - RANDOM FOREST                   ||||
##############################################################
s <- sample.split(df_u$Diabetes_012, SplitRatio = 0.75)
train_data <- subset(df_u, s == TRUE)
test_data <- subset(df_u, s == FALSE)

rf_initial <- randomForest(Diabetes_012 ~ ., data = train_data,
                   ntree = 300) 

    # Plot of errors:
plot(rf_initial, lwd = 2, lty = 1)
grid(lty = 3, col = 'black')

# The overall error(black) converges to a fix value for 
# 150 trees onward. So, 150 would be a good choice.

rf <- randomForest(Diabetes_012 ~ ., data = train_data,
                     ntree = 150) 


# Prediction:
diab_pred <- predict(rf, newdata = test_data,
                     type = 'class')


# Model metrics:
confusionMatrix(test_data$Diabetes_012, diab_pred) -> C
stats2(C, 'Random Forest')  # Line 200



##############################################################
## Fitting of models: 3 - knn                             ||||
##############################################################
# function to scale data: (min-max scale)
scale2 <- function(x) ((x-min(x))/(max(x)-min(x)))

df_u2 <- ovun.sample(Diabetes_012~., data = df_raw,
                    p = 0.5, method = 'under', seed = 1)$data %>% 
  as_tibble() %>% select(Diabetes_012, all_of(imp_vars)) %>% 
  mutate_all(~ scale2(.))

# data under consideration: df_u2

    # Splitting:
s <- sample.split(df_u2$Diabetes_012, SplitRatio = 0.75)
train_data <- subset(df_u2, s == TRUE)
test_data <- subset(df_u2, s == FALSE)


  # Choosing optimal value of "k" :
error <- array(0)
k <- seq(1, 49, by = 2)

for(i in 1:length(k)){
  knn_model <- knn(train_data[,-1], test_data[,-1], 
                   cl = train_data$Diabetes_012, k = k[i])
  error[i] <- mean(test_data$Diabetes_012 != knn_model)
}

  # Error plot:
data.frame('k' = k, 'ErrorRate' = error) %>% 
  ggplot(aes(x = k, y = ErrorRate)) + 
  geom_line(colour = 'red') + geom_point() +
  scale_y_continuous(labels = percent, n.breaks = 10) +
  scale_x_continuous(n.breaks = length(error)) +
  theme_minimal()

k_opt <- 25

  # Final model:
knn_model <- knn(train_data[,-1], test_data[,-1], 
                 cl = train_data$Diabetes_012, k = k_opt)


stats(confusionMatrix(knn_model,as.factor(test_data$Diabetes_012)),
      model = 'Knn')  # line 200

# (logic tar jonyo code ta pore kore debo)
#====================================================================



