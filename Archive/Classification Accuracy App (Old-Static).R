library(ggplot2)
library(caret)
library(e1071)
library(formattable)
windowsFonts(Times=windowsFont("TT Times New Roman"))

#Static input values. ----

meanx1=0  #mean of the predicted distribution.
sdx1=1    #SD of the predicted distribution.
meany1=0  #mean of the actual distribution.
sdy1=1    #SD of the actual distribution.
n=1000    #sample size for both distribution.
r=.55     #correlation between predictors.
pcut = 0  #predicted cut score, positive
acut = 0  #actual cut score, prevalence

x1 = rnorm(n=n, mean=meanx1, sd=sdx1) #Simulated x 
y0 = rnorm(n=n, mean=meany1, sd=sdy1)
y1 = x1*r+y0*(1-r^2)^0.5
Predicted = as.integer(x1>pcut)
Actual = as.integer(y1>acut)
data = data.frame(x1,y1,Predicted,Actual)


#Confusion Matrix ----

#Confusion Matrix
TP = sum(x1>pcut & y1>acut) #A
FN = sum(x1<pcut & y1>acut) #B
TN = sum(x1<pcut & y1<acut) #D
FP = sum(x1>pcut & y1<acut) #C

cm = confusionMatrix(data$Predicted,data$Actual)

Accuracy      = percent(cm$overall[1],digits = 2)
kappa         = round(cm$overall[2], digits = 2)
AccuracyLower = percent(cm$overall[3],digits = 2)
AccuracyUpper = percent(cm$overall[4],digits = 2)
AccuracyNull  = percent(cm$overall[5],digits = 2)
AccuracyPValue= round(cm$overall[6], digits = 2)
McnemarPValue = round(cm$overall[7], digits = 2)

Sensitivity         =percent(cm$byClass[1],digits = 2)
Specificity         =percent(cm$byClass[2],digits = 2)
PPV                 =percent(cm$byClass[3],digits = 2)
NPV                 =percent(cm$byClass[4],digits = 2)
Precision           =percent(cm$byClass[5],digits = 2)
Recall              =percent(cm$byClass[6],digits = 2)
F1                  =round(cm$overall[7], digits = 2)
Prevalence          =percent(cm$byClass[8],digits = 2)
DetectionRate       =percent(cm$byClass[9],digits = 2)
DetectionPrevalence =percent(cm$byClass[10],digits = 2)
BalancedAccuracy    =percent(cm$byClass[11],digits = 2)

# Plot --------------------------------------------------------------------
scatplot = ggplot(data, aes(x=x1, y=y1)) +
  scale_y_continuous(name="Actual", limits = c(-3,3), breaks = seq(-3,3,1), expand = c(0,0)) + 
  scale_x_continuous(name="Predicted", limits = c(-3,3), breaks = seq(-3,3,1), expand = c(0,0)) +
  geom_point(color="#006747", alpha=.7, size=2) +
  
  #TRUE POSITIVES
  annotate("rect", xmin = pcut, xmax = 3, ymin = acut, ymax = 3, fill="darkgreen",alpha = .25, color="darkgreen") +
  annotate("text", x = 2.5, y = 2.5, label = "TP", color="white", size=20, family ="Times", fontface=2) +
  #FALSE NEGATIVES
  annotate("rect", xmin = -3, xmax = pcut, ymin = acut, ymax = 3, fill="red",alpha = .25, color="red") +
  annotate("text", x = -2.5, y = 2.5, label = "FN", color="white", size=20, family ="Times", fontface=2) +
  #TRUE NEGATIVES
  annotate("rect", xmin = -3, xmax = pcut, ymin = -3, ymax = acut, fill="darkgreen",alpha = .25, color="darkgreen") +
  annotate("text", x = -2.5, y = -2.5, label = "TN", color="white", size=20, family ="Times", fontface=2) +
  #FALSE POSITIVES
  annotate("rect", xmin = pcut, xmax = 3, ymin = -3, ymax = acut, fill="red",alpha = .25, color="red") +
  annotate("text", x = 2.5, y = -2.5, label = "FP", color="white", size=20, family ="Times", fontface=2) +
  
  
  theme(text = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white", color = "black"),
        axis.text.y = element_text(color = 'black'),
        axis.text.x = element_text(color = 'black'),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        legend.key = element_rect(fill = NA)
  )
scatplot