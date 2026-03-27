
library(ggplot2)

library(tidyverse)


#Highlight the important stuff - the two plots overlayed show the difference for M/F

set.seed(1234)
df<-data.frame(
  sex = factor(rep(c("F","M"),each= 200)),
  weight = round(c(rnorm(200, mean = 55, sd = 5), rnorm(200, mean = 65, sd = 5)))
)

#use semi-transparent fill
p<-ggplot(df,aes(x=weight, fill = sex, color = sex))+
  geom_histogram(position = "identity",alpha = 0.5)
p
p+scale_color_manual(values=c("#999999","#E69F00","#56B4E9"))+
  scale_fill_manual(values = c("#999999","#E69F00","#56B4E9"))
p




#Visual Hierarchy

#histogram with density plot shows how transparency and color can focus the eye

set.seed(1234)
df<-data.frame(
  sex = factor(rep(c("F","M"), each = 200)),
  weight = round(c(rnorm(200, mean = 55, sd = 5), rnorm(200, mean = 65, sd = 5)))
)

#histogram with density plot
ggplot(df, aes(x=weight))+
  geom_histogram(aes(y=after_stat(density)), colour = "black", fill = "white")+
  geom_density(alpha=.2, fill = "#FF6666")



#Accessibility
#Based on the data below - create the pie chart which can be read by color-blind
#viewers.

#Perce  nt
#Bored 12%
#Not Great 6%
#OK 14%
#Kind of Interested 30%
#Excited 36%


data <-data.frame("category" = c("Bored","Not Great","OK","Kind of Interested","Excited"),
                  "amount" = c(12, 6, 14, 30, 36))


ggplot(data, aes(x="", y=amount, fill = category))+
  geom_bar(stat= "identity", width = 1)+
  coord_polar("y", start = 0)+
  geom_text(aes(label = paste0(amount,"%")), position= position_stack(vjust=0.5))+
  labs(x = NULL, y= NULL, fill = NULL)

#Aesthetics


ggplot(data, aes(x=category, y=amount))+
  geom_bar(stat= "identity", position =  "stack") 
 
 # coord_("y", start = 0)+
#  geom_text(aes(label = paste0(amount,"%")), position= position_stack(vjust=0.5))+
#  labs(x = NULL, y= NULL, fill = NULL)

#Histogram

set.seed(1234)
df <- data.frame(
  sex=factor(rep(c("F","M"), each=200)),
  weight = round(c(rnorm(200, mean=55, sd=5), rnorm(200, mean=65, sd=5)))
)

head(df)
#sex weight
#1   F     49
#2   F     56
#3   F     60
#4   F     43
#5   F     57
#6   F     58

tail(df)
#sex weight
#395   M     68
#396   M     69
#397   M     67
#398   M     68
#399   M     65
#400   M     60

p<-ggplot(df,aes(x=weight))+
  geom_histogram(color = "black", fill = "white", binwidth = 2)
p


#Heatmap
library(reshape2)
data()
ls("package:datasets")
data(package = .packages(all.available = TRUE))



df<-read.csv(file.choose(),header=T) #read.csv("bestsellers.csv")
data<-cor(df[sapply(df,is.numeric)])
data1<-melt(data)

head(data1)
#Var1  Var2        value
#1 User.Rating User.Rating  1.0000000
#2     Reviews User.Rating -0.2513922
#3       Price User.Rating -0.1027238
#4        Year User.Rating  0.2124160
#5 User.Rating     Reviews -0.2513922
#6     Reviews     Reviews  1.0000000


#correlation heat map
ggplot(data1,aes(x=Var1, y=Var2, fill=value))+
  geom_tile()+scale_fill_gradient(high="red", low="white")+
  geom_text(data=data1, aes(label=round(value,2)))+
  labs(title="Correlation Heatmap", 
       x="Variable 1", 
       y="Variable 2")
#.01 - .3 considered low
#.30-.65 considered medium 
#.65 - 1 considered high



#Pareto Chart
df<-data.frame(favorite=c('A','B','C','D','E','F'),
               count = c(140, 97, 58, 32, 17, 6))

df

#favorite count
#1        A   140
#2        B    97
#3        C    58
#4        D    32
#5        E    17
#6        F     6



#step 2: create the pareto chart
#to create a pareto chart to visualize the results of the survey, we use parento.chart()

library(qcc)

#create pareto chart - used for quality control/defect analysis
pareto.chart(df$count)  #default chart - includes 2 Y-axis
#Pareto chart analysis for df$count
#Frequency  Cum.Freq. Percentage Cum.Percent.
#A 140.000000 140.000000  40.000000    40.000000
#B  97.000000 237.000000  27.714286    67.714286
#C  58.000000 295.000000  16.571429    84.285714
#D  32.000000 327.000000   9.142857    93.428571
#E  17.000000 344.000000   4.857143    98.285714
#F   6.000000 350.000000   1.714286   100.000000



pareto.chart(df$count,
             main='Pareto Chart for Favorite Cereal Brands',
             col = heat.colors(length(df$count)))



#Annotated Graph
data <-read.csv(file.choose(),header=T) #read.csv("MagicData.csv)
data <-read.csv("MagicData.csv")

data
#company   x   y
#1               NEC 4.3 3.0
#2     Tech Mahindra 4.0 3.8
#3    DXC Technology 2.2 3.8
#4           Gerpact 5.2 4.0
#5               EXL 6.0 3.0
#6           NTTDATA 4.0 5.8
#7  HCL Technologies 3.0 5.8
#8             Wipro 4.0 6.8
#9              Atos 4.5 6.8
#10          Infosys 5.1 7.2
#11        Cognizant 5.5 6.5
#12        Capgemini 6.5 7.0
#13              TCS 6.5 7.5
#14              IBM 7.5 7.0
#15              PwC 5.5 7.5
#16             KPMG 7.1 7.5
#17        Accenture 7.2 8.0
#18               EY 8.5 8.0
#19         Deloitte 7.7 8.5


str(data)
#'data.frame':	19 obs. of  3 variables:
#$ company: chr  "NEC" "Tech Mahindra" "DXC Technology" "Gerpact" ...
#$ x      : num  4.3 4 2.2 5.2 6 4 3 4 4.5 5.1 ...
#$ y      : num  3 3.8 3.8 4 3 5.8 5.8 6.8 6.8 7.2 ...


#Magic quadrant from gardner group
ggplot(data,aes(x=x, y=y, xmin=0.5, ymin=0.5, xmax=9.0, ymax=9.0)) +
  geom_point(color="blue", size = 3) +
  geom_text(aes(label=company, size=2), nudge_x=0.25, nudge_y = -.20)+
  geom_hline(yintercept = 5)+
  geom_vline(xintercept = 5)+
  annotate("text", x=2.5, y=0.5, label = "NICHE PLAYERS", color="black")+
  annotate("text", x=7.5, y=0.5, label = "VISIONARIES", color="black")+
  annotate("text", x=2.5, y=9.5, label = "CHALLENGERS", color="black")+
  annotate("text", x=7.5, y=9.5, label = "LEADERS", color="black")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")
