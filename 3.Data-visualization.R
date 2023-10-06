# Importing the data set
df <- read.csv("clean_kidney_disease.csv", stringsAsFactors = FALSE, header = TRUE)
# Data exploration
str(df)
summary(df)
View(df)

# Modifining the data set
# After observing the data set, we can see that the data
# set has 400 rows and 26 columns.
# But we see the column name is not understandable.
# for that reason we are going to change the column name
# i found the column name from the data set description
# in web site its more understandable

colnames(df) <- c('Patient ID','Age', 'Blood Pressure', 'Specific Gravity',
                  'Albumin', 'Sugar', 'Red Blood Cells', 'Pus Cell', 'Pus Cell Clumps',
                  'Bacteria', 'Blood Glucose Random', 'Blood Urea', 'Serum Creatinine',
                  'Sodium', 'Potassium', 'Haemoglobin', 'Packed Cell Volume', 'White Blood Cell
Count', 'Red Blood Cell Count', 'Hypertension', 'Diabetes Mellitus',
                  'Coronary Artery Disease', 'Appetite', 'Peda Edema', 'Aanemia', 'Class')

# check the data set
str(df)
View(df)

# Checking the missing value
df[is.na(df)]
# Checking the missing value in each column
colSums(is.na(df))

# i am going to use the data set for visualization
n_df <- df[,c(2,3,5,6,12,14,23,26)]
# check the data set
str(n_df)   
View(n_df)

library(ggplot2)
# This	plot	gives	an	overview	of	all	397	sample	in	the	study.	With	different	colors	I	show	how	
# many	 patients	 are	 with	 chronic	 kidney	 disease	 and	 how	 many	 are	 without.	 The	 x	 axis	
# divides	the	patients	into	different	ages.	I	am	showing	2	variables	in	this	plot:	Age and	Class.	
# I	use	geom_bar() to	plot	here.
# Age Compared with patient condition

#### 1 Visualizations
ggplot(data = n_df) +
  geom_bar(mapping = aes(x = Age, fill = Class), alpha = 0.75 ) +
  labs(title = "Sample Overview: Chronic Kidney Disease Study",
       subtitle = "Patient Age vs Patient Conditions",
       caption = "Sample including 397 patients",
       fill = "Patient Conditions") +
  ylab(label = "Sample Count") +
  xlab(label = "Patient Age") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# now we will see the same plot in histogram
# histogram shows the same result as bar plot
# histogram shows the patient condition

# 2 Visualizations
ggplot(data = n_df) +
  geom_histogram(mapping = aes(x = Age, fill = Class), alpha = 0.75) +
  labs(title = "Sample Overview: Chronic Kidney Disease Study",
       subtitle = "Patient Age vs Patient Conditions",
       caption = "Sample including 397 patients",
       fill = "Patient Conditions") +
  ylab(label = "Sample Count") +
  xlab(label = "Patient Age") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))



######
# The	second	plot	shows	the	relationship	between	patients'	sugar	levels	and	whether	or	not	
# they	have	chronic	kidney	disease.	The	x	axis	divides	 the	patients	into	different	ages.	 I	am	
# showing	3	variables	in	this	plot:	Sugar,	Age and	Class.
# Sugar level Relations with CKD

# 3 Visualization
ggplot(data = n_df) +
  geom_jitter(mapping = aes(x = Age, y = Sugar, color = Class, size
                            = popdensity), alpha = 0.4, size = 3) +
  labs(title = "Chronic Kidney Disease vs Sugar Level",
       subtitle = "Sugar Level vs Patient Conditions ",
       color = "Patient Conditions") +
  ylab(label = "Sugar Level") +
  xlab(label = "Patient Age") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.line = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))



###
# The	 first	plot	shows	 the	relationship	between	patients' sodium	levels	and	whether	or	not	
# they	have	chronic	kidney	disease.	The	x	axis	divides	 the	patients	into	different	ages.	 I	am	
# showing	3	variables	in	this	plot:	Sodium,	Age and	Class.
# CKD relation with sodium level

# 4 Visualization
ggplot(data = n_df) +
  geom_jitter(mapping = aes(x = Age, y = Sodium, color = Class, size
                            = popdensity), alpha = 0.4, size = 3) +
  labs(title = "Chronic Kidney Disease vs Sodium Level",
       subtitle = "Sodium Level (mEq/L) vs Patient Conditions ",
       color = "Patient Conditions") +
  ylab(label = "Sodium Level (mEq/L)") +
  xlab(label = "Patient Age") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.line = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# The	third	plot	shows	the	relationship	between	patients'	albumin	levels	and	whether	or	not	
# they	have	chronic	kidney	disease.	The	x	axis	divides	 the	patients	into	different	ages.	 I	am	
# showing	3	variables	in	this	plot:	Albumin,	Age and	Class
# Albumin Level relation with CKD

# 5 Visualization
ggplot(data = n_df) +
  geom_jitter(mapping = aes(x = Age, y = Albumin, color = Class,
                            size = popdensity), alpha = 0.4, size = 3) +
  labs(title = "Chronic Kidney Disease vs Albumin Level",
       subtitle = "Albumin Level vs Patient Conditions ",
       color = "Patient Conditions") +
  ylab(label = "Albumin Level") +
  xlab(label = "Patient Age") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.line = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))


