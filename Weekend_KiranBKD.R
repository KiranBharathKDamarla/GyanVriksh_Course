
data_table <- read.csv("C:/Users/kiranbharathkumar.da/Desktop/DataScience/DataSci/Assignment/MBA Starting Salaries Data.csv")
attach(data_table)
Age_col = data_table$age
mean(age)
hist(age)

#Elimination of the rows of missing Salary data
data_table$salary[data_table$salary == 999] <- NA
data_table$salary[data_table$salary == 998] <- NA
new<-na.omit(data_table)
new
new_col=new$age

#1 Histogram of Age
hist(new_col)#the distribution is positively skewed distribution or it is said to be skewed to the right.

#2 Gmat Total
gmat_col=new$gmat_tot
mean(gmat_col)
median(gmat_col)
mode(gmat_col)

mode <- function(x) 
{
	ux <- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
}
x <- gmat_col
mode.gmat_total = mode(x)
print(mode.gmat_total)

#3 Draw the Hist & FD of Salary

new_sal=new$salary
hist(new_sal)
barplot(table(new_sal))

#4 No "Higest salary drawn by 'satis' (6) student
summary(new_sal)
max(new_sal)

new[which.max(new_sal),]

new[which(new_sal == max(new_sal)), ]





