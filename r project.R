#read file
df=read.csv("C:\\Users\\tatak\\haroon\\R\\DATASET\\retail_sales_dataset.csv")
df
#check info drop columns
str(df)
summary(df)
df$Transaction.ID=NULL
df$Customer.ID=NULL
df$Date=NULL
df
#check null value
colSums(is.na(df))
#outlier check
numeric_cols=sapply(df, is.numeric)
boxplot(df[, numeric_cols])
# vituallization
hist(df$Total.Amount,main="Distribution of Total Amount",xlab="Total Amount",col = "green")

gender_count=table(df$Gender)
barplot(gender_count,main="Gender Count",xlab="Gender",ylab="Count",col=c("pink","skyblue"))

product_count=table(df$Product.Category)
barplot(product_count,main="Product Category Count",xlab="Category",ylab="Count",col=c("red","orange","yellow"))

library(corrplot)
numeric_data=df[, sapply(df, is.numeric)]
cor_matrix=cor(numeric_data)
corrplot(cor_matrix,
         method = "color",
         addCoef.col = "black",
         number.cex = 0.7)
#Encode categorical data
df$Gender=as.factor(df$Gender)
df$Gender=as.numeric(df$Gender)
df$Product.Category=as.factor(df$Product.Category)
df$Product.Category=as.numeric(df$Product.Category)
head(df)
#train test split
x = df[, names(df) != "Total.Amount",]
y = df$Total.Amount

set.seed(43)
sample_rows=sample(1:nrow(df), 0.8 * nrow(df))
x_train=x[sample_rows,]
x_test=x[-sample_rows,]
y_train=y[sample_rows]
y_test=y[-sample_rows]
#model building
model = lm(y_train ~ ., data = x_train)
summary(model)
#prediction
predictions = predict(model, x_test)
head(predictions)
 
#score
cat("r2_score:", summary(model)$r.squared * 100)

mse = mean((y_test - predictions)^2)
cat("MSE:", mse)

