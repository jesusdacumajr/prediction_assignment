# Download Test File
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
              destfile = "pml-testing.csv")

# Load Data
testdata <- read.csv("pml-testing.csv", header = TRUE)

# Clean Data
blankOrNA <- c(12:36, 50:59, 69:83, 87:101, 103:112, 125:139, 141:150)

testClean <- testdata[, -blankOrNA] # Remove columns with blank or NA values
testClean <- testClean[, -c(1:7)] # Remove columns with metadata

# Use trained model to predict test data
testpred <- predict(SVMfit, newdata = testClean)

# Write predictions to text files
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("submissions/problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(testpred)