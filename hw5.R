###Question 1

#The vector sep works as follows. 
#In a given row, it puts a space after each number except the last one.
#For the last one, it starts a new line.

#If instead of "\n", we used "\t", the rows would be separated by a tab instead of a new line.

#The output will be 1 2 3 4 5   6 7 8 9 10.


###Question 2
#takes a data frame and returns it as a list
#the first row of the list is the name of columns from the dataframe.
#the second row is the type of each column.
#the third row is the name of the rows.
#all other rows are the entries from the data frame.
df_to_list=function(x){
  y=list()
  
  #first row is names of the columns
  y[[1]]=as.character(names(x))  
  
  #initialize second row with a bunch of NA's
  y[[2]]=c(rep(NA, length(x))) 
  
  #for loop to fill the second row
  for(j in 1:length(x)){
    y[[2]][j]=as.character(typeof(x[[j]]))     
  }
  
  y[[3]]=rownames(x)
  
  #filling in the other rows  
  for(j in 1:nrow(x)){
    y[[3+j]]=as.numeric(x[j, ]) 
  }
  
  return(y)
}


#dataframe given in question.
df = data.frame(i = 1:10, d = 0.5*1:10, f = as.factor(2*1:10),c = as.character(3*1:10))


#function to take dataframe and write it in a text file in the format of a list.
write.table.list=function(x){
  y=df_to_list(x)
  directory_path = "~/Documents/Rstuff/hw5"
  filename_temp="output.txt"
  description_temp=file.path(directory_path,filename_temp)
  write(y[[1]], file = description_temp, ncolumns = length(x), sep = '\t')
  write(y[[2]], file = description_temp, append = TRUE, ncolumns = length(x), sep = '\t')
  write(y[[3]], file = description_temp, append = TRUE, ncolumns = length(x[ ,1]), sep = '\t')
  for(j in 4:length(y)){
    write(y[[j]], file = description_temp, append = TRUE, sep = '\t')
  }
}


write.table.list(df)



###Question 3
directory_path = "~/Documents/Rstuff/hw5"
filename_1 = "eustocks.txt"
description_1=file.path(directory_path,filename_1)

#store the file eustocks.txt in a vector named b
b=scan(description_1)
b


#creating 4 vectors to store the entries from the vector b
DAX=c()
SMI=c()
CAC=c()
FTSE=c()



for(i in 1:length(b)){
  
  #Filling up the vector DAX with i-th enrty of b where i=1,5,9 etc
  if(i%%4==1){
    DAX[length(DAX)+1]=b[i]
  }
  
  #Filling up the vector SMI with i-th enrty of b where i=2,6,10 etc
  if(i%%4==2){
    SMI[length(SMI)+1]=b[i]
  }
  
  #Filling up the vector CAC with i-th enrty of b where i=3,7,11 etc
  if(i%%4==3){
    CAC[length(CAC)+1]=b[i]
  }
  
  #Filling up the vector FTSE with i-th enrty of b where i=4,8,12 etc
  if(i%%4==0){
    FTSE[length(FTSE)+1]=b[i]
  }
}

#Creating a new dataframe with columns DAX, SMI, CAC and FTSE
eustock=data.frame(DAX, SMI, CAC, FTSE)


#writing the dataframe eustock to a text file named eustock_formatted.txt
directory_path = "~/Documents/Rstuff/hw5"
filename_Q3 = "eustocks_formatted.txt"
description_Q3=file.path(directory_path,filename_Q3)
write.table(eustock, description_Q3)
#The formatting doesn't look good. 
#To make it look better, set row.names=FALSE and sep="\t". See next line
write.table(eustock, description_Q3, row.names = FALSE, sep = "\t")








###Question 4
directory_path = "~/Documents/Rstuff/hw5"
filename_Q4 = "atotc.txt"
description_Q4=file.path(directory_path,filename_Q4)

book=readLines(description_Q4)
str(book)
length(book)

#splitting book[i] for i=1,2,3 to collect all the words
#there will be some empty characters. will get rid of them later
split_1=strsplit(book[1], split = "( )|( [0-9]+ )|(\t)|([0-9]+)")
split_2=strsplit(book[2], split = "( )|( [0-9]+ )|(\t)|([0-9]+)")
split_3=strsplit(book[3], split = "( )|( [0-9]+ )|(\t)|([0-9]+)")


#takes a vector of characters as input and deletes the empty character
delete_empty_string_from_vector=function(character_vector){
  y=c()
  for (i in 1:length(character_vector)){
    if(character_vector[i] != ""){
      y[length(y)+1]=character_vector[i]
    }
  }
  return(y)
}

#deleting empty strings from split_1, split_2 and split_3
#Recall that the output of strsplit is a list. 
#So the indexing is of the form [[1]]
word_1=delete_empty_string_from_vector(split_1[[1]])
word_2=delete_empty_string_from_vector(split_2[[1]])
word_3=delete_empty_string_from_vector(split_3[[1]])


#deleting duplications from word_1, word_2 and word_3
word_1=unique(word_1)
word_2=unique(word_2)
word_3=unique(word_3)

#joining words_1, words_2 and words_3 to form a single vector called all_words 
all_words=c(word_1, word_2, word_3)

#deleting duplications from all_words
all_words=unique(all_words)

#sorting all_words alphabetically
all_words=sort(all_words)


#takes as input word and string.
#returns the number of times the word appears in the string.
#Example. If word="ab" and string = "ab 1 cd 2 ab 3", then output is 1+3=4. 
count_word_in_string=function(word, string){
  word_space=paste(word, " ", sep = "")
  
  if(length(grep(word_space, string))==0){
    return(0)
  }
  
  g=gregexpr(word_space, string)
  vect=c() #vector of counts
  
  x=`attributes<-`(g[[1]],NULL) #x keeps the starting position of each match
  y=attr(g[[1]], "match.length") #y keeps the match length
  
  for(j in 1:length(x)){
    temp=substr(string, x[j]+y[j], x[j]+y[j])
    vect[length(vect)+1]=as.integer(temp)
  }
  return(sum(vect))
}





length(all_words) #10129


#splitting all_words into 6 smaller vectors.
#this was necessary because size of input was too big for my computer.
all_words_1=all_words[1:2000]
all_words_2=all_words[2001:4000]
all_words_3=all_words[4001:6000]
all_words_4=all_words[6001:8000]
all_words_5=all_words[8001:10000]
all_words_6=all_words[10001:10129]


#word count for section 1
section_1=c()
for(word in all_words_1){
  section_1[length(section_1)+1]=count_word_in_string(word, book[1])
}


for(word in all_words_2){
  section_1[length(section_1)+1]=count_word_in_string(word, book[1])
}


for(word in all_words_3){
  section_1[length(section_1)+1]=count_word_in_string(word, book[1])
}

for(word in all_words_4){
  section_1[length(section_1)+1]=count_word_in_string(word, book[1])
}

for(word in all_words_5){
  section_1[length(section_1)+1]=count_word_in_string(word, book[1])
}

for(word in all_words_6){
  section_1[length(section_1)+1]=count_word_in_string(word, book[1])
}

section_1
#word count for section 1 complete


#word count for section 2
section_2=c()
for(word in all_words_1){
  section_2[length(section_2)+1]=count_word_in_string(word, book[2])
}

for(word in all_words_2){
  section_2[length(section_2)+1]=count_word_in_string(word, book[2])
}

for(word in all_words_3){
  section_2[length(section_2)+1]=count_word_in_string(word, book[2])
}

for(word in all_words_4){
  section_2[length(section_2)+1]=count_word_in_string(word, book[2])
}

for(word in all_words_5){
  section_2[length(section_2)+1]=count_word_in_string(word, book[2])
}

for(word in all_words_6){
  section_2[length(section_2)+1]=count_word_in_string(word, book[2])
}

section_2
#word count for section 2 complete


#begin word count for section 3
section_3=c()
for(word in all_words_1){
  section_3[length(section_3)+1]=count_word_in_string(word, book[3])
}

for(word in all_words_2){
  section_3[length(section_3)+1]=count_word_in_string(word, book[3])
}

for(word in all_words_3){
  section_3[length(section_3)+1]=count_word_in_string(word, book[3])
}

for(word in all_words_4){
  section_3[length(section_3)+1]=count_word_in_string(word, book[3])
}

for(word in all_words_5){
  section_3[length(section_3)+1]=count_word_in_string(word, book[3])
}

for(word in all_words_6){
  section_3[length(section_3)+1]=count_word_in_string(word, book[3])
}
section_3
#word count for section 2 complete


#creating a dataframe with section_1, section_2 and section_3 as columns
word_counts_df=data.frame(section_1, section_2, section_3)
word_counts_df

#setting the row names as the words in the book
rownames(word_counts_df)=all_words

word_counts_df

#writing the data frame word_counts_df into a textfile titles atotc_df.txt
directory_path = "~/Documents/Rstuff/hw5"
filename_Q4_2 = "atotc_df.txt"
description_Q4_2=file.path(directory_path,filename_Q4_2)
write.table(word_counts_df, description_Q4_2, sep = "\t")



