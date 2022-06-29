#Lecture

my_data = read.csv(file = "Possum.csv")

head(my_data)

my_data_rownames = read.csv(file = "Possum.csv",
                            row.names = 1 )
head(my_data)

#.RData

myName = "ece"
myAge = 21
myHomeTown = "eskiþehir"

save(list = c("myName", "myAge", "myHomeTown", "my_data_rownames"),
     file = "myObjects.RData")

rm(list = ls())
ls()
load("myObjects.RData")

#Manipulating Data
#subsetting

my_grades = c(26, 53, 89, 45, 100)
names(my_grades) = c("mt1", "mt2", "q1", "q2", "final")

my_grades["final"]

my_grades[c("final","q1", "mt1")]

head(my_data_rownames,100)

class(my_data_rownames)
my_data_rownames[,"Pop"]
my_data_rownames["C23",]
my_data_rownames[,c("Pop", "sex")]

my_data_rownames["C23", "Pop"]

read.csv(file = "Possum.csv")

my_grades
my_grades[3]
my_grades[c(2,3)] #second and third indices are printed
my_grades[1]
my_grades[-1] #first indice is omitted
my_grades
my_grades[-c(2,3)] #second and third indices are omitted

head(my_data_rownames)
my_data_rownames[1:6,]
my_data_rownames[c(1,2,3,4,5,6),]

my_data_rownames[1:6,1:9] #left of the comma rows right of the comma columns

#with logical vector

c(TRUE,FALSE,TRUE)
c(T,F)

passed = my_grades >= 60
class(passed)

my_grades[passed]
my_grades[my_grades >= 60]

my_grades[c(T,T,F,F,F)]
my_grades[c(F,F,T,F,T)]

head(my_data_rownames)
my_data_rownames[1:6,"Pop"]
my_data_rownames[c("C3","C5"),3] #3 means third column

guppy = read.csv(file = "GuppyFatherSon.csv")
head(guppy,)

guppy[1:10, "sonAttractiveness"]
guppy[1:10,2] #2 means sonAttractivenes


highly_ornamented = guppy[,"fatherOrnamentation"] >  0.5
as.numeric(highly_ornamented)
sum(highly_ornamented)
length(highly_ornamented)

less_attractive = guppy[,"sonAttractiveness"] < 0.5
sum(less_attractive)

highly_ornamented_values = guppy[highly_ornamented, "fatherOrnamentation"]
highly_ornamented_values

less_attractive_values = guppy[less_attractive, "sonAttractiveness"]
less_attractive_values

uglySon_HotDad = less_attractive & highly_ornamented

sum(uglySon_HotDad) / length(uglySon_HotDad)
mean(uglySon_HotDad)


mean(my_grades)

my_grades2 = my_grades
my_grades3 = my_grades

my_grades2[c(1,2,5)] = c(50,50,0) #change 1.2.5. values
my_grades2
my_grades

my_grades3[c(1,2,5)] = NA
my_grades3

names(my_grades3)[c(1,2,5)] = "" #deleted the names of the 1.2.5. indices
my_grades3

char_vec = c("ece", "çýnar", "21", "43", "28")
as.numeric(char_vec)

num_vec = 1:10
as.character(num_vec)

#Example 4

possum = my_data_rownames
head(possum)

colnames(possum)[8] = "totalLength"
head(possum)

site3 =possum[, "site"] == "3"
site4 =possum[, "site"] == "4"

siteCondition = site3 | site4
possum[siteCondition,] #column should be defined with comma 

possum[siteCondition,"sex"] = "m"
possum[siteCondition,]

ageCondition = possum[,"age"] <= 1
head(ageCondition)

possum[ageCondition, "sex"] = "u"
possum[ageCondition,]

class(possum[,"sex"])
levels(possum[,"sex"])


#after the warning

possum[,"sex"] = as.character(possum[,"sex"])

possum[ageCondition, "sex"] = "u"
possum[ageCondition,]

class(possum[,"sex"])
levels(possum[,"sex"])

possum[,"sex"] = as.factor(possum[,"sex"])

class(possum[,"sex"])
levels(possum[,"sex"])
possum[ageCondition,]


write.csv(x = possum, file = "possum_edited.csv",
          row.names = T, quote = F)

possum[,"sex"]
possum$chest #dollar sign gives all column names

### Exercises 
#1

load(file = "w2_exercise.RData")
head(hospital)
heartFailure = hospital[, "Measure.ID"] == "MORT_30_HF"
heartFailure
#heartFailure = hospital[ hospital[, "Measure.ID"] == "MORT_30_HF",]
heartFailure_characters = hospital[heartFailure, "Measure.ID" ]
heartFailure_characters
head(heartFailure)
as.character(heartFailure)
tail(heartFailure)

head(heartFailure_characters)

#heartFailure = hospital[hospital$Measure.ID == "MORT_30_HF"] another way to write of previous line
# which is heartFailure_characters object

length(heartFailure)
#what is the purpose of this line


heartFailure
options(max.print=100)
heartFailure_values = hospital[heartFailure, "Score"]
heartFailure_values

sum(heartFailure_values)


# There is logical vector values so that the mean is calculated from 1 or 0 values
# we don't want to this we have to convert to real values of better_heart_fail etc. 

####selection = heartFailure$Compared.to.National == "better"
####heartFailure$TrueFalse = selection
####head(heartFailure,20)


better_heart_fail = hospital[heartFailure,"Compared.to.National"] == "better" #is a t f vector
better_heart_fail
better_heart_fail_values = hospital[better_heart_fail, "Score"]
better_heart_fail_values
sum(better_heart_fail)
sum(better_heart_fail_values)


worse_heart_fail = hospital[heartFailure,"Compared.to.National"] == "worse"
worse_heart_fail
worse_heart_fail_values = hospital[worse_heart_fail, "Score"]
worse_heart_fail_values
sum(worse_heart_fail)
sum(worse_heart_fail_values)


noDiff_heart_fail = hospital[heartFailure, "Compared.to.National"] == "noDiff"
noDiff_heart_fail
noDiff_heart_fail_values = hospital[noDiff_heart_fail, "Score"]
noDiff_heart_fail_values
sum(noDiff_heart_fail)
sum(noDiff_heart_fail_values)
  

k = mean(better_heart_fail_values)
l = mean(worse_heart_fail_values)
m = mean(noDiff_heart_fail_values)
k
l
m
(k < m) & (m < l)

# 8.34
#15.23
#11.62 
#True

#2


View(genome_annotation)
retrived_genes = genome_annotation[,"entry"] == "gene"
retrived_genes
head(genome_annotation[retrived_genes,])

a = genome_annotation[,"transcript_id"] 
a
manipulated_data = genome_annotation[c(retrived_genes),"transcript_id"] = NA
a
genome_annotation
manipulated_data

#genome_annotation[retrieved_genes,"transcript_ID"] = NA

genome_annotation$length = genome_annotation$end - genome_annotation$start + 1 #note works as a function but dollar sign allows us to create a new column
chr19 = genome_annotation[, "chromosome"] == 19 & retrived_genes
chr21 = genome_annotation[, "chromosome"] == 21 & retrived_genes

chr19_gene_lengths = genome_annotation[chr19, "length"]
chr21_gene_lengths = genome_annotation[chr21, "length"]

chr19_gene_lengths
chr21_gene_lengths

length(chr19_gene_lengths)
length(genome_annotation[,"length"]) #length(genome_annotation$length)

mean(chr19_gene_lengths) < mean(chr21_gene_lengths)

#21>19 
