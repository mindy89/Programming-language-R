##注釋 (commands)
##習慣上, 整行的注釋使用雙井號作為開始
##運算式尾端注釋使用單井號開始

##基本指令
1 + 2
pi #圓周率
round(pi) #取整數

## assign an object
x <- 1 #等同於1 -> x
x
x = 2 #取代1
print(x)

## name an object on your own
ktl_lab <- "hello:)"

##不同的完整指令要在同一行輸入時, 可用 ; (分號) 隔開
x <- 1 + 2; y <- 3 + 4 #variables will be updated in environment

##vector
num <- 1
class(num) #class()可以檢視基本模式
is.numeric(num) #return TRUE or FALSE

##Logical
##TRUE(T) = 1，FALSE(F) = 0
2==3
4!=5
6<=7
TRUE + 5
FALSE * 5
T + F

##向量產生函式 c()
##輸入向量資料將數值或文字連接成向量，c()為 concatenate (連接)
empty.vec <- c() #空向量
num.vec <- c(1/1, 1/2, 1/3, 1/4, 1/5)
int.vec <- 1:5 #c(1:5)
int.vec <- int.vec[-3] #用負號刪除資料
linlab.vec <- c("KT", "PX", "YT", "RH", "YH", "PC", 
                "PY", "SYC", "GS", "YJ", "CW", "SYH", 
                "SYL", "ZLW", "MC")
logic.vec <- c(T, F, T, T, F)

##Arithmetic operator
a <- 1 ; b <- 1
a + 1 #unary
a <- a + 1 #把運算結果存到a
a + b #binary

##and/or
A <- 0 ; B <- 1
A & B
T | F
!TRUE
!A | B
A <- 1 ; B <- 0
A <- 1 ; B <- 1
A <- 0 ; B <- 0

##& <-> && ; | <-> ||
x.vec <- 1:5
y.vec <- c(0, 2, 4, 6, 8)
(x.vec > 0) & (y.vec > 0) 
## [1] FALSE  TRUE  TRUE  TRUE  TRUE
(x.vec > 0) && (y.vec > 0)
## [1] FALSE

##if-else
##縮排Indent
##利用tab鍵讓文字內縮，對於理解程式的邏輯和架構很重要
score <- 59
if(score >= 60){
  print("及格") #tab鍵後輸入if條件判斷為T要執行的程式
}else{
  print("不及格")
}
##if - else if - else
score <- 100
if(score == 100){
  print("滿分")
}else if(score > 60 | score == 60){
  print("及格")
}else{
  print("不及格")
}
##ifelse(test = 邏輯判斷,yes = 判斷為T要執行的程式碼
##                      ,no = 判斷為F要執行的程式碼)
scores <- c(88, 35, 46, 79, 90)
ifelse(scores >= 60, "及格", "不及格")

##Function
median(num.vec)
mean(int.vec)
lab <- matrix(linlab.vec, 3, byrow = T, 
              dimnames = list(c("row 1", "row 2", "row 3"), 
                              c("A", "B", "C", "D", "E")))
lab <- matrix(linlab.vec, 3)
colnames(lab) <- c("A", "B", "C", "D", "E")
rownames(lab) <- c("row 1", "row 2", "row 3")

##Matrix
lab[1, 1] #[第一列，第一欄]
lab[2, ] #第二列
lab[1:2, c(3, 5)]
lab["row 2", "B"]

##Array
array.3D <- array(1:24, dim = c(4, 3, 2), dimnames = list(
  X = c("x1","x2","x3","x4"),
  Y = c("y1", "y2", "y3"), 
  Z = c("z1", "z2")))
array.2D <- array.3D[,,1]
class(array.2D) #identical to matrix
array.mon <- array(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                     "Jul", "Aug", "sep", "Oct", "Nov", "Dec"))
for(i in 12:1){
  print(array.mon[i])
}
for(array in array.mon){
  print(array)
}

##For loop
for(i in 1:10){  #習慣上以i, j, k, … 命名
  print(i) 
}

for(i in c("A", "C", "B")){
  print(i) 
}

for(i in 1:3){
  for(j in 1:3){
    if(i == 2){ # & j == 3
      break
    }
    cat("i:", i, "j:", j, "\n") #等同於print
  }
}

##while loop
i <- 10
while(i > 0){
  i <- i - 2
  print(i)
}

##Data frame
id.vec <- 1:4
name.vec <- c("Joe", "Bob", "Vicky", "Lily")
age.vec <- c(35, NA, 45, 25) #NA = not available
sex.vec <- c("Male", "Male", "Female", "Female")
disease.vec <- c(1, 0, 0, 1) #1 = yes/T ; 0 = no/F
DF  <-  data.frame(
  id  = id.vec,
  name = name.vec,
  age = age.vec,
  sex = sex.vec,
  disease = disease.vec
)
DF <- na.exclude(DF)

#install.packages
install.packages("ggplot2") 
install.packages("survminer")
library("survminer")
require("survival")

##匯入資料
clinical <- read.csv(file = "C:/Users/.../clinical.tsv", header = T, sep = "\t")
samplesheet <- read.csv(file = "C:/Users/.../gdc_sample_sheet.2022-08-30.tsv", header = T, sep = "\t")
targetscan <- read.csv(file = "C:/Users/.../temp.txt", header = F, sep = "\n")

##Survival
survival_KRAS_LUAD <- data.frame()
clinical.row <- nrow(clinical)
clinical.col <- ncol(clinical)
samplesheet.row <- nrow(samplesheet)
samplesheet.col <- ncol(samplesheet)
targetscan.row <- nrow(targetscan)

temp <- colnames(clinical)
for(i in 1:clinical.col){
  if(isTRUE(temp[i] == "case_submitter_id")){
    case_ID.c <- i
  }
  else if(isTRUE(temp[i] == "vital_status")){
    vital_status <- i
  }
  else if(isTRUE(temp[i] == "days_to_last_follow_up")){
    days_to_last_follow_up <- i
  }
  else if(isTRUE(temp[i] == "days_to_death")){
    days_to_death <- i
  }
}

temp <- colnames(samplesheet)
for(i in 1:samplesheet.col){
  if(isTRUE(temp[i] == "Case.ID")){
    case_ID.s <- i
  }
  else if(isTRUE(temp[i] == "File.Name")){
    file_name <- i
  }
  else if(isTRUE(temp[i] == "Sample.Type")){
    sample_type <- i
  }
}

for(h in 1:targetscan.row){
  miR <- targetscan[h, 1]
  for(i in 1:clinical.row){
    for(j in 1:samplesheet.row){
      if(i %% 2 == 1 & clinical[i, case_ID.c] == samplesheet[j, case_ID.s]){
        survival_KRAS_LUAD[j, 1] <- clinical[i, case_ID.c]
        survival_KRAS_LUAD[j, 2] <- samplesheet[j, file_name]
        survival_KRAS_LUAD[j, 3] <- clinical[i, vital_status] 
        if(survival_KRAS_LUAD[j, 3] == "Alive"){
          survival_KRAS_LUAD[j, 4] <- 0
          survival_KRAS_LUAD[j, 3] <- clinical[i, days_to_last_follow_up] 
        }
        else if(survival_KRAS_LUAD[j, 3] == "Dead"){
          survival_KRAS_LUAD[j, 4] <- 1
          survival_KRAS_LUAD[j, 3] <- clinical[i, days_to_death] 
        }
        mir <- read.csv(file = paste0("C:/Users/.../", samplesheet[j, 1], "/", samplesheet[j, 2]), header = T, sep = "\t")
        mir.row <- nrow(mir)
        for(k in 1:mir.row){
          if(mir[k, 1] == miR){
            find.row <- k
            break
          }
        }
        survival_KRAS_LUAD[j, 5] <- mir[find.row, 2]
        survival_KRAS_LUAD[j, 6] <- samplesheet[j, sample_type]
      }
    }
  }
  for(j in 1:2){
    for(i in 1:samplesheet.row){
      if(isTRUE(survival_KRAS_LUAD[i, 3] == "'--")){
        survival_KRAS_LUAD <- survival_KRAS_LUAD[-i, ]
      }
      if(isTRUE(survival_KRAS_LUAD[i, 6] != "Primary Tumor")){
        survival_KRAS_LUAD <- survival_KRAS_LUAD[-i, ]
      }
    }
  }
  M <- median(survival_KRAS_LUAD[, 5])
  survival_KRAS_LUAD.row <- nrow(survival_KRAS_LUAD)
  for(i in 1:survival_KRAS_LUAD.row){
    if(isTRUE(survival_KRAS_LUAD[i, 5] > M)){
      survival_KRAS_LUAD[i, 7] <- ("high")
    }
    else{
      survival_KRAS_LUAD[i, 7] <- ("low") 
    }
  }
  colnames(survival_KRAS_LUAD) <- c("ID", "file_name", "days_to_last_follow_up", "vital_status", miR, "sample_type", "expression")
  write.csv(survival_KRAS_LUAD, file = paste0("C:/Users/.../survival_KRAS_LUAD_", miR, ".csv"), row.names = F)
  survival_KRAS_LUAD[ ,3]<- as.numeric(survival_KRAS_LUAD[ ,3])
  fit <- survfit(Surv(survival_KRAS_LUAD$days_to_last_follow_up, survival_KRAS_LUAD$vital_status) ~ survival_KRAS_LUAD$expression)
  ggsurvplot(fit, data = survival_KRAS_LUAD, xlim = c(0, 4000), xlab = "days to last follow up", pval = TRUE, legend.title = paste0("KRAS_LUAD_",miR))
  output <- ggsurvplot(fit, data = survival_KRAS_LUAD, xlim = c(0, 4000), xlab = "days to last follow up", pval = TRUE, print = TRUE, legend.title = paste0("KRAS_LUAD_",miR))
  ggsave(filename = paste0("C:/Users/.../survival_KRAS_LUAD_", miR, ".png"), plot = print(output), width = 5, height = 4)
}

##Scatter plot
samplesheet.RNA <- read.csv(file = "C:/Users/.../gdc_sample_sheet_RNA.2022-08-30.tsv", header = T, sep = "\t")
RNA.row <- nrow(samplesheet.RNA)
scatter_KRAS_LUAD <- data.frame()

miR <- readline(prompt = "miRNA:")
expression.miR <- read.csv(file = paste0("C:/Users/.../survival_KRAS_LUAD_", miR, ".csv"))
miR.row <- nrow(expression.miR)

for(i in 1:RNA.row){
  for(j in 1:miR.row){
    if(samplesheet.RNA[i, 6] == expression.miR[j, 1]){
      scatter_KRAS_LUAD[j, 1] <- expression.miR[j, 1]
      scatter_KRAS_LUAD[j, 2] <- samplesheet.RNA[i, 2]
      scatter_KRAS_LUAD[j, 3] <- expression.miR[j, 5]
      expression.RNA <- read.csv(paste0("C:/Users/.../", samplesheet.RNA[i, 1], "/", samplesheet.RNA[i, 2]), header = F, sep = "\t", stringsAsFactors = F)
      scatter_KRAS_LUAD[j, 4] <- expression.RNA[6789, 8]
    }
  }
}
colnames(scatter_KRAS_LUAD) <- c("ID", "file_name", miR, "KRAS")
write.csv(scatter_KRAS_LUAD, file = paste0("C:/Users/.../scatter_KRAS_LUAD_", miR, ".csv"))

x <- scatter_KRAS_LUAD$`hsa-mir-199a-1`
y <- as.numeric(scatter_KRAS_LUAD$KRAS)
ggscatter(scatter_KRAS_LUAD, main = paste0("KRAS_LUAD_", miR), x = miR, y = "KRAS", add = "reg.line", add.params = list(color = "blue")) + stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.001)
scatter <- ggscatter(scatter_KRAS_LUAD, main = paste0("KRAS_LUAD_", miR), x = miR, y = "KRAS", add = "reg.line", add.params = list(color = "blue")) + stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.001)
ggsave(filename = paste0("C:/Users/.../scatter_KRAS_LUAD_", miR, ".png"), plot = print(scatter), width = 5, height = 4)


##setwd
##Session -> set working directory -> choose directory