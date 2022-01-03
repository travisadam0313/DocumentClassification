library(tesseract)
library(ggplot2)
library(stringr)
library(ijtiff)
library(tiff)
eng <- tesseract("eng")
setwd("~/Desktop/Programming/R/Document Classification")

#Cert Comm Training Images
Document_A1<-"usmccertcom.png"
Document_A2<-"usmccertcom2.jpg"
#Promotion Warrant Training Images
Document_B1<-"mcepw.png"
Document_B2<-"mcepw2.jpeg"

targetImage<-"targetImageCC.png" #Image that is known to be in the training data that we would like to classify.
  
#Classify Each Training Image Into A Text Table (be careful find & replacing this)
Document_B1_Txt<-tesseract::ocr(as.character(Document_B1)) #The OCR Function that reads the document
Document_B1_Txt_Table<-data.frame(strsplit(Document_B1_Txt, " ")[[1]]) #Stage OCR Results in a vector via string split operation
colnames(Document_B1_Txt_Table)<-c('Text') #Provide a column name
Document_B1_Txt_Table$Count<-1 #Populate a column of 1's for word counting
Document_B1_Txt_Table<-aggregate(Document_B1_Txt_Table$Count, by=list(Text=Document_B1_Txt_Table$Text), FUN=sum) #Aggregate by word (Sum&Group BY in SQL)
colnames(Document_B1_Txt_Table)<-c('Text','Count')
Document_B1_Txt_Table$Count<-Document_B1_Txt_Table$Count * -1
Document_B1_Txt_Table

#OCR Function that returns a dataframe of words found and their respective counts...
OCR_Function<-function(targetImage){
  target_Txt<-tesseract::ocr(as.character(targetImage)) #The OCR Function that reads the document
  target_Txt_Table<-data.frame(strsplit(target_Txt, " ")[[1]]) #Stage OCR Results in a vector via string split operation
  colnames(target_Txt_Table)<-c('Text') #Provide a column name
  target_Txt_Table$Count<-1 #Populate a column of 1's for word counting
  target_Txt_Table<-aggregate(target_Txt_Table$Count, by=list(Text=target_Txt_Table$Text), FUN=sum) #Aggregate by word (Sum&Group BY in SQL)
  colnames(target_Txt_Table)<-c('Text','Count')
  target_Txt_Table
}

#Compare target table against training data
Inner_Tbl_Function<-function(Target_Table,Training_Table){
  Inner_Table<-merge(Target_Table,Training_Table,by="Text")
  Inner_Table$z<-Inner_Table$Count.x+Inner_Table$Count.y
  nFound<-sum(Inner_Table$z==0)
  nPossible<-nrow(Target_Table) #figure this out start
  nPossibleT<-nrow(Training_Table)
  score<-nFound/(nPossibleT+nPossible)
  as.numeric(score)
}

#Evaluate Against Each Training Text Table
Eval_Function<-function(Target_Table){  
  #Build Empty Result Data Frame
  Result_Set<-as.data.frame(matrix(nrow=0,ncol=4))
  colnames(Result_Set)<-c("DocumentType","Score","Index","FileName")
  #Cert Comm Evaluation
  for(i in 1:2){
    Training_Table<-eval(as.symbol(paste0("Document_A",i,"_Txt_Table")))
    Document_Score<-Inner_Tbl_Function(Target_Table,Training_Table)
    Result<-c("Cert Comm",Document_Score,1,targetImage)
    Result_Set<-rbind(Result_Set,Result)
    colnames(Result_Set)<-c("DocumentType","Score","Index","FileName")
    Result_Set$Score<-as.numeric(Result_Set$Score)
  }
  #Promotion Warrant Evaluation
  for(i in 1:2){ #number could be static; it really depends how you intend to handle training sets
    Training_Table<-eval(as.symbol(paste0("Document_B",i,"_Txt_Table")))
    Document_Score<-Inner_Tbl_Function(Target_Table,Training_Table)
    Result<-c("Promotion Warrant",Document_Score,1,targetImage)
    Result_Set<-rbind(Result_Set,Result)
    colnames(Result_Set)<-c("DocumentType","Score","Index","FileName")
    Result_Set$Score<-as.numeric(Result_Set$Score)
  }
Result_Set
}#EndFunction

Run_Doc_Class<-function(targetImage){
  Results<-as.data.frame(matrix(nrow=0,ncol=4))
  colnames(Results)<-c("DocumentType","Score","Index","FileName")
  Target_Table<-OCR_Function(targetImage)
  Results_Rec<-Eval_Function(Target_Table) #only saves last image results; write some rbinds
  Results<-rbind(Results,Results_Rec)
  #Aggregate up the individual scores into a final scoring table  
  Results$Score<-as.numeric(Results$Score)
  Results<-na.omit(Results)
  Results$Score<-Results$Score * 10
  Final_Classification<-Results[with(Results, ave(Score, Index, FileName, FUN=max)==Score),]
  Final_Classification<-Final_Classification[with(Final_Classification, ave(Score, Index, FileName, FUN=max)==Score),]
  Final_Classification
}

Run_Doc_Class(targetImage)

#Match assessment purposes, produces plot of 'Final Classification'
MaxMatch<-max(Results[which(Results$Index==1),]$Score)*1.1
Results_Plt<-Results[which(Results$Index==1),]
Results_Plt<-Results_Plt[which(Results_Plt$FileName=="targetImageCC.png"),]
plt<-ggplot(Results_Plt,aes(DocumentType,Score))
plt+geom_col()+expand_limits(y = c(0, MaxMatch))

#A ShinyApp version is available on my Shiny IO page that allows user to provide an image to classify

