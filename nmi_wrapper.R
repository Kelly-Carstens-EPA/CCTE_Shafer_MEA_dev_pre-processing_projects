require('pracma')
library('compiler')


nmi_wrapper<-function(mutdata){

apply_fun<-function(c){
mutinfo<-sapply(c,nmi)
}

L<-length(mutdata)

meta<-list(mutdata[[L-3]],mutdata[[L-2]],mutdata[[L-1]],mutdata[[L]])
mutdata2<-mutdata            ### removing the meta info here
mutdata2[[L]]<-NULL
mutdata2[[L-1]]<-NULL
mutdata2[[L-2]]<-NULL
mutdata2[[L-3]]<-NULL

mutdata3<-list()
for (i in 1:length(mutdata2)){       #### I take the transpose here so that mutual info values match up with the correct wells (I think, need to have Ken double check this)
  mutdata3[[i]]<-t(mutdata2[[i]])
}

#mutdata3<-sapply(mutdata2,t) doesn't do what I would expect, which is why I used the for loop above instead 

testing<-sapply(mutdata3,apply_fun)

div5<-data.frame(meta[[1]],testing[,1])
div7<-data.frame(meta[[2]],testing[,2])
div9<-data.frame(meta[[3]],testing[,3])
div12<-data.frame(meta[[4]],testing[,4])

heading<-c("date","Plate.SN","DIV","well","trt","dose","units","file.name", "Mutual Information")

colnames(div5)<-heading
colnames(div7)<-heading
colnames(div9)<-heading
colnames(div12)<-heading


output<-rbind(div5,div7,div9,div12)

return(output)
}

