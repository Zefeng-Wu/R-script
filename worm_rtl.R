## Normally, it will need 20-30 minutes to run this calculation, which depends on
## the hardware of your computer and the condition of your Internet.
## Please check this calculation periodically to ensure it is running,
## because this algorithm will be stopped when the Internet is poor.
## By Xingfeng Si. May 08, 2014

require(RCurl)
require(XML)

npaper=365
res = matrix(0,ncol=7,nrow=npaper,dimnames=list(1:npaper,c('Received','Accepted','Published','Public','Citation','Editor','Subject')))
res = data.frame(res)

#retrive numbers
no.f=function(test){
  test=as.character(test)
  ntest=nchar(test)
  testsplit=strsplit(test,NULL)[[1]]
  en.res=paste(testsplit[testsplit%in%c(0:9)],collapse="")
  en.res
}

for (i in 1:npaper) {
    urlPJ=paste('https://peerj.com/articles/',i,'/',sep="")
    docPJ = getURL(urlPJ)#,timeout = 130)
    treePJ = htmlTreeParse(docPJ)
    
    body = treePJ$chi[['html']][['body']]
    content=xmlValue(body[[5]][[3]][[1]])
    
    date.bg = nchar(strsplit(content,"Published20")[[1]][1])+1 #gregexpr("Published20",content)[[1]]
    date.ed = nchar(strsplit(content,"Academic Editor")[[1]][1])
    date = substr(content,date.bg,date.ed)
    Rdate=substr(date,46,55)
    Adate=substr(date,28,37)
    Pdate=substr(date,10,19)
    
    editor.bg = nchar(strsplit(content,"Academic Editor")[[1]][1])+1+nchar("Academic Editor")
    editor.ed = nchar(strsplit(content,"Subject Area")[[1]][1])
    editor = substr(content,editor.bg,editor.ed)
    
    subject.bg = nchar(strsplit(content,"Subject Areas")[[1]][1])+1+nchar("Subject Areas")
    subject.ed = nchar(strsplit(content,"Keywords")[[1]][1])
    subject = substr(content,subject.bg,subject.ed)
    
    citation.bg=nchar(strsplit(content,"Articles citing this paper")[[1]][1])+nchar('Articles citing this paper')
    citation = no.f(substr(content,citation.bg+1,citation.bg+4))
    if (citation=="") citation=0
    
    public.yn = strsplit(content,"makethe review history of this articlepublic.")
    public = summary(public.yn[[1]])[1]==2 ## if review history was set as public, return TRUE
    
    res[i,"Received"]=Rdate
    res[i,"Accepted"]=Adate
    res[i,"Published"]=Pdate
    res[i,"Editor"]=editor
    res[i,"Public"]=public
    res[i,"Citation"]=citation
    res[i,"Subject"]=subject
    write.table(res,"res.txt")
    print(paste(i,format(Sys.time(), "%H:%M:%S")))
}

res # file ‘res’ includes the needed information of each article
