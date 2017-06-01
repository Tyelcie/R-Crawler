rm(list = ls())
library(bitops)
library(RCurl)
library(ggplot2)

getcontent <- function(s,g){
  substring(s,g,g+attr(g,'match.length')-1)
}

url<-'http://jamanetwork.com/journals/jama/currentissue?text='
web <- readLines(url,encoding = 'UTF-8')

Issue.pre <- web[grep('<strong>',web)]
Issue <- NULL
for(i in 1:length(Issue.pre)){
  Issue1 <- gregexpr('<strong>',Issue.pre)
  Issue2 <- gregexpr('</strong>',Issue.pre)
  Issue <- cbind(Issue,substr(Issue.pre[i],Issue1[[i]][1]+8,Issue2[[i]][1]-1))
}
Vol <- substr(web[grep('<strong>',web)+1][1],17,24)
No. <- substr(web[grep('<strong>',web)+2][1],17,23)
Page <- substr(web[grep('<strong>',web)+3][1],21,35)

Current <- paste(Issue[2],Issue[1],Vol,No.,Page)

Type.pre <- web[grep('<div class="article-type-group thm-col">',web)]
Type <- 
for(i in 1:length(Type.pre)){
  Type1 <- gregexpr('col">',Type.pre)
  Type2 <- gregexpr('</div><div class="issue-group-articles">',Type.pre)
  Type <- cbind(Type,substr(Type.pre[i],Type1[[i]][1]+5,Type2[[i]][1]-1))
}

Title.pre <- web[grep('<h3 class="article--title">',web)]
Title <- NULL
for(i in 1:length(Title.pre)){
  Title1 <- gregexpr('fullarticle',Title.pre)
  Title2 <- gregexpr('</a>',Title.pre)
  Title <- cbind(Title,substr(Title.pre[i],Title1[[1]]+21,Title2[[i]][1]-1))
}

Author.pre <- web[grep('<div class="article--authors">',web)+1]
# how to substract the first 20 spaces?

Citation.pre <- web[grep('<div class="article--citation">',web)+1]
J <- substr(Citation.pre,21,25)
Meta <- NULL
for(i in 1:length(Citation.pre)){
  Meta1 <- gregexpr('"meta-citation\">',Citation.pre)
  Meta2 <- gregexpr('</span>',Citation.pre)
  Meta <- cbind(Meta,substr(Citation.pre[i],Meta1[[i]][1]+17,Meta2[[i]][1]-1))
}
Citation <- paste(J,Meta)

Content <- data.frame(Title = t(Title),Citation = Citation)

Current.Issue <- list(Current, Content)
