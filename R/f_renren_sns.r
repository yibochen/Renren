

# 校内好友关系
f_renren_sns <- function(cH=ch0, topk=3){
  
  require(RCurl)
  require(RJSONIO)
  
  the0url <- 'http://friend.renren.com/myfriendlistx.do'
  the0get <- getURL(the0url, curl=cH, .encoding='UTF-8')
  # write(the0get, 'xxx.txt')
  uid <- gsub('^.*\n\'id\':\'(\\d+)\'.*$', '\\1', the0get)
  # uid
  # "41021031"
  
  a1 <- gsub('^.*var friends=\\[(\\{.+\\})];.*$', '\\1', the0get)
  a1 <- strsplit(a1, '\\},\\{')[[1]]
  a1 <- gsub('\\{|\\}', '', a1)
  a1 <- paste('{', a1, '}', sep='')
  a_id <- unname(sapply(a1, function(x){fromJSON(x)$id}))
  a_name <- unname(sapply(a1, function(x){fromJSON(x)$name}))
  
  my_friends <- data.frame(u0=NA, id0=uid, u1=a_name, id1=as.character(a_id), stringsAsFactors=F)
  my_friends <- unique(my_friends)
  
  # 如果我没有好友 ="=
  if(nrow(my_friends) <= 0){
    cat('no!!!!!!i have no friends!!!!', '\n')
    return(NULL)
  } else{
    # 查找共同好友
    f_share <- function(id, name){
      share_url <- paste('http://friend.renren.com/shareFriends?p={%22init%22:true,%22uid%22:true,%22uhead%22:true,%22uname%22:true,%22group%22:true,%22net%22:true,%22param%22:{%22guest%22:', 
                          id, '}}', sep='')
      h <- getURL(share_url, curl=cH)
      h2 <- fromJSON(h)$candidate
      if(length(h2) > 0){
        newid <- sapply(h2, function(x){x$id})
        cat('#shareFriends with ', name, ' : ', length(newid), '\n')
        return(data.frame(id0=id, id1=newid))
      } else{
        cat('no shareFriends with ', name, '\n')
        return(NULL)
      }
    }
    sns_df <- NULL
    for(index in seq_len(nrow(my_friends))){
      userid <- my_friends$id1[index]
      username <- my_friends$u1[index]
      new_df <- f_share(userid, username)
      sns_df <- unique(rbind(sns_df, new_df))
    }
    sns_df$id0 <- as.character(sns_df$id0)
    sns_df$id1 <- as.character(sns_df$id1)
    
    # 去掉已经注销的账号
    sns_df <- sns_df[sns_df$id1 %in% my_friends$id1 & sns_df$id0 %in% my_friends$id1, ]
    
    require(igraph)
    
    people <- unique(data.frame(id=my_friends$id1, name=my_friends$u1, stringsAsFactors=F))
    people <- people[people$id %in% sns_df$id0, ]
    gg <- graph.data.frame(d=sns_df, directed=F, vertices=people)
    is.simple(gg)
    gg1 <- simplify(gg, remove.loops=T, remove.multiple=T)
    is.simple(gg1)
    dg <- degree(gg1)
    # V(gg1)[dg == 0]
    gg2 <- induced.subgraph(gg1, which(dg > 0))
    
    # 子群划分
    com <- walktrap.community(gg2, steps=4)
    subgroup <- split(com$names, com$membership)
    V(gg2)$sg <- com$membership
    
    # 图形的参数，这个需要设计一下  ="=
    # V(gg2)$degree <- degree(gg2, mode='in')
    V(gg2)$betweenness <- betweenness(gg2)
    top_b <- quantile(V(gg2)$betweenness, (length(V(gg2))-topk)/length(V(gg2)))
    V(gg2)$size <- 2
    V(gg2)$label <- NA
    V(gg2)$labelcex <- 3
    V(gg2)[betweenness>=top_b]$size <- 6
    V(gg2)[betweenness>=top_b]$label <- V(gg2)[betweenness>=top_b]$name
    V(gg2)$vertexcolor <- rainbow(max(V(gg2)$sg))[V(gg2)$sg]
    V(gg2)$framecolor <- rainbow(max(V(gg2)$sg))[V(gg2)$sg]
    
    png(paste('renren_mysns_', Sys.Date(), '.png', sep=''),width=1000,height=1000)
    par(mar=c(0,0,0,0))
    set.seed(17)
    plot(gg2,
         layout=layout.fruchterman.reingold,
         vertex.size=V(gg2)$size,
         vertex.label=V(gg2)$label,
         vertex.label.cex=V(gg2)$labelcex,
         vertex.label.color=1,
         vertex.label.dist=0,
         vertex.color=V(gg2)$vertexcolor,
         vertex.frame.color=V(gg2)$framecolor,
         edge.color=grey(0.8),
         edge.arrow.size=0.5,
         edge.arrow.width=0.5
    )
    dev.off()
    return(list(my_friends=my_friends, sns_df=sns_df))
  }
}

