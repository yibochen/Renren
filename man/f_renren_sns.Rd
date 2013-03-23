\name{f_renren_sns}
\alias{f_renren_sns}
\title{好友关系网络的可视化}
\usage{
  f_renren_sns(cH=ch0, topk=3)
}
\arguments{
  \item{cH}{之前登录获得的CurlHandle}
  \item{topk}{关注前topk个节点}
}
\value{
  \item{my_friends}{数据框，当前账户的好友列表，字段分别为：昵称0、ID0、昵称1、ID1}
  \item{sns_df}{数据框，好友之间的关注关系，字段分别为：ID0、ID1}
}
\details{
  运行完成后，会在工作路径下生成renren_mysns_时间.png文件
}
\examples{
renren_sns <- f_renren_sns(cH=ch0, topk=3)
head(renren_sns$sns_df)
}

