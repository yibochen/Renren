Renren
这是一个用于抓取人人网站数据并做一些分析的R包
http://www.renren.com
======
zip包下载地址：http://vdisk.weibo.com/s/uKPy-

目前主要的函数有：
f_renren_login      登录
f_renren_sns        好友关系网络的可视化

具体使用可以参考帮助文档  

下面是demo:
require(Renren)

# 0、登录
ch0 <- f_renren_login('myemail', 'mypwd')
# 模拟登录失效的时候，可以尝试用cookie文件登录
# 其中的cookie文件可以通过Firefox的Firebug插件获得
ch0 <- f_renren_login(cookie_file='C:/Users/ASUS/Desktop/cookies.txt')

# 1、好友关系网络的可视化
renren_sns <- f_renren_sns(cH=ch0, topk=3)
head(renren_sns$sns_df)
