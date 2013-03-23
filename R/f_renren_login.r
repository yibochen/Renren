

# 校内登录的函数
f_renren_login <- function(name='****', 
                           pwd='****', 
                           cookie_file=NULL){
  require(RCurl)
  if(is.null(cookie_file)){
    
    d <- debugGatherer()
    cH <- getCurlHandle(followlocation=T, verbose=T, 
                        debugfunction=d$update, 
                        ssl.verifyhost=F, ssl.verifypeer=F, 
                        cookiejar='./cookies', cookiefile='./cookies')
    
    pinfo <- c(
      'email'=name,
      'password'=pwd,
      'origURL'='http://www.renren.com/Home.do',
      'domain'='renren.com'
    )
    
    # 登录
    x <- try(ttt <- postForm('http://passport.renren.com/PLogin.do', 
                             .params=pinfo, curl=cH, style='post'), silent=T)
    if(class(x) == 'try-error'){cat('no!!!!!!', '\n');return(NULL)}
  } else{
    d <- debugGatherer()
    cH <- getCurlHandle(followlocation=T, verbose=T, 
                        debugfunction=d$update, 
                        ssl.verifyhost=F, ssl.verifypeer=F, 
                        cookiejar='./cookies', cookiefile=cookie_file)
  }
  return(cH)
}

