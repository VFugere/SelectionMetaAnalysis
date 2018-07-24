make.italic <- function(x) as.expression(lapply(x, function(y) bquote(italic(.(y)))))

'%!in%' <- function(x,y)!('%in%'(x,y))

outplot <- function(x,p=F,id=F){
  plot(x=x,y=1:length(x),pch=1,col='grey',xlab='value',ylab='index')
  abline(v=mean(x),lwd=3)
  abline(v=(mean(x)-sd(x)),lty=2,lwd=2)
  abline(v=(mean(x)+sd(x)),lty=2,lwd=2)
  abline(v=(mean(x)-2*sd(x)),lty=2)
  abline(v=(mean(x)+2*sd(x)),lty=2)
  if (p)
  {
    sx = scale(x)
    sx = abs(sx)
    i = which(sx > 2)
    r = data.frame('row' = i,'value' = x[i])
    print(r)
    rm(sx,i)
  }
  if (id)
    identify(x=x,y=1:length(x))
}