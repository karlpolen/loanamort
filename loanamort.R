loanamort=function(r=NULL,bal0=NULL,pmt=NULL,n=NULL,apr=FALSE,start=NULL,freq=1) {
  ans=list()
  risnull=is.null(r)
  bal0isnull=is.null(bal0)
  pmtisnull=is.null(pmt)
  nisnull=is.null(n)
  if(1<sum(c(risnull,bal0isnull,pmtisnull,nisnull))) stop('loanamort error -- need to provide at least three parameters')
  n.f=n
  if(apr) n.f=n*freq
  if(!risnull) {
    if(apr) {
      r.f=r/freq
    } else {
      r.f=-1+(1+r)^(1/freq)
    }
  } else {
    cf=c(-bal0,rep(pmt,n.f))
    if(0<=sum(cf)) {
      rootrange=c(0,1.01) } else {
      rootrange=c(1,1000)
      }
    d=(uniroot(function(d) {sum(cf*d^(0:n.f))},rootrange))$root
    r.f=(1/d)-1
  }
  d=1/(1+r.f)
  f=1+r.f
  if(pmtisnull) pmt=(bal0*r.f)/(1-d^n.f)
  perp=pmt/r.f
  if(bal0isnull) bal0=perp-perp*(d^n)
  if(pmt<=(r.f*bal0)) stop(paste(pmt,r.f*bal0,'payment must be greater than interest'))
  if(nisnull) n.f= ceiling(log((1-(bal0*r.f)/pmt))/log(d))
  i=1:n.f
  bal=pmax(0,((bal0*f^i)-(((pmt*f^i)-pmt)/r.f)))
  balall=c(bal0,bal)
  int=balall[i]*r.f
  prin=-diff(balall)
  if(!is.null(start)) {
    bal=zooreg(bal,start=start+1/freq,freq=freq)
    int=zooreg(int,start=start+1/freq,freq=freq)
    prin=zooreg(prin,start=start+1/freq,freq=freq)
  }
  if(apr) {
    ans$r=r.f*freq
    ans$n=n.f/freq
  } else {
    ans$r=-1+((1+r.f)^freq)
    ans$n=n.f
  }
  ans$pmt=pmt
  ans$bal0=bal0
  ans$freq=freq
  ans$start=start
  ans$apr=apr
  ans$bal=bal
  ans$prin=prin
  ans$int=int
  return(ans)
}
lastinvec=function(x) tail(x,1)
