par(mfrow=c(2,2),mar=c(3.1,0,1.1,0),omi=c(2,.5,.5,0),pty='s',xpd=T)
cex=1
a.val<-c(.00001,1,3,5,10,20,1000)
x<-times
num.bins<-round(rev(length(x)/c(10,20,30,40,50)))[c(1,3)]
        x <- sort(x)
        n <- length(x)
        p <- ((1:n) - 1/2)/n

main.lab<-NULL
plot(x,p,col=1,pch='.',xlab='',ylab='',cex=cex,yaxt='n')
		usr<-par('usr')
text(usr[1],usr[4]-par('cxy')[2]/2, ' ecdf',cex=cex,adj=0)


data<-NULL
for ( j in seq(length(a.val))[c(7,1,4)]) {
	if (a.val[j]==.00001) lab<-'histogram'
	else if (a.val[j]==1000) lab<-'e-a hist'
	else lab<-paste('dhist; a =',a.val[j],'*IQR',sep='')
	#dhist(x,nbin=14,a=a.val[j]*iqr(x),xlabel='')
	dhist(x,nbin=14,a=a.val[j]*iqr(x),xlab='')
		usr<-par('usr')
text(usr[1],usr[4]-par('cxy')[2]/2, paste(' ',lab),cex=cex,adj=0)
	box()
}
