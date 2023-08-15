par(mfrow=c(5,2),mar=c(.7,0,1.1,1),omi=c(0,2,.5,2),pty='s')
cex=1
a.val<-c(.00001,1,3,5,10,20,1000)

x<-delays
x<-x[!is.na(x)]
nbin<-length(hist(x,plot=F)$counts)+1

        x <- sort(x)
        n <- length(x)
        p <- ((1:n) - 1/2)/n

main.lab<-NULL
plot(x,p,col=1,pch='.',cex=.5)
		usr<-par('usr')
		text(usr[1],usr[4]-par('cxy')[2]/2,
		paste('  num = ',length(x),sep=''),cex=.5,adj=0)


data<-NULL
a.vec<-c(.00001,1,5,11,1000,.00001,1,5,11)[c(5,1,6,2,7,3,8,4,9)]
offset<-c(rep(0,5),rep(2,4))[c(5,1,6,2,7,3,8,4,9)]
rx<-range(x,na.rm=T)
for ( j in seq(length(a.vec))) {
	a.val<-a.vec[j]
	offset.val<-diff(rx)/(nbin-1)*offset[j]/4
	new.rx<-rx
	new.rx[1]<-rx[1]-offset.val
	new.rx[2]<-new.rx[2]-offset.val+diff(rx)/(nbin-1)

	if (a.val==.00001) lab<-paste('histogram \noffset =',
		format(round(offset.val)),' ')
	else if (a.val==1000) lab<-paste('e-a hist \noffset =',
		format(round(offset.val)),' ')
	else lab<-paste('dhist; a =',a.val,'*IQR \noffset = ',
		format(round(offset.val)),sep='',' ')
	dhist(x,nbin=nbin,a=a.val*iqr(x),xlab='',rx=new.rx)
	usr=par('usr')
	text(usr[2],usr[4]-par('cxy')[2]*1.2,lab,adj=1,cex=.6)
	box()
}
