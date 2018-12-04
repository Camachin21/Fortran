program pendulum
  implicit none

  real:: l, ang, g=9.81, pi=3.1415,angr, P_0,p, n,f, error, rerr
  integer:: i
  print*, "inserta la longitud de la cuerda"
  read*, l
 
  P_0=(2.0*pi)*sqrt(l/g)
  open(1,file='tabla.dat',status='unknown')
  open(2,file='tabla2.dat',status='unknown')
  error=P_0
  Do i=0,90 
     angr=2.0*pi*float(i)/180.0
     p=(2.0*pi)*sqrt(l/g)*f(angr)
     rerr=abs((p-error)/p)*100.0
     if(rerr<1) then
        write(2,*) i,rerr
     else
        error=p
     end if
     
     write(1,*) angr, p/P_0
     
  end  do
 end program pendulum

 function f(angr)
   real,intent(in):: angr
   f=(1.0+(1.0/16.0)*angr**2.0+(11.0/3072.0)*angr**4.0+(173.0/737280.0)*angr**6.0+(22931.0/1321205760.0)*angr**8.0)
   end function f
