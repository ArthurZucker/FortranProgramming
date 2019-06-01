program suite
  implicit none
  integer:: i, n
  real :: u0 = 3. ,u1 = sqrt(3.)
  real :: un

  write (*,fmt = "('Veuillez indiquer le rang N',2X)", advance = 'no')
  read *,n
  if (n == 1) then
     write(*, fmt = "(3X,'iteration 0 :  ',3X,f5.3)")u0
  else if (n == 2) then 
     write(*, fmt = "(3X,'iteration 1 :  ',3X,f5.3)")u1
  end if 
  do i=1,n-1,1
     un = u1 * 1/2 + sqrt(1+cos(u0))
     write(*, fmt = "(3X,'iteration',X,i3,':',2X,f5.3)")i,un
     u0 = u1
     u1 = un
  end do 
   write(*, fmt = "(3X,'Le ',i3,'-ème terme vaut :',2X,f5.3)")n,un



  
stop "fin suite"
end program suite
