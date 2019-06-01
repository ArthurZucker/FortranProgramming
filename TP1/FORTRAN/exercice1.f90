program EntierPremier
  implicit none
  !Declaration des variables
  integer :: i,j,n,res
  call system("clear")
  write(*,fmt = "(/,'.Veuillez indiquer la limite N : ')",advance='no')
  read *,n 
  do i=2,n,1
     res = 0
     do j=1,i-1,1
        if (mod(i,j)==0)  then
          res = res +j 
        end if 
     end do
     if (res==i)  then
        write(*,fmt="(/,'... ',2X,i8,' est somme de ses diviseurs')")i
     end if
  end do 
  stop "Fin entier premier"
end program EntierPremier
