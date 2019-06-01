program listerprimes

  implicit none
  integer :: N,i,j
  call system("clear")
  write(*,fmt="('. Bienvenue dans Lister premiers')")
  write(*,fmt="(3X,'. Veuillez indiquer votre entier limite : ')",advance = 'no')
  read *,N
  write(*,fmt="(3X,'. [La liste]')")
  j = 1
  do i=1,N,1
     if (est_premier(i)) then
        write(*,fmt="(3X,i4,3X,i4)")j,i
        j = j+1
     end if
  end do
  stop "Fin lister premier..."
  
contains
  function est_premier(P)
    logical::est_premier
    integer,intent(in)::P
    integer :: i
    do i=2,P-1,1
       if (mod(P,i)==0) then
          est_premier = .false.
          return
       end if
    end do
    est_premier = .true.
    return 
  end function est_premier
end program listerprimes
