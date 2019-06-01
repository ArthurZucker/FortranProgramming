program Primes

  implicit none

  integer::P
  call system("clear")
  write(*,fmt="('. Bienvenue dans est premier')")
  write(*,fmt="(3X,'. Veuillez indiquer votre entrée : ')",advance = 'no')
  read *,P
  if (.NOT. est_premier(P)) then
     write(*,fmt="(3X,'[Est premier ?] : NON')")
  else
     write(*,fmt="(3X,'[Est premier ?] : OUI')")
  end if 


  stop "Fin primes..."
  
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
end program Primes
