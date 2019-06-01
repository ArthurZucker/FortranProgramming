program hamming

  implicit none

  integer::P
  call system("clear")
  write(*,fmt="('. Bienvenue dans est Hamming')")
  write(*,fmt="(3X,'. Veuillez indiquer votre entrée : ')",advance = 'no')
  read *,P
  if (.NOT. est_hamming(P)) then
     write(*,fmt="(3X,'[Est hamming ?] : NON')")
  else
     write(*,fmt="(3X,'[Est hamming ?] : OUI')")
  end if 


  stop "Fin hamming..."
  
contains
  function est_hamming(P)
    logical::est_hamming
    integer,intent(in)::P
    integer :: i
    do i =2,P-1,1
       if (mod(P,i)==0 .AND. est_premier(i)) then
          if ( i .NE. 2 .OR. i .NE. 3 .OR. i .NE. 5  )  then
             est_hamming = .false.   
             return
          end if
       end if
    end do 
    est_hamming = .true.
    return 
  end function est_hamming
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
end program hamming
