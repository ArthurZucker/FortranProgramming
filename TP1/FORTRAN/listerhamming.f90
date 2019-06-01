program listerhamming

  implicit none
  integer::N,i,j
  call system("clear")
  write(*,fmt="('. Bienvenue dans Lister Hamming')")
  write(*,fmt="(3X,'. Veuillez indiquer votre entier limite : ')",advance = 'no')
  read *,N
  write(*,fmt="(3X,'. [La liste]')")
  j = 1
  do i=1,N,1
     if (est_hamming(i)) then
        write(*,fmt="(3X,i4,3X,i4)")j,i
        j = j+1
     end if
  end do
  stop "Fin hamming..."
  
contains
  function est_hamming(P)
    logical::est_hamming
    integer,intent(in)::P
    integer :: i
    if (P==1) then
       est_hamming = .false.
       return
    end if
    do i =2,P,1
       if (mod(P,i)==0 .AND. est_premier(i)) then
          if ( i .NE. 2 .AND. i .NE. 3 .AND. i .NE. 5  )  then
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
end program listerhamming
