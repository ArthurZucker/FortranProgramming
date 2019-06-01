program lexico
  implicit none
  integer,parameter :: etand1=5,etand2=5
  integer :: i
  integer, dimension(etand1) :: u 
  integer, dimension(etand2) :: v
  u=(/2,3,6,5,7/)
  v=(/2,3,6,5,5/)
  call system("clear")
  write(*,"('. Bienvenue dans le comparateur lexinf')")
  write(*,fmt = "(/2X,'Vecteur u : ',5i4 )") u ! definir le format avec la size du tableau 
  write(*,fmt = "(2X,'Vecteur v : ',5i4 /)") v
  write(*,fmt = "(2X,'[u lexinf v ?] ')",advance = 'no')
  if (is_lex(u,v)) then
     write (*,fmt = "('Oui')")
  else
     write (*,fmt = "('NON')")
  end if 
  stop "Fin lexorder"
contains
  function is_lex(u,v)
    logical :: is_lex
    integer, dimension(:),intent(in) :: u 
    integer, dimension(:),intent(in) :: v
    integer :: i = 1
    do while ( (u(i) .EQ. v(i)))
        i = i+1
    end do
    if (i .NE. size(u) .AND. u(i) .LE. v(i)) then
       is_lex = .true.
       return
    else
       is_lex = .false.
       return
    end if
    
  end function is_lex
end program lexico
