subroutine afficher_mat(M)
  implicit none
  integer,intent(in),dimension(:,:)::M
  integer ::i,j
  do i= 1,size(M,1),1
          write (*,*)
          do j = 1,size(M,2),1
             write (*, fmt = "(2X,i4 )",advance = 'no') M(i, j)
          end do
       end do
       write (*,*)
  return
end subroutine afficher_mat
