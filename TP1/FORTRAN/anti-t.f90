function anit_t(M)
  integer,dimension(:,:),intent(in)::M
  integer,dimension(:,:),allocatable::anit_t
  integer :: aerr,i,j
  allocate(anit_t(size(M,1),size(M,1)), stat=aerr)
  if (aerr .NE. 0) print *, "ATM: Allocation request denied"

  do i = 1, size(M, dim=1), 1
    do j = 1, size(M, dim=1), 1
      anit_t(i,j) = M(size(M, dim=1)-j+1,size(M, dim=1)-i+1)
    end do
  end do


  return
end function anit_t
