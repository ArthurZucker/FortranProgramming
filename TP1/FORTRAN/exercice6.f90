program anti_transposeur

  implicit none

  interface
    subroutine afficher_mat(Matrice)
      implicit none
      integer,intent(in),dimension(:,:)::Matrice
      integer ::i,j
    end subroutine afficher_mat
    function anit_t(M)
      integer,dimension(:,:),intent(in)::M
      integer,dimension(:,:),allocatable::anit_t
      integer :: aerr,i,j
    end function anit_t
  end interface

  integer :: ordre,i,j,malloc,dealloc
  integer, dimension(:,:),allocatable::M,ATM

  call system("Clear")

  write(*, fmt="('Bienvenue dans anti-transposeur version 1.0 : ',/2X,'Ordre de la matrice : ')",advance='NO')
  read *,ordre
  allocate(M(ordre,ordre), stat=malloc)
  if ( malloc .NE. 0) print *, "M: Allocation request denied"

  do i =1,ordre,1
      do j = 1, ordre, 1
        M(i,j) = floor(rand()*49 +1)
      end do
  end do
  ! inclure la procédure afficher matrice
  write(*,fmt="(3X,'.La matrice'))")
  call afficher_mat(M)
  ATM = anit_t(M)
  write(*,fmt="(3X,'.Sa transpose matrice'))")
  call afficher_mat(ATM)
  if (allocated(M)) deallocate(M, stat=dealloc)
  if ( dealloc .NE. 0) print *, "M: Deallocation request denied"
  if (allocated(ATM)) deallocate(ATM, stat=dealloc)
  if ( dealloc .NE. 0) print *, "M: Deallocation request denied"
  stop "FIN anti-transpose"
end program anti_transposeur
