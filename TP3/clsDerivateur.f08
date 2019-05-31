! AZ
module clsDerivateur
  use clsIntervalle
  use clsNumFonction
  implicit none
  type Derivateur
  contains
    procedure , public  ::  deriver
  end type Derivateur
contains
  function deriver(this , f, I)
    class(Derivateur), intent(in) :: this
    type(Intervalle) :: I
    interface
      real function f(x)
      real , intent(in) :: x
      end  function f
    end interface
    type(NumFonction),allocatable ::  deriver
    real,dimension(:),allocatable::vals
    integer::j,err
    allocate(deriver)
    allocate(vals(I%getTailleSubdivision()), stat=err)
    if (err .NE. 0) print *, "vals: Allocation request denied"
    call deriver%setIntervalle(I)
    do j = 1, I%getTailleSubdivision() -1, 1
      !(f(I%getSubdivision(j+1))-f(I%getSubdivision(j)))/(I%getSubdivision(j+1)-I%getSubdivision(j))
      vals(j+1) = (f(I%getSubdivision(j+1))-f(I%getSubdivision(j)))/(I%getSubdivision(j+1)-I%getSubdivision(j))
    end do
    call deriver%setValeurs(vals)
    if (allocated(vals)) deallocate(vals, stat=err)
    if (err .NE. 0) print *, "vals: Deallocation request denied"
  end function deriver
end module clsDerivateur
