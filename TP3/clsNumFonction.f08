! AZ
module clsNumFonction
  use clsIntervalle
  use clsFuncGrapher
  implicit none
  type ::  NumFonction
    type(Intervalle), private  ::  interval
    real , dimension (:), allocatable , private  ::  valeurs
    contains
    !--- Publics
    procedure , public  ::  getIntervalle , setIntervalle , getNbrValeurs
    generic , public  :: setValeurs => setValeurs_toutes , setValeurs_une
    generic , public  ::  getValeurs => getValeurs_toutes , getValeurs_une
    !-- Afficher l’intervalle  et si  disponible  les  valeurs  de lafonction
    procedure , public  :: infos
    !-- Representer  le  graphe  de la  fonction
    procedure , public  ::  dessiner
    !-- Echantillonner  une  fonction  donnee  par une  procedure ...
    generic , public  ::  echantillonner => echant_fonc_inter , echant_fonc
    !-- Operators
    generic , public  ::  assignment (=) => numfonc_recoit_numfonc
    !-- Private
    procedure , private  ::  printValeurs
    procedure , private  ::  getValeurs_toutes , getValeurs_une
    procedure , private  ::  setValeurs_toutes , setValeurs_une
    procedure , private  ::  numfonc_recoit_numfonc
    procedure , private  ::  echant_fonc_inter , echant_fonc
  end  type  NumFonction
contains
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-----------TESTERS>>>>>>>>>>>>>>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine test2()
    class(NumFonction),allocatable::pourtester,recoit
    type(Intervalle),allocatable::Interv,Interv2
    real,dimension(:),allocatable::testdevaleurs
    integer::i,err4
    write(*,"('---------------------------------------------------',/,'------------DEBUT TEST NumFonction-----------------')")
    write(*,"('---------------------------------------------------')")
    allocate(Interv, stat=err4)
    if ( err4 .NE. 0) print *, "Interv: Allocation request denied"
    allocate(Interv2, stat=err4)
    if ( err4 .NE. 0) print *, "Interv2: Allocation request denied"
    allocate(testdevaleurs(10),stat = err4)
    if ( err4 .NE. 0) print *, "testdevaleurs: Allocation request denied"
    do i = 1, 10, 1
      testdevaleurs(i) = i
    end do
    call Interv%setBorneInf(0.2)
    call Interv%setBorneSup(10.2)
    call Interv%subdiviser(10)
    allocate(pourtester, stat=err4)
    if (err4 .NE. 0) print *, "pourtester: Allocation request denied"
    allocate(recoit, stat=err4)
    if (err4 .NE. 0) print *, "pourtester: Allocation request denied"
    ! Test 1 set Intervalle
    write(*,"(7X,'Test 1 : test de setIntervalle et getIntervalle')")
    call pourtester%setIntervalle(Interv)
    Interv2 = pourtester%getIntervalle()
    call Interv2%printsubdivision()
    ! Test 2 set valeurs et getNbrValeurs
    write(*,"(7X,'Test 2 : getNbrvaleurs et setValeurs ')")
    call pourtester%setValeurs(testdevaleurs)
    call pourtester%infos()
    write(*,"('Get valeur()')")
    write(*,*)pourtester%getValeurs()
    write(*,"('Get valeur(k=2)')")
    write(*,*)pourtester%getValeurs(2)
    ! Test 3 numFonction recoit numFonction
    write(*,"(7X,'Test 3 numFonction recoit numFonction ')")
    recoit = pourtester
    call recoit%infos()
    if (allocated(pourtester)) deallocate(pourtester, stat=err4)
    if (err4 .NE. 0) print *, "pourtester: Deallocation request denied"
    call Interv%liberer()
    call Interv2%liberer()
    write(*,"('---------------------------------------------------',/,'------------FIN TEST NumFonction-------------------')")
    write(*,"('---------------------------------------------------')")
  end subroutine test2
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-----------GETTERS and SETTERS>>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  function getIntervalle(this)
    class(NumFonction),intent(in)::this
    type(Intervalle)::getIntervalle
    getIntervalle = this%interval
  end function getIntervalle

  subroutine setIntervalle(this,interv)
    class(NumFonction)::this
    type(Intervalle)::interv
    this%interval = interv
  end subroutine setIntervalle

  function getNbrValeurs(this)
    class(NumFonction),intent(in)::this
    real ::getNbrValeurs
      getNbrValeurs = size(this%valeurs,1)
  end function getNbrValeurs

  subroutine setValeurs_une(this,values,k)
    class(NumFonction)::this
    real,intent(in)::values
    integer::err,k
    if ( size(this%valeurs)<k) then
      allocate(this%valeurs(k), stat=err)
        this%valeurs(k) = values
    else
      this%valeurs(k) = values
    end if
  end subroutine setValeurs_une

  subroutine setValeurs_toutes(this,values)
    class(NumFonction)::this
    real,dimension(:),allocatable,intent(in)::values
    integer::err,i
    allocate(this%valeurs(size(values)), stat=err)
    if ( err .NE. 0) print *, ": Allocation request denied"
    do i = 1, size(values), 1
      this%valeurs(i) = values(i)
    end do
  end subroutine setValeurs_toutes

  function getValeurs_une(this,k)
    class(NumFonction),intent(in)::this
    real::getValeurs_une
    integer::k
    if ( allocated(this%valeurs) .AND. k .GE. 0 .AND. k .LE. size(this%valeurs) ) then
      getValeurs_une = this%valeurs(k)
    end if
  end function getValeurs_une

  function getValeurs_toutes(this)
    class(NumFonction),intent(in)::this
    real,dimension(:),allocatable::getValeurs_toutes
    integer::err,i
    if ( allocated(this%valeurs) ) then
      allocate(getValeurs_toutes(size(this%valeurs)), stat=err)
      if (err .NE. 0) print *, "getValeurs_toutes: Allocation request denied"
      do i = 1, size(this%valeurs), 1
        getValeurs_toutes(i)=this%valeurs(i)
      end do
    end if
  end function getValeurs_toutes
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-------------OPERATORS>>>>>>>>>>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine numfonc_recoit_numfonc(left,right)
    class(NumFonction),intent(in):: right
    class(NumFonction),intent(out)::left
    integer::err2
    left%Interval = right%Interval
    if ( allocated(left%valeurs)) then
      deallocate(left%valeurs,stat=err2)
        if ( err2 .NE. 0) print *, "valeur a droite: DeAllocation request denied"
    end if
    call left%setValeurs(right%valeurs)
    return
  end subroutine numfonc_recoit_numfonc
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-------------Echantillonner>>>>>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine echant_fonc_inter(this,Int,f)
    !Echantillone la fontion sur l'intervalle
    !selon le pas k
    implicit none

    interface
      function f(y)
        real, intent(in) :: y
        real :: f
      end function f
    end interface
    class(NumFonction)::this
    type(intervalle),intent(in)::Int
    integer::i
    call this%setIntervalle(Int)
    do i = 1, this%interval%getTailleSubdivision(), 1
      this%valeurs(i) = f(this%interval%getSubdivision(i))
    end do
  end subroutine echant_fonc_inter

  subroutine echant_fonc(this,f)
    implicit none
    interface
      function f(y)
        real, intent(in) :: y
        real :: f
      end function f
    end interface
    class(NumFonction)::this
    integer::i,err
    if ( allocated(this%valeurs) ) then
      deallocate(this%valeurs,stat = err)
      if ( err .NE. 0) print *, "this%valeurs: DeAllocation request denied"
    end if
    allocate(this%valeurs(this%interval%getTailleSubdivision()),stat=err)
    if ( err .NE. 0) print *, "this%valeurs: Allocation request denied"
    do i = 1, this%interval%getTailleSubdivision(), 1
      this%valeurs(i) = f(this%interval%getSubdivision(i))
    end do
  end subroutine echant_fonc
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-------------PRINTERS>>>>>>>>>>>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine infos(this)
    class(NumFonction),intent(in)::this
    write(*,"('Intervalle:')")
    call this%interval%print()
    call this%interval%printsubdivision()
    call this%printValeurs()
  end subroutine infos

  subroutine printValeurs(this)
    class(NumFonction),intent(in)::this
    integer::i
    if ( allocated(this%valeurs) ) then
      write(*,"('Valeurs : [')",advance='NO')
      do i = 1, size(this%valeurs), 1
        write(*,"('|',f20.6,'|')",advance='NO')this%valeurs(i)
      end do
    end if
    write(*,"(']')")
  end subroutine printValeurs
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-------------DRAWERRR>>>>>>>>>>>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine dessiner(this,title)
    class(NumFonction),intent(in)::this
    character(len=*), intent(in) :: title
    type(FuncGRAPHER)::FuncGRA
    call FuncGRA%plot(this%interval%getSubdivision(),this%valeurs,"data.txt",title,"x","y")
  end subroutine dessiner
end module clsNumFonction
