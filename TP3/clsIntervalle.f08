! AZ
module clsIntervalle
  implicit none
  type Intervalle
    real,private::binf
    real,private::bsup  !bsup>binf
    real,dimension(:),allocatable,private :: subdivison
  contains
    procedure , public  ::  getBorneInf , getBorneSup
    procedure , public  ::  setBorneInf , setBorneSup
    !-- Possibilites  de  recuperer  toute la  subdivision , une  section  de
    !celles -ci ou un point  particulier  indique  par son  indice
    generic , public  ::  getSubdivision => getsubdivision_toute ,getsubdivision_section , getSubdivision_k
    procedure , public  ::  getTailleSubdivision
    !-- Afficher l’intervalle
    procedure , public  :: print
    !-- Afficher  la  subdivision
    procedure , public  ::  printsubdivision
    !-- Subdiviser l’intervalle  en  indiquant  le  nombre  de  points.
    generic , public  ::  subdiviser => subdiv_taille
    procedure , public  ::  liberer
    !--- Operators
    generic , public  ::  assignment (=) => intervalle_recoit_intervalle
    !--- Private
    procedure , private  ::  verifierBornes , bornesok
    procedure , private  ::  getsubdivision_toute , getsubdivision_section ,getSubdivision_k
    procedure , private  ::  subdiv_taille
    procedure , private  ::  intervalle_recoit_intervalle
  end type Intervalle
contains
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-----------TESTERS------------>>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine test()
    class(Intervalle),allocatable::Interv,Interv2
    real,dimension(:),allocatable::subdivision
    integer :: alocerr,dealerr,taille
    allocate(Interv, stat=alocerr)
    if ( alocerr .NE. 0) print *, "Interv: Allocation request denied"
    allocate(subdivision(10), stat=alocerr)
    if ( alocerr .NE. 0) print *, "subdivison: Allocation request denied"
    !Test des fonctions implementees
    write(*,"('---------------------------------------------------',/,'------------DEBUT TEST Intervalle -----------------')")
    write(*,"('---------------------------------------------------')")
    !Test 1
    write(*,"(7X,'Test 1 : test des setters et getters')")
    call Interv%setBorneInf(0.2)
    call Interv%setBorneSup(10.2)
    call Interv%print()
    ! Test 2 Subdivision
    write(*,"(7X,'Test 2 : test des subdivision')")
    call Interv%subdiviser(10)
    subdivision = Interv%getSubdivision()
    call Interv%printsubdivision()
    taille = Interv%getTailleSubdivision()
    write(*,"('Taille de la subdivision :',i4)")taille-1
    ! Test 3 Liberer
    write(*,"(7X,'Test 3 : Liberer')")
    call Interv%liberer()
    ! Test 4 Affectation
    write(*,"(7X,'Test 4 : Affectation')")
    allocate(Interv2)

    Interv2 = Interv
    write(*,"('Intervalle 2 affecté avec intervalle 1')")
    call Interv2%print()
    call Interv2%printsubdivision()

    ! Test 5 verifier bornes??
    write(*,"(7X,'Test 5 : check')")
    if ( Interv%verifierBornes() ) then
      write(*,"('Bonnes bornes')")
    end if

    write(*,"('---------------------------------------------------',/,'--------------FIN TEST Intervalle -----------------')")
    write(*,"('---------------------------------------------------')")
    if (allocated(Interv)) deallocate(Interv, stat=dealerr)
    if ( dealerr .NE. 0) print *, "Interv: Deallocation request denied"
  end subroutine test
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-----------GETTERS and SETTERS>>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  function getBorneInf(this)
    implicit none
    class(Intervalle),intent(in)::this
    real :: getBorneInf
    getBorneInf = this%binf
  end function getBorneInf

  function getBorneSup(this)
    class(Intervalle),intent(in)::this
    real :: getBorneSup
    getBorneSup = this%bsup
  end function getBorneSup

  subroutine  setBorneInf(this,borne)
    class(Intervalle)::this
    real,intent(in)::borne
    this%binf = borne
  end subroutine setBorneInf

  subroutine  setBorneSup(this,borne)
    class(Intervalle)::this
    real,intent(in)::borne
    this%bsup = borne
  end subroutine setBorneSup

  function getsubdivision_toute(this)
    class(Intervalle),intent(in)::this
    real,dimension(:),allocatable :: getsubdivision_toute
    getsubdivision_toute = this%subdivison
  end function getsubdivision_toute

  function getsubdivision_section(this,sec1,sec2)
    class(Intervalle),intent(in)::this
    real,dimension(:),allocatable :: getsubdivision_section
    integer::sec1,sec2
    getsubdivision_section = this%subdivison(sec1:sec2)
  end function getsubdivision_section

  function getsubdivision_k(this,k)
    class(Intervalle),intent(in)::this
    integer::k
    real:: getsubdivision_k
    getsubdivision_k = this%subdivison(k)
  end function getsubdivision_k

  function getTailleSubdivision(this)
    class(Intervalle),intent(in)::this
    integer:: getTailleSubdivision
    getTailleSubdivision = size(this%subdivison)
  end function getTailleSubdivision
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-----------Checkers>>>>>>>>>>>>>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  function verifierBornes(this)
    class(Intervalle), intent(in)::this
    logical::verifierBornes
    if ( this%binf > this%bsup ) then
      verifierBornes = .FALSE.
    else
      verifierBornes = .TRUE.
    end if
  end function verifierBornes

  function bornesOk(this,k)
    class(Intervalle), intent(in)::this
    integer::k
    logical :: bornesOk
    if ( k>this%binf .AND. k<this%bsup ) then
      bornesok = .TRUE.
    else
      bornesOk = .FALSE.
    end if
  end function bornesOk

  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-----------SUBDIVISION>>>>>>>>>>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine subdiv_taille(this,nbpoint)
    class(Intervalle):: this
    integer,intent(in)::nbpoint
    integer::erro,i
    real::pas
    if ( .NOT. allocated(this%subdivison) ) then
      allocate(this%subdivison(nbpoint+1), stat=erro)
      if (erro .NE. 0) print *, "this%subdivison: Allocation request denied"
    end if
    this%subdivison(1) = this%binf
    pas = (this%bsup -this%binf )/nbpoint
    do i = 2, nbpoint+1, 1
      this%subdivison(i) = this%subdivison(i-1) + pas
    end do
  end subroutine subdiv_taille
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-----------OPERATORS>>>>>>>>>>>>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine intervalle_recoit_intervalle(left,right)
    class(Intervalle),intent(in):: right
    class(Intervalle),intent(out)::left
    left%binf = right%binf
    left%bsup = right%bsup
    call left%subdiviser(size(right%subdivison)-1)
    return
  end subroutine intervalle_recoit_intervalle
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-----------PRINT>>>>>>>>>>>>>>>>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine print(this)
    class(Intervalle),intent(in)::this
    write(*,"(4X,'binf =',f8.4,/4X,'bsup = ',f8.4)")this%binf,this%bsup
  end subroutine print
  subroutine printsubdivision(this)
    class(Intervalle),intent(in)::this
    integer::i
    write(*,"('[')",advance = 'NO')
    do i = 1, size(this%subdivison)-1, 1
      write(*,"(f8.4,',')",advance = 'NO')this%subdivison(i)
    end do
    write(*,"(f8.4,']',/)",advance = 'NO')this%subdivison(size(this%subdivison))
  end subroutine printsubdivision
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !-----------FREERS------------->>>>>>>>>>>
  !<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine liberer(this)
    class(Intervalle)::this
    if ( allocated(this%subdivison) ) then
      deallocate(this%subdivison)
    end if
  end subroutine liberer
end module clsIntervalle
