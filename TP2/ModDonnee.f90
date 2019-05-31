module ModDonnee
  implicit none

  type Donnee
     integer, private :: numero
     real, private :: poids
  end type Donnee

  interface assignment(=)
     module procedure donnee_recoit_donnee
  end interface assignment(=)

  interface operator(.EQ.)
     module procedure donnee_egale_donnee
  end interface operator(.EQ.)

  private :: donnee_egale_donnee
  private :: donnee_recoit_donnee

contains
  !<><><><><><><><><><><><><><><><><><><><>
  !< OPERATOR EQ
  !<><><><><><><><><><><><><><><><><><><><>
  function donnee_egale_donnee(lhs,rhs)
    implicit none
    type(Donnee), intent(in) :: lhs, rhs
    logical :: donnee_egale_donnee
    logical :: numero_identique, poids_identique

    numero_identique = lhs%numero .EQ. rhs%numero
    poids_identique = lhs%poids .EQ. rhs%poids

    donnee_egale_donnee = numero_identique .AND. poids_identique

    return
  end function donnee_egale_donnee

  !<><><><><><><><><><><><><><><><><><><><>
  !< OPERATOR =
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine donnee_recoit_donnee(lhs,rhs)
    implicit none
    type(Donnee), intent(in) :: rhs
    type(Donnee), intent(out) :: lhs

    lhs % numero = rhs % numero
    lhs % poids = rhs % poids

    return 
  end subroutine donnee_recoit_donnee

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHODS: GETTERS
  !<><><><><><><><><><><><><><><><><><><><>
  function getNumero(arg)
    implicit none
    type(Donnee), intent(in) :: arg
    integer :: getNumero
    
    getNumero = arg%numero
    return 
  end function getNumero

  function getPoids(arg)
    implicit none
    type(Donnee), intent(in) :: arg
    real :: getPoids

    getPoids = arg%poids

    return 
  end function getPoids

  subroutine setNumero(arg, valeur)
    implicit none
    type(Donnee) :: arg
    integer, intent(in) :: valeur
    
    arg%numero = valeur
    return 
  end subroutine setNumero

  subroutine setPoids(arg,valeur)
    implicit none
    type(Donnee) :: arg
    real, intent(in) :: valeur

    arg%poids = valeur
    return 
  end subroutine setPoids

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHOD: Print
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine print(arg)
    implicit none 
    type(Donnee), intent(in) :: arg
    character(len=*), parameter :: UsedFormat="(3X,'Nume = ',I3,2X,'Poids = ', F5.2)"

    write(*,FMT=UsedFormat) getNumero(arg),getPoids(arg)

    return 
  end subroutine print

end module ModDonnee
