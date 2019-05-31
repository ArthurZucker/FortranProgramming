module clsFonction
  use clsIntervalle
  implicit none

  public :: Fonction

  type :: Fonction
     type(Intervalle), private :: int
     procedure(signature_fonction), pointer, nopass :: expr => Null()
   contains
     !--- Publics
     procedure, public :: valeur
  end type Fonction

  abstract interface
     real function signature_fonction(x)
       import Fonction
       real, intent(in) :: x
     end function signature_fonction
  end interface
contains

  real function valeur(this,x)
    class(Fonction), intent(in) :: this
    real, intent(in) :: x

    valeur = this%expr(x)
    return
  end function valeur

end module clsFonction
