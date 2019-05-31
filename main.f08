! AZ
program main
  use clsFuncGrapher
  use clsIntervalle , testInter => test
  use clsNumFonction, testNumFonction => test2
  use clsDerivateur
  implicit none
  type(Intervalle) ::  interval
  type(NumFonction) :: Df, f
  type(Derivateur) ::  FWEuler
  integer , parameter  ::  nbrPoints = 1000
  call system("clear")
  call testInter()
  call testNumFonction()
  call  interval%setBorneinf (0.)
  call  interval%setBornesup (15.)
  call  interval%subdiviser(nbrPoints)
  !call  interval%printsubdivision()
  !--- La  fonction  ...
  call f%setIntervalle( interval )
  call f%echantillonner( x2cosx )
  call f%dessiner("Graphe  de x-->x2cosx")
  !--- Calculer  la  derivee  ...
  Df = FWEuler%deriver(x2cosx ,interval)
  call Df%dessiner("Graphe  derivee  de x-->x2cosx")
  stop "Fin programme main"
contains
  function x2cosx(x)
    real, intent(in) :: x
    real :: x2cosx
    x2cosx = x*x*cos(x)
  end function x2cosx
end program main
