!(c) Hacène Ouzia, UPMC 2013
module clsFuncGrapher

  implicit none

  public :: FuncGRAPHER

  type FuncGRAPHER
     ! No attribute
   contains
     procedure, public :: plot

     ! Méthodes privées
     procedure, nopass, private :: exportFunc, gnuplot
  end type FuncGRAPHER

contains
  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
  ! Ecrire dans un fichier les valeurs x, fx. Le fichier sera exploitable avec Gnuplot
  subroutine plot(this,X,Fx,nomFichierDonnees,TitreGraphique,LegendeAxeX,LegendeAxeY)
    implicit none
    class(FuncGRAPHER) :: this
    real, dimension(:), intent(in) :: Fx
    real, dimension(:), intent(in) :: x
    character(len=*), intent(in) :: nomFichierDonnees,TitreGraphique,LegendeAxeX,LegendeAxeY

    call exportFunc(x,Fx,nomFichierDonnees)
    call gnuplot(nomFichierDonnees,TitreGraphique,LegendeAxeX,LegendeAxeY)

    return
  end subroutine plot

  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
  ! Cette procédure génère le script qui sera utilisé par gnuplot et tracer la figure

  subroutine gnuplot(nomFichierDonnees,titreGraphe,legendeAxeX,legendeAxeY)
    implicit none

    character(len=*), intent(in) :: nomFichierDonnees
    character(len=*), intent(in)  :: titreGraphe,legendeAxeX,legendeAxeY

    character(len=*), parameter :: nomGnuScript="Gnuplot.gnu"
    integer, parameter :: numFichier = 27
    integer :: Ko

    open(file=nomGnuScript,Unit=numFichier,iostat=Ko)

    if(Ko.GT.0) stop "Erreur : probleme avec le fichier ... "

    call addComment("(c) Hacène Ouzia, UPMC 2013")
    call addComment("Script Gnuplot ... ")

    write(numFichier,*)

    call setOption("title",titreGraphe)
    call setOption("xlabel",legendeAxeX)
    call setOption("ylabel",legendeAxeY)

    call setOption("grid")

    write(numFichier,*)

    call cmdPlot("title 'f(x)' with lines")

    write(numFichier,*)
    call gnuclose()

    close(NumFichier)

    call tracer()

    return
  contains
    subroutine tracer()
      call system("gnuplot "//nomGnuScript)
      return
    end subroutine tracer

    subroutine gnuclose()
      write(numFichier,*) "pause -1"
      write(numFichier,*) "quit"
      return
    end subroutine gnuclose

    subroutine cmdPlot(options)
      character(len=*), intent(in) :: options

      write(numFichier, *) 'plot "'//nomFichierDonnees//'" '//options
      return
    end subroutine cmdPlot

    subroutine addComment(Commentaire)
      character(len=*), intent(in) :: Commentaire

      write(numFichier, *) '# '//Commentaire
      return
    end subroutine addComment

    subroutine setOption(option,valeur)
      character(len=*), intent(in) :: option
      character(len=*), optional :: valeur

      if(Present(valeur) ) then
         write(numFichier, *) 'set '//option//' "'//valeur//' "'
      else
         write(numFichier, *) 'set '//option
      end if

      return
    end subroutine setOption
  end subroutine gnuplot

  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  ! Exporter vers un fichier nommé nomFichier les valeurs x et Fx
  subroutine exportFunc(x,Fx,nomFichierDonnees)
    implicit none
    real, dimension(:), intent(in) :: Fx
    real, dimension(:), intent(in) :: x
    character(len=*), intent(in) :: nomFichierDonnees

    integer, parameter :: numFichier = 56
    integer :: i, Nx, Nfx
    logical :: fichierExiste

    inquire(file=nomFichierDonnees,exist=fichierExiste)

    if(fichierExiste)then
       open(file=nomFichierDonnees,Unit=numFichier,status='old')
    else
       open(file=nomFichierDonnees,Unit=numFichier,status='new')
    end if

    Nx = size(X); Nfx = size(Fx)

    if(Nx.NE.Nfx) stop "Erreur : x et fx doivent avoir la meme taille ... "

    do i=1, Nx
       write(numFichier,*) X(i),Fx(i)
    end do

    close(NumFichier)

    return
  end subroutine exportFunc
end module clsFuncGrapher
