!
! (c) Arthur Zucker, Sorbonne Universite France
!
module ModListe
  use ModElement

  implicit none

  type Liste
     integer, private :: size
     type(Element), pointer, private :: head
  end type Liste

  interface creerListe
     module procedure creer_liste_vide, creer_liste_element, creer_liste_donnee, creer_liste_liste
  end interface creerListe

  interface chercherListe
     module procedure localiser_donnee, contient_donnee
  end interface chercherListe

  private :: setSize
  private :: localiser_donnee, contient_donnee
  private :: creer_liste_vide, creer_liste_element, creer_liste_donnee, creer_liste_liste

contains

  subroutine todo(msg)
    implicit none
    character(len=*), intent(in) :: msg

    write(*,"(/,'Not implemened yet --> procdeure : ')", advance='no'); write(*,*) msg
    stop "You have a todo to do ... "
    return
  end subroutine todo
  !<><><><><><><><><><><><><><><><><><><><>
  !<  CONSTRUCTORS
  !<><><><><><><><><><><><><><><><><><><><>
  function vide(argListe)
    implicit none
    type(Liste), intent(in) :: argListe
    logical :: vide
    if ( getSize(argListe)==0 ) then
      vide = .TRUE.
    else
      vide = .FALSE.
    end if
    return
  end function vide

  subroutine creer_liste_vide(argListe)
    implicit none
    type(Liste),intent(in) :: argListe
    call nullifyHead(argListe)
    call setSize(argListe, 0)
    return
  end subroutine creer_liste_vide

  subroutine creer_liste_element(argList, elem)
    type(Liste) :: argList
    type(Element),pointer, intent(in) :: elem

    stop " Error: not implemented "
    return
  end subroutine creer_liste_element

  subroutine creer_liste_donnee(argList,newData)
     type(Liste) :: argList
     type(Donnee), pointer, intent(in) :: newData

    stop " Error: not implemented "
    return
  end subroutine creer_liste_donnee

  subroutine creer_liste_liste(argList, lst)
    type(Liste) :: argList
    type(Liste), pointer, intent(in) :: lst

    stop " Error: not implemented "
    return
  end subroutine creer_liste_liste

  !<><><><><><><><><><><><><><><><><><><><>
  !<  METHOD: ADD
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine ajouter(argList,newData)
    implicit none
    type(Donnee), intent (in) :: newData
    type(Liste) :: argList
    type(Element),pointer :: currentElem,lastElement,temp
    logical :: pFils
    integer :: taille
    call creerElement(lastElement,newData)
    call nullifyFils(lastElement)
    if ( vide(argList) ) then
      call setHead(argList,lastElement)
      call setSize(argList,1)
    else
      currentElem => getHead(argList)
      do while ( possedeFils(currentElem) )
        call elementSuivant(currentElem,currentElem)
      end do
      call setFils(currentElem,lastElement)
      taille = getSize(argList)+1
      call setSize(argList,taille)
    end if
    return
  end subroutine ajouter

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHOD: DESTROY
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine supprimer(argList)
    implicit none
    type(Liste) :: argList
    type(Element), pointer :: nextElem, currentElem
    if ( .NOT. vide(argList) ) then
      currentElem => getHead(argList)
      do while (getSize(argList)>0)
        do while ( elementVide(currentElem) )
          call elementSuivant(currentElem,currentElem)
        end do
        call setSize(argList,getSize(argList)-1)
        nullify(currentElem)
      end do
    end if
    return
  end subroutine supprimer


  !<><><><><><><><><><><><><><><><><><><><>
  !< METHOD: PRINT
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine printListe(argList)
    implicit none
    type(Liste), intent(in) :: argList
    type(Element), pointer :: currentElem,temp
    if ( vide(argList) ) then
      write(*,"(7X,'!!Liste vide!!')")

    else
      write(*,"(7X,'should print')")
      currentElem => getHead(argList)
      do while ( elementVide(currentElem) )
        call printElement(currentElem)
        call elementSuivant(currentElem,temp)
        currentElem => temp
      end do
    end if

    return
  end subroutine printListe

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHOD: FIRST
  !<><><><><><><><><><><><><><><><><><><><>
  function first(arglist)
    implicit none
    type(Liste), intent(in) :: argList
    type(Element), pointer :: first

    first => getHead(argList)

    return
  end function first

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHODS: FIND
  !<><><><><><><><><><><><><><><><><><><><>
  function contient_donnee(argList, info)
    implicit none
    type(Donnee), intent (in) :: info
    type(Liste) :: argList
    logical :: contient_donnee
    type(Element),pointer::currentElem
    contient_donnee = .FALSE.
    if ( vide(argList) ) then
      write(*,"(7X,'!!Liste vide; la donnee ne peut etre dedans!!')")
    else
      currentElem => getHead(argList)
      do while ( elementVide(currentElem)  )
        if ( getDonnee(currentElem) .EQ. info ) then
          contient_donnee = .TRUE.
        end if
        call elementSuivant(currentElem,currentElem)
      end do
    end if
    return
  end function contient_donnee

  function localiser_donnee(argList,info,prev,elem)
    implicit none
    type(Liste) :: arglist
    type(Donnee), intent(in) :: info
    type(Element), pointer :: elem, next, prev

    logical :: localiser_donnee
    localiser_donnee = .FALSE.
    if ( vide(argList) ) then
      write(*,"(7X,'!!Liste vide; la donnee ne peut etre dedans!!')")
    else
      elem => getHead(argList)
      do while ( possedeFils(elem) .AND. .NOT. localiser_donnee)
        if ( getDonnee(elem) .EQ. info ) then
          localiser_donnee = .TRUE.

        else
          prev => elem
          call elementSuivant(elem,elem)
          if ( elementVide(elem) .AND.  getDonnee(elem) .EQ. info) then
            localiser_donnee = .TRUE.
          end if
        end if

      end do
    end if
    return
  end function localiser_donnee

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHOD: REMOVE
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine remove(argList, info)
    type(Liste) :: argList
    type(Donnee) :: info
    type(Element), pointer :: elem, prev
    logical :: logi
    logi = chercherListe(argList,info,prev,elem)
    if ( .NOT. logi )then
      write(*,"('Element non trouve dans la liste, ne peut etre suprimé')")
    else
      call elementSuivant(elem,elem)
      call setFils(prev,elem)
    end if
    return
    end subroutine remove

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHODS: GETTERS
  !<><><><><><><><><><><><><><><><><><><><>
  function getSize(argListe)
    implicit none
    type(Liste), intent(in) :: argListe
    integer :: getSize

    getSize = argListe%size
    return
  end function getSize

  function getHead(argListe)
    type(Liste), intent(in) :: argListe
    type(Element), pointer :: getHead

    getHead => argListe % head
    return
  end function getHead

 !<><><><><><><><><><><><><><><><><><><><>
  !< METHODS: SETTERS
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine setSize(argListe,valeur)
    implicit none
    integer, intent(in) :: valeur
    type(Liste) :: argListe

    argListe%size = valeur

    return
  end subroutine setSize

  subroutine setHead(argListe, Elem)
    implicit none
    type(Liste) :: argListe
    type(Element), pointer, intent(in) :: Elem

    if( .NOT. associated( Elem ) ) then
       call ElementPointerNotAssociatedError("setHead")
    else
       argListe % head => Elem
    end if

    return
  end subroutine setHead

  subroutine nullifyHead(argList)
    implicit none
    type(Liste) :: argList

    nullify(argList % head)

    return
  end subroutine nullifyHead

end module ModListe
