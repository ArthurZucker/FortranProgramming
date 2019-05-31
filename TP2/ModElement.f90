module ModElement
  use ModDonnee, printDonnee => print
  implicit none

  type Element
     type(Donnee), private :: info
     type(Element), pointer,private :: next 
  end type Element

  interface creerElement
     module procedure creer_element_vide, creer_element_donnee
  end interface creerElement

  interface operator(.EQ.)
     module procedure element_egal_element
  end interface operator(.EQ.)

  private :: creer_element_vide, creer_element_donnee
  private :: element_egal_element

contains 
  !<><><><><><><><><><><><><><><><><><><><>
  !< ERROR MESSAGES
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine ElementPointerNotAssociatedError(methodName, msg)
    implicit none
    character(len=*), intent(in) :: methodName
    character(len=*), optional, intent(in) :: msg

    write(*,"(//)")
    if( present(msg) ) then 
       write(*,"('***   Hints: ')",advance='no'); write(*,*) msg
    end if

    write(*,"('***  Method: ')", advance='no'); write(*,*) methodName
    stop "  Error: element pointer not associated "
    return  
  end subroutine ElementPointerNotAssociatedError

  subroutine tstElementAssociated(Pointeur)
    implicit none
    type(Element), pointer, intent(in) :: pointeur

    if(associated(Pointeur) )  then 
       print *, " debug: your pointer is associated ..."
    else
       print *, " debug: your pointer is NOT associated ..."
    end if

    return 
  end subroutine tstElementAssociated
  
  !<><><><><><><><><><><><><><><><><><><><>
  !< METHOD: OPERATOR EQ
  !<><><><><><><><><><><><><><><><><><><><>
  function element_egal_element(lhs,rhs)
    implicit none
    type(Element), intent(in) :: lhs, rhs
    logical :: element_egal_element 
    
    element_egal_element = lhs%info .EQ. rhs%info
    
    return 
  end function element_egal_element

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHOD: EMPTY
  !<><><><><><><><><><><><><><><><><><><><>
  function elementVide(Elem)
    implicit none
    type(Element), pointer, intent(in) :: Elem
    logical :: elementVide
    
    elementVide = associated(Elem)

    return 
  end function elementVide

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHOD: NEXT ELEMENT
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine elementSuivant(current, next)
    implicit none
    type(Element), pointer, intent(in) :: current
    type(Element), pointer :: next

    next => current % next
    return
  end subroutine elementSuivant

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHODS: MEMBERSHIP TESTS
  !<><><><><><><><><><><><><><><><><><><><>
  function possedeFils(Elem)
    implicit none
    type(Element), intent(in) :: Elem
    logical :: possedeFils
    
    possedeFils = associated( Elem % next)
    
    return 
  end function possedeFils

  function possedeDonnee(Elem, info)
    implicit none
    type(Donnee), intent(in) :: info
    type(Element), intent(in), pointer :: Elem
    logical :: possedeDonnee

    possedeDonnee = .false. 

    if(associated(Elem)) then 
       possedeDonnee = Elem % info .EQ. info
    else
       call ElementPointerNotAssociatedError("hasData")
    end if

    return 
  end function possedeDonnee

  function filsPossedeDonnee(Elem, info)
    implicit none
    type(Donnee), intent(in) :: info
    type(Element), intent(in), pointer :: Elem

    logical :: filsPossedeDonnee

    if( associated(Elem) ) then 
        if( associated( Elem%next ) ) then 
           filsPossedeDonnee = Elem%next%info .EQ. info
        else
           print *
           call ElementPointerNotAssociatedError("filsPossedeDonnee", "Aucun fils associe")
        end if
    else
       call ElementPointerNotAssociatedError("filsPossedeDonnee")
    end if

    return
  end function filsPossedeDonnee

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHODS: GETTERS
  !<><><><><><><><><><><><><><><><><><><><>
  function getFils(Elem)
    implicit none
    type(Element), pointer, intent(in) :: Elem
    type(Element), pointer :: getFils
    
    nullify(getFils) 

    if( associated(Elem) ) then 
       getFils => Elem % next
    else
       call ElementPointerNotAssociatedError("getFils")
    end if

    return
  end function getFils

  function getDonnee(Elem)
    implicit none
    type(Element), pointer, intent(in) :: Elem
    type(Donnee), pointer :: getDonnee

    if( associated(Elem) ) then 
       getDonnee => Elem%info
    else
       call ElementPointerNotAssociatedError("getDonnee")
    end if

    return
  end function getDonnee

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHODS: SETTERS
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine setFils(Pere, Fils)
    implicit none
    type(Element), pointer :: Pere
    type(Element), pointer, intent(in) :: Fils
    
    if( associated(Pere) )then 
       Pere % next => Fils
    else
       call ElementPointerNotAssociatedError("setFils","Aucun pere trouve")
    end if
 
    return 
  end subroutine setFils

  subroutine setElement(Elem,info)
    implicit none
    type(Element),pointer :: Elem
    type(Donnee), intent(in) :: info

    ! Notice that the assignment operator 
    ! was implemented in ModDonnee (nice thing to do)
    if( associated(Elem) )then 
       Elem % info = info 
    else
        call ElementPointerNotAssociatedError("setElement")
    end if

    nullify(Elem % next)

    return 
  end subroutine setElement

  subroutine nullifyFils(Elem)
    implicit none
    type(Element),pointer :: Elem
        
    if( associated(Elem) )then 
       nullify(Elem % next)
    else
        call ElementPointerNotAssociatedError("nullifyFils")
    end if

    return
  end subroutine nullifyFils
  
  !<><><><><><><><><><><><><><><><><><><><>
  !< CONSTRUCTORS
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine creer_element_vide(Elem)
    implicit none
    type(Element), pointer :: Elem
    integer :: alloc_status

    allocate(Elem, stat=alloc_status)

    if( alloc_status > 0) then 
       stop "Erreur: impossible d'allouer une nouvel element "
    end if
    
    return
  end subroutine creer_element_vide

  subroutine creer_element_donnee(Elem, newData)
    implicit none
    type(Element), pointer :: Elem
    type(Donnee), intent(in) :: newData
    
    call creer_element_vide(Elem)
    call setElement(Elem, newData)
    
    return 
  end subroutine creer_element_donnee
  
  !<><><><><><><><><><><><><><><><><><><><>
  !< METHODS: USEFUL
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine printElement(Elem,msg)
    implicit none
    type(Element), pointer,intent(in) :: Elem
    character(len=*), optional :: msg
    
    if( associated(Elem) ) then 
       if( present(msg) ) then 
          print *, msg
       end if
       call printDonnee( getDonnee(Elem) ) 
    end if
   
    write(*,*)
    return 
  end subroutine printElement

end module ModElement
