!
! (c) Hacene Ouzia, UPMC France
!
program listes
  use ModListe
  implicit none

  type(Donnee) :: D1,D2,D3,D4
  type(Liste) :: lst
  type(Element),pointer::e1,e2
  call system("clear")

  call setData()


  write(*,"(/,7x, 'TP-5 :: Tester Program liste ... ',/)")


  ! 1. CreerListe
  write(*,"(/,7x, 'Test 1 :: Creation de la liste et affichage ... ',/)")
  call creerListe(lst)
  call printListe(lst)
  ! 2. Ajouter des elements à la liste ...
  write(*,"(/,7x, 'Test 2 ::Ajout ... ',/)")
  call ajouter(lst,D1)
  call printListe(lst)
  call ajouter(lst,D2)
  call printListe(lst)
  call ajouter(lst,D3)
  call printListe(lst)
  call ajouter(lst,D4)
  call printListe(lst)
  ! 3. Afficher la liste courante

  ! 4. Est-ce D1 est dans la liste ?
  write(*,"(/,7x, 'Test 3 ::Est-ce D1 est dans la liste ? ;',' ... ',/)")
  if ( chercherListe(lst,D1) ) then
    write(*,"(/,9x, 'Oui',/)")
  end if

  ! 5. Est-ce D4 est dans la liste ?
  write(*,"(/,7x, 'Test 4 ::Quelle est l adresse de D4 dans la liste ? ;',' ... ',/)")
  if ( chercherListe(lst,D4,e1,e2) ) then
    write(*,"(/,9x,'Oui',/)")
  end if
  ! 6. Supprimer D2 de la liste
  write(*,"(/,7x, 'Test 5 ::remove  D2;',' ... ',/)")
  call remove(lst,D2)
  call printListe(lst)
  ! 7. Supprimer D4 de la liste
  write(*,"(/,7x, 'Test 6 ::remove D4 ;',' ... ',/)")
  call remove(lst,D3)
  call printListe(lst)
  ! 8. Afficher la liste

  ! 9. Vider la liste
  write(*,"(/,7x, 'Test 3 ::Supprimer la liste ... ',/)")
  call supprimer(lst)
  call printListe(lst)
  write(*,*)
  stop "Listes chainees"
contains

  subroutine setData()

    call setNumero(D1,1)
    call setNumero(D2,2)
    call setNumero(D3,3)
    call setNumero(D4,2)

    call setPoids(D1, 2.75)
    call setPoids(D2, 4.76)
    call setPoids(D3, 6.89)
    call setPoids(D4, 3.67)

    return
  end subroutine setData

end program listes
