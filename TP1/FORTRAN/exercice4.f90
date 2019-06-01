program matrice
  implicit none
  integer, dimension(:,:) ,allocatable :: M
  integer :: nblin, nbcol, i , j , pballocation
  write (*, fmt = "('Taille de la matrice : ',/2X,'Nombre de lignes',2X)",advance = 'no')
  read *,nblin
  write (*, fmt = "(2X,'Nombre de colonnes',2X)",advance = 'no')
  read *,nbcol
  allocate ( M(nblin,nbcol),stat = pballocation  )
  if (pballocation .GT. 0) then
     stop "Erreur d'allocation"
  end if
  write (*,*)
  write (*, fmt = "(2X,'Merci de saisir (ligne par ligne) les entree de la mat :',2X)")
  
  do i= 1,nblin,1
     write (*, fmt = "(2X,'Ligne :',i4)") i
     do j = 1,nbcol,1
        write (*, fmt = "(2X,'col ',i4,':' )",advance = 'no') j
        read *,M(i,j)
     end do
  end do
   do i= 1,size(M,1),1
        write (*,*)
        do j = 1,size(M,2),1
           write (*, fmt = "(2X,i4 )",advance = 'no') M(i, j) 
        end do
     end do
     write (*,*)
  deallocate(M)
  stop "End matrice"
end program matrice
