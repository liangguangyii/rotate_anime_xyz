module fileIO
    
    implicit none
    
    contains
    
    subroutine readxyzs(filename, elements, xyzcoords)
        character(len=200), intent(in) :: filename
        character(len=2), allocatable, intent(out):: elements(:)
        real(kind=8), allocatable, intent(out):: xyzcoords(:, :)
        
        character:: element*2, ctemp*2
        
        
        integer:: natoms, nlines, nsteps, ierr, i, istep, iatom
        
        real(kind=8):: rtemp
        
        
        open(unit=10, file=filename, status='old', action='read')
        
        !read the number of atoms
        read(10,*) natoms
        
        read(10,*)
        
        allocate(elements(natoms))
        do i = 1, natoms
          read(10,"(A2,3f18.10)") elements(i), rtemp, rtemp, rtemp
        end do
        
        rewind(10)
        
        !read the number of lines in the file
        !NOTE: the read function in Fortran will not read the last line of a file
        !if it does not end with a newline character
        nlines = 0
        do
          read(10, *, iostat=ierr)
          if (ierr /= 0) exit
          nlines = nlines + 1
        end do
        rewind(10)
        
        nsteps = nlines/(natoms+2)
        
        allocate(xyzcoords(natoms*nsteps, 3))
        
        do istep = 1, nsteps
            read(10,*)
            read(10,*)
            do iatom = 1, natoms
                read(10,"(A2,3f18.10)") ctemp, xyzcoords((istep-1)*natoms+iatom, :)
            end do
        end do
        
        close(10)
        
    end subroutine readxyzs

end module fileIO
