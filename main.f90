program main
    use fileIO
    use rotatexyz
    
    implicit none
    
    integer:: natoms, nsteps, dim, atomindex1, atomindex2
    integer:: istep, iatom
    character(len=200):: filename
    character(len=2), allocatable:: elements(:)
    real(kind=8), allocatable:: xyzcoords(:, :), xyztemp(:, :)
    
    real(kind=8), dimension(3):: veca, vecb, veca0, vecb0
    
    filename = 'pos.xyz'
    atomindex1 = 8      ! apex of pyramid -> (0,0,z)
    atomindex2 = 10     ! Pt atoms -> (x,0,z)
    
    veca0 = (/ 0D0, 0D0, 1D0 /)
    vecb0 = (/ 1D0, 0D0, 0D0 /)
    
    
    
    call readxyzs(filename, elements, xyzcoords, natoms, nsteps)
    
    dim = natoms*nsteps
    
    
    allocate(xyztemp(3, natoms))
    do istep = 1, nsteps
        
        !veca -> veca0
        veca(1:3) = xyzcoords(1:3, (istep-1)*natoms + atomindex1)
        xyztemp = xyzcoords(:, (istep-1)*natoms + 1: istep*natoms)
        call rotate_a2b(natoms, veca, veca0, xyztemp)
        
        !vecb -> vecb0
        !MUST BASED on xyztemp, i.e. the coordinates after veca rotation!!
        ! not considerding xy components
        vecb(1:3) = xyztemp(1:3, atomindex2)
        vecb(3) = 0D0
        
        call rotate_a2b(natoms, vecb, vecb0, xyztemp)
        
        xyzcoords(:, (istep-1)*natoms + 1: istep*natoms) = xyztemp
    end do
    
    call writexyzs(natoms, nsteps, elements, xyzcoords)
    
end program main
