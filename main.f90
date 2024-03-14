program main
    use fileIO
    
    implicit none
    
    character(len=200):: filename
    character(len=2), allocatable:: elements(:)
    real(kind=8), allocatable:: xyzcoords(:, :) 
    
    filename = 'pos.xyz'
    
    call readxyzs(filename, elements, xyzcoords)
    
    
end program main
