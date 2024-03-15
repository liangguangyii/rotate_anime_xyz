module rotatexyz
    
    implicit none
    
    contains
    
    function vec_cross(veca, vecb) result(vecc)
        real*8, dimension(3), intent(in) :: veca, vecb
        real*8, dimension(3):: vecc
            
        vecc(1) = veca(2)*vecb(3) - veca(3)*vecb(2)
        vecc(2) = veca(3)*vecb(1) - veca(1)*vecb(3)
        vecc(3) = veca(1)*vecb(2) - veca(2)*vecb(1)
        
    end function vec_cross
        
    subroutine vec_norm(veca, veca_normed)
        real*8, dimension(3), intent(in):: veca
        real*8, dimension(3), intent(out):: veca_normed
            
        real*8:: norm
            
        norm = sqrt(dot_product(veca, veca))
            
        veca_normed = veca/norm
        
    end subroutine vec_norm
    
    ! rotate vector veca to the direction parallel to vecb
    ! the rotated tensor must be 3*N
    subroutine rotate_a2b(dim, veca, vecb, xyzmat)
        integer, intent(in):: dim
        real(kind=8), dimension(3), intent(in):: veca, vecb
        
        real(kind=8), dimension(3,dim), intent(inout):: xyzmat
        
        real(kind=8):: theta
        real(kind=8), dimension(3):: nvec, nvec_normed, veca_normed, vecb_normed
        real(kind=8), dimension(3,3):: rotmat
        real(kind=8), allocatable:: temp(:,:)
        
        call vec_norm(veca, veca_normed)
        call vec_norm(vecb, vecb_normed)
        
        theta = acos(dot_product(veca_normed, vecb_normed))
        
        nvec = vec_cross(veca_normed, vecb_normed)
        call vec_norm(nvec, nvec_normed)
        nvec = nvec_normed
        
        !vec(1,3) * rotmat(3,3)^T = vec(1,3)
        !rotmat(3,3) *vec(3,1) = vec(3,1)
        
        rotmat(1,:) = (/ nvec(1)*nvec(1)*(1 - cos(theta)) + cos(theta),&
                            nvec(1)*nvec(2)*(1 - cos(theta)) - nvec(3)*sin(theta),&
                            nvec(1)*nvec(3)*(1 - cos(theta)) + nvec(2)*sin(theta)/)
                             
        rotmat(2,:) = (/ nvec(2)*nvec(1)*(1 - cos(theta)) + nvec(3)*sin(theta),&
                            nvec(2)*nvec(2)*(1 - cos(theta)) + cos(theta),&
                            nvec(2)*nvec(3)*(1 - cos(theta)) - nvec(1)*sin(theta)/)
                             
        rotmat(3,:) = (/ nvec(3)*nvec(1)*(1 - cos(theta)) - nvec(2)*sin(theta),&
                            nvec(3)*nvec(2)*(1 - cos(theta)) + nvec(1)*sin(theta),&
                            nvec(3)*nvec(3)*(1 - cos(theta)) + cos(theta)/)        
        
        allocate(temp(3,dim))
        
        temp = matmul(rotmat, xyzmat)
        xyzmat = temp
        
        
    end subroutine rotate_a2b

end module rotatexyz
