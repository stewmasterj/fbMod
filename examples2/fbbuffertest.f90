! vim:fdm=marker
! Author: Ross J. Stewart
! DATE: August 2019
! compile: gfortran ../fbMod2.f90 fbbuffertest.f90 -o fbbuffertest
! run: ./fbbuffertest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
program fbtest
use fbMod
implicit none
character(4) :: px, bpx ! B, G, R, A?
integer :: i, j, k, r

! open and set framebuffer
call fb%fbinit(10,"/dev/fb0",1440,900,1472) !,"buffer")
!fb%FID = 10
!fb%devicePath = "/dev/fb0"
! this can be found with fbset
!fb%width  = 1440
!fb%height =  900
!fb%line   = 1472 !for some reason line length is not the same as width, WTF?

!open(fb%FID,file=fb%devicePath,ACCESS='STREAM',FORM='UNFORMATTED')

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! draw a gradient square
px = char(0)//char(0)//char(0)//char(0)
do j = 0, 255 ! Y should always be outer
  px(2:2) = char(j)
  do i = 0, 255 ! X   should always be inner
    px(1:1) = char(i)
    call fb%putPixel(i+20,j+10,px)
    !k = getrec(i+20,j+10)*4
    !pxbuff(k-3:k) = px
  enddo
enddo
!write(fb%FID ) pxbuff
call fb%display

call fb%save("fbbuffertest.ppm",2)

call fb%close
end program fbtest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
