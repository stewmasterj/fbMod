! vim:fdm=marker
! Author: Ross J. Stewart
! DATE: August 2018
! compile: gfortran fbMod.f90 fbbuffertest.f90 -o fbbuffertest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
program fbtest
use fbMod
implicit none
integer :: fbwidth, fbheight, fbline
character(4) :: px, bpx ! B, G, R, A?
character(5299200) :: pxbuff
integer :: i, j, k, r
real(4), dimension(2,400) :: XY
real(4), dimension(2,2) :: pr
integer, dimension(2,2) :: sr

! open and set framebuffer
call fb_init(10,"/dev/fb0","buffer")
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
    call fb_pixel(i+20,j+10,px)
    !k = getrec(i+20,j+10)*4
    !pxbuff(k-3:k) = px
  enddo
enddo
!write(fb%FID ) pxbuff
call fb_write

call fb_close
end program fbtest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
