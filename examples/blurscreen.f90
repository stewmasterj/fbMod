! vim:fdm=marker
! Author: Ross J. Stewart
! DATE: June 30, 2019
! compile: gfortran ../fbMod.f90 blurscreen.f90 -o blurscreen
! run: ./blurscreen
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
program blurscreen
use fbMod
implicit none
integer :: r
character(8) :: cr

if (iargc().eq.1) then
 call getarg(1,cr)
 read(cr,*) r
else
 r = 3
endif

! open and set framebuffer
!call fb_init(10,"/dev/fb0","direct", 1440, 900, 1472, .false.)
call fb_init(10,"/dev/fb0","buffer", 1440, 900, 1472, .false.)

! this can be found with fbset
!fbwidth=1440
!fbheight=900
!fbline=1472 !for some reason line length is not the same as width, WTF?

! read the current screen into buffer
call fb_read

! test a blured rectangle (quarter of the circle
call fb_blurRec( 1, 1, 1440, 900, real(r) )

! write the modified buffer to the screen
call fb_write

! close the frame buffer file
call fb_close
end program blurscreen
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
