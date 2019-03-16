! vim:fdm=marker
! Author: Ross J. Stewart
! DATE: August 2018
! compile: gfortran fbMod.f90 colors.f90 -o colors
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
program fbtest
use fbMod
implicit none
!integer :: fbwidth, fbheight, fbline
character(4) :: px, bpx, red, green, blue, cyan, yellow, magenta, black, white, grey
!character(1024) :: pxline
integer :: r, x, y, h, rh
!real(4), dimension(2,400) :: XY
!real(4), dimension(2,2) :: pr
!integer, dimension(2,2) :: sr

! open and set framebuffer
call fb_init(10,"/dev/fb0","direct")

! this can be found with fbset
!fbwidth=1440
!fbheight=900
!fbline=1472 !for some reason line length is not the same as width, WTF?

red = char(0)//char(0)//char(255)//char(0)
green = char(0)//char(255)//char(0)//char(0)
blue = char(255)//char(0)//char(0)//char(0)
cyan = char(255)//char(255)//char(0)//char(0)
yellow = char(0)//char(255)//char(255)//char(0)
magenta = char(255)//char(0)//char(255)//char(0)
black = char(0)//char(0)//char(0)//char(0)
grey = char(127)//char(127)//char(127)//char(0)
white = char(255)//char(255)//char(255)//char(0)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! test small square letter strings
px = char(50)//char(250)//char(50)//char(0)
bpx = black
call fb_printString("cheese", 1300, 350, 1, px, bpx)
call fb_printString("abcdefghijklmnopqrstuvwxy z", 1000, 360, 1, px, bpx)

! test triangle fill
!call fb_filltriangle( 700,600, 750,550, 780,630, px )
x = 700
y = 450
r = 128
rh = 64
h = 111 !r*sin(60degrees)
call fb_filltriangle3c( x-r, y,   x-rh,y+h, x,y, red, magenta, grey )
call fb_filltriangle3c( x-rh,y+h, x+rh,y+h, x,y, magenta, blue, grey )
call fb_filltriangle3c( x+r, y,   x+rh,y+h, x,y, cyan, blue, grey )

call fb_filltriangle3c( x-r, y,   x-rh,y-h, x,y, red, yellow, grey )
call fb_filltriangle3c( x-rh,y-h, x+rh,y-h, x,y, yellow, green, grey )
call fb_filltriangle3c( x+r, y,   x+rh,y-h, x,y, cyan, green, grey )

call fb_close
end program fbtest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
