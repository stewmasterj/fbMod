! vim:fdm=marker
! Author: Ross J. Stewart
! DATE: August 2018
! compile: gfortran ../fbMod2.f90 colors.f90 -o colors
! run: ./colors
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
program fbtest
use fbMod
implicit none
!integer :: fbwidth, fbheight, fbline
character(4) :: px, bpx, red, green, blue, cyan, yellow, magenta, black, white, &
  & grey, purple, orange
!character(1024) :: pxline
integer :: i, x, y, h, rh, s, v, r, g, b
!real(4), dimension(2,400) :: XY
!real(4), dimension(2,2) :: pr
!integer, dimension(2,2) :: sr

! open and set framebuffer
!call fb_init(10,"/dev/fb0","direct")
call fb%fbinit(10,"/dev/fb0")

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
purple = char(255)//char(0)//char(127)//char(0)
orange = char(0)//char(128)//char(255)//char(0)

!call fb_read

! test small square letter strings
px = char(50)//char(250)//char(50)//char(0)
bpx = black
!call fb_printString("abcdefghijklmnopqrstuvwxy z", 1000, 360, 1, px, bpx)

! test triangle fill
!call fb_filltriangle( 700,600, 750,550, 780,630, px )
x = 700; y = 450
r = 128
rh = 64
h = 111 !r*sin(60degrees)
call fb%putString("RGB Hexagon", x-30, y-r, 1, orange, black)

call fb%fillTriangle3c( x-r, y,   x-rh,y+h, x,y, red, magenta, grey )
call fb%fillTriangle3c( x-rh,y+h, x+rh,y+h, x,y, magenta, blue, grey )
call fb%fillTriangle3c( x+r, y,   x+rh,y+h, x,y, cyan, blue, grey )

call fb%fillTriangle3c( x-r, y,   x-rh,y-h, x,y, red, yellow, grey )
call fb%fillTriangle3c( x-rh,y-h, x+rh,y-h, x,y, yellow, green, grey )
call fb%fillTriangle3c( x+r, y,   x+rh,y-h, x,y, cyan, green, grey )

! test HSV
x = 500; y = 300
call fb%putString("H S V", x-2, y-10, 1, white, black)
  call fb%putPixel(x+8,y+126, white)
 call fb%line(x+7,y+127, x+8,y+127, white)
call fb%line(x+6,y+128, x+8,y+128, white)
 call fb%line(x+7,y+129, x+8,y+129, white)
  call fb%putPixel(x+8,y+130, white)

  call fb%putPixel(x+18,y-2, white)
 call fb%line(x+17,y-1, x+18,y-1, white)
call fb%line(x+16,y, x+18,y, white)
 call fb%line(x+17,y+1, x+18,y+1, white)
  call fb%putPixel(x+18,y+2, white)

  call fb%putPixel(x+28,y+126, white)
 call fb%line(x+27,y+127, x+28,y+127, white)
call fb%line(x+26,y+128, x+28,y+128, white)
 call fb%line(x+27,y+129, x+28,y+129, white)
  call fb%putPixel(x+28,y+130, white)

call HSV2RGB( 128,255,128,  r,g,b )
 px = char(b)//char(g)//char(r)//char(0)
call fb%fillRec( x,y+260, x+25,y+270, px )
do i = 0, 255
 ! Hue
 h = 255-i; s=255; v=255
 call HSV2RGB( h,s,v,  r,g,b )
 px = char(b)//char(g)//char(r)//char(0)
 call fb%line( x,y+i, x+5,y+i, px)
 ! saturation
 h = 128; s=255-i; v=255
 call HSV2RGB( h,s,v,  r,g,b )
 px = char(b)//char(g)//char(r)//char(0)
 call fb%line( x+10,y+i, x+15,y+i, px)
 ! value (brightness level)
 h = 128; s=255; v=255-i
 call HSV2RGB( h,s,v,  r,g,b )
 px = char(b)//char(g)//char(r)//char(0)
 call fb%line( x+20,y+i, x+25,y+i, px)
enddo

call fb%display

call fb%save("colors.ppm",2)

call fb%close
end program fbtest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
