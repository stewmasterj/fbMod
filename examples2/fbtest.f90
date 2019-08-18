! vim:fdm=marker
! Author: Ross J. Stewart
! DATE: August 2018
! compile: gfortran -fbounds-check ../fbMod2.f90 fbtest.f90 -o fbtest
! run: ./fbtest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
program fbtest
use fbMod
implicit none
integer :: fbwidth, fbheight, fbline
character(4) :: px, bpx, red, green, blue, cyan, yellow, magenta, black, white 
character(1024) :: pxline
integer :: i, j, k, r
real(4), dimension(2,400) :: XY
real(4), dimension(2,2) :: pr
integer, dimension(2,2) :: sr

! open and set framebuffer
!call fb_init(10,"/dev/fb0","direct", 1440, 900, 1472, .false.)
call fb%fbinit(10,"/dev/fb0", 1440, 900, 1472, .false.)

! this can be found with fbset
!fb%w=1440
!fb%h=900
!fb%lline=1472 !for some reason line length is not the same as width, WTF?

red = char(0)//char(0)//char(255)//char(0)
green = char(0)//char(255)//char(0)//char(0)
blue = char(255)//char(0)//char(0)//char(0)
cyan = char(255)//char(255)//char(0)//char(0)
yellow = char(0)//char(255)//char(255)//char(0)
magenta = char(255)//char(0)//char(255)//char(0)
black = char(0)//char(0)//char(0)//char(0)
white = char(255)//char(255)//char(255)//char(0)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! pixel tests
px=char(255)//char(255)//char(255)//char(0)
call fb%putPixel(1,1,px) !record 1
px=char(255)//char(0)//char(0)//char(0)
call fb%putPixel(2,1,px) !record 2
px=char(0)//char(255)//char(0)//char(0)
call fb%putPixel(3,1,px) !record 3
px=char(0)//char(0)//char(255)//char(0)
call fb%putPixel(4,1,px) !record 4
call fb%putPixel(fb%w,1,px) !top right
call fb%putPixel(fb%lline+1,1,px) ! second row top left?

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! draw a gradient square
px = char(0)//char(0)//char(0)//char(0)
do j = 0, 255 ! Y should always be outer
  px(2:2) = char(j)
  do i = 0, 255 ! X   should always be inner
    px(1:1) = char(i)
    call fb%putPixel(i+20,j+10,px)
  enddo
enddo

! draw a gradient square
!pxline = char(0)//char(0)//char(0)//char(0)
!do j = 0, 255 ! Y should always be outer
!  !px(2:2) = char(j)
!  do i = 0, 255 ! X   should always be inner
!    !px(1:1) = char(i)
!    pxline(i*4+1:i*4+2) = char(i)//char(j)
!  enddo
!  call fb_pixel(20,j+310,pxline)
!enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! draw a line from point to point
call fb%line(400,300, 600,200, px)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! draw a set of lines at certain angles
px = char(255)//char(0)//char(100)//char(255)
do i = 0, 360, 30
   call fb%linepolar( 1400,40, 30, i, px)
enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Build data array and plot it
!make data to plot
do i = 1, 400
  XY(1,i) = 3.1415926535*float(i)/200.0
  XY(2,i) = sin(XY(1,i)) + sin(5.0*XY(1,i)) + 0.2*sin(130.0*XY(1,i)) + &
   &   0.2*sin(1300.0*XY(1,i))
enddo

! X and Y data ranges
pr(1,:) = (/ 0.0, 3.14159*2.0 /)
pr(2,:) = (/ -2.4, 2.4 /)

! X and Y plot location, screen range [sr] 
sr(1,:) = (/ 200, 600 /)
sr(2,:) = (/ 400, 700 /)
! horizontal lines
call fb%line(199,400, 601,400, px)
call fb%line(199,700, 601,700, px)
! vertical lines
bpx = char(10)//char(10)//char(10)//char(0)
call fb%line2c(199,399, 199,699, px, bpx)
call fb%line(601,399, 601,699, px)

px = char(100)//char(0)//char(200)//char(0)
call fb%plot( XY, pr, sr, px )

! print some numbers
call fb%putNumber( 1, 1400, 300, 1, px )
call fb%putNumber( 8, 1405, 300, 1, px )
call fb%putNumber( 3, 1410, 300, 1, px )
call fb%putNumber( 2, 1415, 300, 1, px )
! larger
call fb%putNumber( 4, 1400, 306, 2, px )
call fb%putNumber( 7, 1405, 306, 2, px )
call fb%putNumber( 6, 1410, 306, 2, px )
call fb%putNumber( 5, 1415, 306, 2, px )
! normal size and inverse
px = char(50)//char(0)//char(250)//char(0)
bpx = char(10)//char(10)//char(10)//char(0)
call fb%putNumber( 0, 1400, 315, 3, px, bpx )
call fb%putNumber( 7, 1406, 315, 3, px, bpx )
call fb%putNumber( 6, 1412, 315, 3, px, bpx )
call fb%putNumber( 9, 1418, 315, 3, px, bpx )

! test small square letter strings
px = char(50)//char(250)//char(50)//char(0)
call fb%putString("cheese", 1300, 350, 1, px, bpx)
call fb%putString("abcdefghijklmnopqrstuvwxy z", 1000, 360, 1, px, bpx)

! test a circle
!call fb_circle(1300, 500, 40, px)
call fb%fillCircle(1300, 500, 40, px)
! test a blured rectangle (quarter of the circle
call fb%blurRec( 1290, 490, 1350, 550, 10.0 )

call fb%sphere(1300, 400, 40, px)

! test triangle fill
call fb%filltriangle( 700,600, 750,550, 780,630, px )
call fb%filltriangle3c( 700,800, 750,750, 780,830, red, green, blue )
call fb%filltriangle3c( 800,800, 850,750, 880,830, cyan, magenta, yellow )
call fb%filltriangle3c( 600,800, 650,750, 680,830, px, white, black )

! X and Y plot location, screen range [sr] 
sr(1,:) = (/ 900, 1300 /)
sr(2,:) = (/ 500, 900 /)
! turn on Z-buffer
fb%Lzbuff = .true.
allocate( fb%zbuff(fb%w, fb%h) )
fb%zbuff = -1.0 !clear Z-Buffer as well (-1 is INF))
! test overlaps
call fb%filltriangle3c( 1000,600, 1000,800, 1100,700, white, white, blue, sr, 1.0, 1.0, 0.0 )
call fb%filltriangle3c( 1000,600, 1000,800, 1200,700, white, white, red, sr, 1.0, 1.0, 1.0 )
! the second triangle will obscure the first, if no zbuffer check
call fb%line(1000,700, 1200,700, px)
call fb%line(1100,600, 1100,800, px)

call fb%display

call fb%save("fbtest.ppm",2)

call fb%close
end program fbtest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
