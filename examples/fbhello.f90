! vim:fdm=marker
!compile: gfortran fbhello.f90 -o fbhello
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
program fbhello
implicit none
integer :: fbwidth, fbheight, fbline
character(4) :: px ! B, G, R, A?
integer :: i, j, k, r
real(4), dimension(2,400) :: XY
real(4), dimension(2,2) :: pr
integer, dimension(2,2) :: sr


! this can be found with fbset
fbwidth=1440
fbheight=900
fbline=1472 !for some reason line length is not the same as width, WTF?

! 32 bits per pixel means 4 bytes per pixel
! each record in "file" is a pixel
open(10,file='/dev/fb0',ACCESS='DIRECT',RECL=4,FORM='UNFORMATTED')

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! pixel tests
!{{{
write(6,*) "test white pixel at top left"

px=char(255)//char(255)//char(255)//char(0)
write(10,REC=1) px
px=char(255)//char(0)//char(0)//char(0)
write(10,REC=2) px
px=char(0)//char(255)//char(0)//char(0)
write(10,REC=3) px
px=char(0)//char(0)//char(255)//char(0)
write(10,REC=4) px
write(10,REC=fbwidth) px ! top right
write(10,REC=fbline+1) px ! second row top left?
!}}}

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! draw a gradient square
!{{{
do i = 0, 255 ! X
  do j = 0, 255 ! Y
    r  = getrec(i+20,j+10)
    px = char(i)//char(j)//char(0)//char(0)
    write(10,REC=r) px
  enddo
enddo
!}}}

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! draw a line from point to point
call fb_line(400,300, 600,200, px)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! draw a set of lines at certain angles
!{{{
px = char(255)//char(0)//char(100)//char(255)
do i = 0, 360, 30
   call fb_linepolar( 1400,40, 30, i, px)
enddo
!}}}

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Build data array and plot it
!{{{
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
call fb_line(199,400, 601,400, px)
call fb_line(199,700, 601,700, px)
! vertical lines
call fb_line(199,399, 199,699, px)
call fb_line(601,399, 601,699, px)

px = char(100)//char(0)//char(200)//char(0)
call fb_plot( XY, pr, sr, px )
!}}}

close(10)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! returns the 4Byte record number for an XY screen position.
integer function getrec(x,y) !{{{
integer :: x, y
getrec = (y-1)*fbline + x
end function getrec !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
!  point slope form of line !{{{
integer function PS(i,x1,y1,x2,y2)
implicit none
integer(kind=4) :: i,x1,x2,y1,y2
!I forgot point slope form
   PS=y2-((x2-i)*(y2-y1))/(x2-x1)
end function PS !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_line( x1, y1, x2, y2, px )
!      Draw a line from point to point to the terminal directly
subroutine fb_line(x1,y1,x2,y2,px) !{{{
implicit none
integer(kind=4) :: i,x1,x2,y1,y2, x, y
character(4) :: px
 if ((x2-x1).ne.0 .and. abs((y2-y1)/(x2-x1)).lt.1) then
  do i=min(x1,x2),max(x1,x2)
    y = PS(i,x1,y1,x2,y2)
    x = i
    !write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',PS(i,x1,y1,x2,y2),';',i,'H', "*"
    write(10,REC=getrec(x,y)) px
  end do
 else
  do i=min(y1,y2),max(y1,y2)
    y = i
    x = PS(i,y1,x1,y2,x2)
    !write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',i,';',PS(i,y1,x1,y2,x2),'H', "*"
    write(10,REC=getrec(x,y)) px
  end do
 endif
end subroutine fb_line !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_linepolar( x, y, r, theta, px )
!      Draw a line from point with radius and angle to the terminal directly
!   Angle in degrees
subroutine fb_linepolar(x,y,r,theta,px) !{{{
implicit none
integer(kind=4) :: i,x,y,theta,r
character(4) :: px
real(kind=4) :: deg
 deg=theta/57.2957795130823
 !write(6,*) nint(r*cos(deg)), x,y, r, deg
 !    write(6,*) nint(r*sin(deg))
  call fb_line(x,y,x+nint(r*cos(deg)),y-nint(r*sin(deg)), px)
end subroutine fb_linepolar !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_plot( XY, pr, sr )
!   plot data XY within the plot range pr into a pixel domain sr
subroutine fb_plot( XY, pr, sr, px ) !{{{
implicit none
real(4), dimension(:,:), intent(in) :: XY, pr
integer, dimension(2,2), intent(in) :: sr
character(4), intent(in) :: px
integer :: i, x, y
real(4) :: dx, dy

! if there's too much data to fit, it must be binned
dx = (pr(1,2)-pr(1,1))/float(sr(1,2)-sr(1,1)-1)
dy = (pr(2,2)-pr(2,1))/float(sr(2,2)-sr(2,1)-1)

do i=1, size(XY,2) 
   x = int(XY(1,i)/dx+pr(1,1)) + sr(1,1) + 1
   y = -int((XY(2,i)+pr(2,2))/dy) + sr(2,2) - 1
   ! check if out or on terminal domain bounds
   if ( (x.ge.sr(1,2).or.x.le.sr(1,1)).and. &
      & (y.ge.sr(2,2).or.y.le.sr(2,1)) ) cycle
   write(10,REC=getrec(x,y)) px
   !call tput( "@", x, y )
enddo

end subroutine fb_plot !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80

end program fbhello
