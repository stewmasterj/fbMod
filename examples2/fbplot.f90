! vim:fdm=marker
! Author: Ross J. Stewart
! DATE: September 2018
! compile: gfortran ../fbMod2.f90 fbplot.f90 -o fbplot
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
program fbplot
use fbMod
implicit none
character(4) :: px, bpx ! B, G, R, A?
character(4), dimension(:), allocatable :: pxY
integer :: i, j, k, r, err, filelines, nY
integer, dimension(:), allocatable :: col
!real(4), dimension(2,400) :: XY
real(4), dimension(2,2) :: pr
integer, dimension(2,2) :: sr
character(80) :: cfile
character(80), dimension(:), allocatable :: tmp
real(4),dimension(:,:),allocatable :: XY

! commandline arguments
if (iargc().eq.1) then
  nY = 1 !assume first and second column
  allocate( col(nY+1) )
  col(1)=1; col(2)=2
elseif (iargc().eq.2) then
  nY = 1 !assume first and second column
  allocate( col(nY+1) )
  col(1)=1; col(2)=2
elseif (iargc().eq.0) then
  call help
else
  ! number of curves to plot from file
  nY=iargc()-2
  allocate( col(nY+1) )
endif

! read file name and open data file
call getarg(iargc(),cfile)
open(10, file=trim(cfile)) 

! read while columns  of file are the X, Y1, Y2, ...Yn
do i = 1, nY+1
 call getarg(i,cfile)
 read(cfile,*) col(i)
enddo

! scan file to find total number of lines
i=0
do
 read(10,*,iostat=err) 
 if (err.ne.0) exit
 i = i + 1
enddo
filelines=i
allocate( XY(nY+1,filelines), pxY(nY) )
rewind(10)

! read the file contents into XY array
allocate( tmp(maxval(col)) )
do i = 1, filelines
 read(10,*) tmp
 do j = 1, nY+1
  read(tmp(col(j)),*) XY(j,i) 
 enddo
enddo
 

! open and set framebuffer
!call fb_init(40,"/dev/fb0","direct")
call fb%fbinit(40,"/dev/fb0")

! this can be found with fbset
!fbwidth=1440
!fbheight=900
!fbline=1472 !for some reason line length is not the same as width, WTF?

! X and Y data ranges
pr(1,:) = (/ minval(XY(1,:)), maxval(XY(1,:)) /)
pr(2,:) = (/ minval(XY(2:,:)), maxval(XY(2:,:)) /)
! X and Y plot location, screen range [sr] 
sr(1,:) = (/ 100, fb%w-100 /)
sr(2,:) = (/ 100, fb%h-100 /)
!write(0,*) sr(1,:), sr(2,:)
write(0,*) "xr:",pr(1,:), "yr:",pr(2,:)

call fb%loadScreen  !load the current screen into the frame buffer

! horizontal lines
px = char(200)//char(200)//char(200)//char(0) !light gray
call fb%line(sr(1,1)-1,sr(2,1)-1, sr(1,2)+1,sr(2,1)-1, px)
call fb%line(sr(1,1)-1,sr(2,2)+1, sr(1,2)+1,sr(2,2)+1, px)
! vertical lines
call fb%line(sr(1,1)-1,sr(2,1), sr(1,1)-1,sr(2,2), px)
call fb%line(sr(1,2)+1,sr(2,1), sr(1,2)+1,sr(2,2), px)

do r = 1, nY
  err = 0
  outer: do i=1,2
    do j=1,2; do k=1,2
      err=err+1
      pxY(r) = char(i*100)//char(j*100)//char(k*100)//char(0)
      if (err.eq.r) exit outer
    enddo; enddo
  enddo outer
enddo
call fb%mplot( XY, pr, sr, pxY )

!! print some numbers
!call fb%putNumber( 1, 1400, 300, 1, px )
!call fb%putNumber( 8, 1405, 300, 1, px )
!call fb%putNumber( 3, 1410, 300, 1, px )
!call fb%putNumber( 2, 1415, 300, 1, px )
!! larger
!call fb_printNumber( 4, 1400, 306, 2, px )
!call fb_printNumber( 7, 1405, 306, 2, px )
!call fb_printNumber( 6, 1410, 306, 2, px )
!call fb_printNumber( 5, 1415, 306, 2, px )
!! normal size and inverse
!px = char(50)//char(0)//char(250)//char(0)
!bpx = char(10)//char(10)//char(10)//char(0)
!call fb_printNumber( 0, 1400, 315, 3, px, bpx )
!call fb_printNumber( 7, 1406, 315, 3, px, bpx )
!call fb_printNumber( 6, 1412, 315, 3, px, bpx )
!call fb_printNumber( 9, 1418, 315, 3, px, bpx )

call fb%display

call fb%save("fbplot.ppm",2)

call fb%close
end program fbplot
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine help
write(0,*) "fbplot -- Plot data from columns of file to the framebuffer device /dev/fb0"
write(0,*) "Usage:"
write(0,*) "   fbplot  xcol ycol y2col y3col ... yNcol FILE"
write(0,*) "xcol  is the column of FILE to use as x coordinates"
write(0,*) "ycol  is the column of FILE to use as the first dependent variable"
write(0,*) " subsequent integer arguments are interpreted as additional dependent variables"
write(0,*) "  to plot onto the same plot."
write(0,*) "FILE  is the last argument and is the file path"
STOP
end subroutine help
