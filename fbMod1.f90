! vim:fdm=marker
! Fortran module for interacting with the framebuffer /dev/fb0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
Module fbMod
implicit none

type FrameBufferType
 integer :: FID, width, height, line
 character(80) :: devicePath, mode
 character(len=:), allocatable :: pxbuff !full frame buffer
 logical :: Lbuff, Lzbuff
 ! the fb_line2c and fb_filltriangle3c routines can use the Z-Buffer
 real(4), allocatable, dimension(:,:) :: zbuff
end type
type(FrameBufferType) :: fb

type PixBuffType
 integer :: w, h, len
 character(len=:) :: pb !(BGRA)
contains
 procedure :: init => fb_initPixBuff
end type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_init(i,dev)  set variables and open framebuffer device
!    i   file descriptor
!    dev  device path of framebuffer, usually /dev/fb0
subroutine fb_init(i,dev,mode, wd,ht,ln, zbu) !{{{
implicit none
integer :: i
character(*) :: dev, mode
integer, optional, intent(in) :: wd,ht,ln
logical, optional, intent(in) :: zbu

fb%FID = i
fb%devicePath = trim(dev)
fb%mode = trim(mode)
! this can be found with fbset
fb%width  = 1440
fb%height =  900
fb%line   = 1472 !for some reason line length is not the same as width, WTF?
if (present(wd)) fb%width = wd
if (present(ht)) fb%height = ht
if (present(ln)) fb%line = ln

fb%Lzbuff = .false. !use Z-buffer?
if (present(zbu)) fb%Lzbuff = zbu

if (fb%lzbuff) allocate( fb%zbuff(fb%width, fb%height) )
! mode options:
!   direct    each function write is directly to frame buffer device
!   buffer    each function write is to the buffer: "fb%pxbuff"
!              this mode must be explicitly written to the device.
if (trim(mode).eq."direct") then
   fb%Lbuff = .false.
! 32 bits per pixel means 4 bytes per pixel
! each record in "file" is a pixel
   open(fb%FID,file=fb%devicePath,ACCESS='DIRECT',RECL=4,FORM='UNFORMATTED')
elseif (trim(mode).eq."buffer") then
   fb%Lbuff = .true.
   allocate( character(len=4*fb%line*fb%height) :: fb%pxbuff )
! open as stream to be written to in one go. fastest rendering option
   open(fb%FID,file=fb%devicePath,ACCESS='STREAM',FORM='UNFORMATTED')
else
   write(0,*) "ERROR: fb_init: no mode option: "//trim(mode)
   STOP
endif

end subroutine fb_init !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine fb_write !{{{
! write buffer to framebuffer device file to render to screen
rewind(fb%FID)
write(fb%FID) fb%pxbuff
end subroutine fb_write !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine fb_read !{{{
! read framebuffer device file to to buffer
rewind(fb%FID)
read(fb%FID) fb%pxbuff
end subroutine fb_read !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine fb_dump(FL,mode) !{{{
character(*), intent(in) :: FL
integer, intent(in) :: mode
! mode : description
!  1     full buffer dump, as BGRA and excess columns
!  2     full screen binary P6-PPM
!  3     only non NULL pixels printed in ASCII: X Y R G B
integer :: i, k, j, b, g, r, x, y, itmp
character(80) :: frmtstr
character :: rgb(3,fb%line,fb%height)
open(11,file=trim(FL))
select case(mode)
  case(1); write(11) fb%pxbuff
  case(2);
    !write(11,'(''P6'', 2(1x,i4),'' 255 '',$)') fb%line, fb%height
    write(11,'(''P6'', 2(1x,i4),'' 255 '')',advance="no") fb%line, fb%height
    itmp = fb%line*fb%height*3
    !write(frmtstr,'(''('',i8.8,''A,$)'')') itmp
    write(frmtstr,'(''('',i8.8,''A)'')') itmp
    do i = 1, fb%line*fb%height
      k = i*4-3
      y = int(real(i-1)/real(fb%line)) +1
      x = i -(y-1)*fb%line
if (x.le.0 .or. y.le.0) then
  write(0,*) "ERROR: pixel point of bounds (x,y)=",x, y, i, k
  STOP
endif
      rgb(3,x,y) = fb%pxbuff(k:k)
      rgb(2,x,y) = fb%pxbuff(k+1:k+1)
      rgb(1,x,y) = fb%pxbuff(k+2:k+2)
    enddo
    write(11,fmt=frmtstr,advance="no") (((rgb(k,i,j),k=1,3),i=1,fb%line),j=1,fb%height)
  case(3); 
    do i = 1, fb%line*fb%height
      k = i*4-3
      b = ichar(fb%pxbuff(k:k))
      g = ichar(fb%pxbuff(k+1:k+1))
      r = ichar(fb%pxbuff(k+2:k+2))
      if (b.eq.0 .and. g.eq.0 .and. r.eq.0) cycle
      y = int(real(i)/real(fb%line)) +1
      x = i -(y-1)*fb%line
      write(11,'(5i5)') x, y, r, g, b
    enddo
end select
close(11)
end subroutine fb_dump !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine fb_PPM2PixBuff( fil, pb ) !{{{
implicit none
character(*) :: fil
type(PixBuffType) :: pb
character(2) :: ty
character(80) :: frmtstr
integer :: w, h, itmp, i, k
character, allocatable :: rgb(:,:)
open(11,file=trim(fil))
read(11,*)  ty, w, h, itmp
 call pb%init( w, h )
 allocate( rgb(3,w*h) ) !assume 24 bit
 write(frmtstr,'(''('',i8.8,''A)'')') itmp !this should be 3x w*h
 read(11,fmt=frmtstr,advance="no") ((rgb(k,i),k=1,3),i=1,w*h)
 do i = 1, w*h !read each pixel
  k = i*4-3
  pb%pb(k:k)     = rgb(3,i)
  pb%pb(k+1:k+1) = rgb(2,i)
  pb%pb(k+2:k+2) = rgb(1,i)
 enddo
close(11)
end subroutine fb_PPM2PixBuff !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine fb_close !{{{
close(fb%FID)
end subroutine fb_close !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine fb_initPixBuff( pb, w, h ) !{{{
class(PixBuffType) :: pb
integer :: w, h
pb%w = w
pb%h = h
pb%len = h*w
   allocate( character(len=4*pb%len) :: pb%pb )
end subroutine fb_initPixBuff !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! returns the 4Byte record number for an XY screen position.
!  or the byte offset in "buffer" mode.
integer function getrec(x,y) !{{{
integer :: x, y
getrec = (y-1)*fb%line + x
if (fb%Lbuff) getrec = getrec*4-3
end function getrec !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine fb_pixel(x,y,px) !{{{
implicit none
integer, intent(in) :: x, y
integer :: k
character(4), intent(in) :: px
if (fb%Lbuff) then
 k = getrec(x,y)
 fb%pxbuff(k:k+3) = px
else
 write(fb%FID,REC=getrec(x,y)) px
endif
end subroutine !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine fb_getPixel(x,y,px) !{{{
implicit none
integer, intent(in) :: x, y
integer :: k
character(4), intent(out) :: px
k = getrec(x,y)
if (fb%Lbuff) then
 px = fb%pxbuff(k:k+3)
else
 read(fb%FID,REC=k) px
endif
end subroutine fb_getPixel !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine fb_clear( b ) !{{{
integer, optional, intent(in) :: b
integer :: bb
bb = 0 !default byte to write is NULL
if (present(b)) bb=b
fb%pxbuff = repeat(char(bb),4*fb%line*fb%height)
if (fb%Lzbuff) fb%zbuff = -1.0 !clear Z-Buffer as well (-1 is INF)
end subroutine fb_clear !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine RGB2HSV( r,g,b, h,s,v ) !{{{
! from: https://www.cs.rit.edu/~ncs/color/t_convert.html
implicit none 
integer, intent(in) :: r, g, b  !range 0-255
integer, intent(out) :: h, s, v !
real :: mn, mx, dl

mn = min(r, g, b)
mx = max(r, g, b)
v = nint(mx)

dl = mx-mn

if (mx.gt.1.e-3) then
 s = nint(dl/mx*255)
else
 s = 0
 h = -1
 return
endif

if (r==nint(mx)) then
 h = nint(real(g-b)/dl)
elseif (g==nint(mx)) then
 h = 2+nint(real(b-r)/dl)
else
 h = 4+nint(real(r-g)/dl)
endif

h = h * 60 !degrees
if (h.lt.0) h = h + 360
h = nint(h *255./360.)

end subroutine RGB2HSV !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine HSV2RGB( h,s,v, r,g,b ) !{{{
! from: https://www.cs.rit.edu/~ncs/color/t_convert.html
! and : https://dystopiancode.blogspot.com/2012/06/hsv-rgb-conversion-algorithms-in-c.html
implicit none 
integer, intent(out) :: r, g, b  !range 0-255
integer, intent(in) :: h, s, v !
integer :: i
real :: f,p,t,hh,ss,vv, c

if (s==0) then !grey
 r = v; g = v; b = v
 return
endif

hh = real(h)/42.51 ! range [0:5]
i = int(hh)
!f = hh - i !fractional part of h
ss = real(s)/255.
vv = real(v)/255.
c = vv*ss
p = vv * (1.0 -ss) ! m = vv - c
!q = vv * (1.0 -ss*f)
!t = vv * (1.0 -ss*(1.0 -f))
f = c*(1.0-abs(mod(hh,2.0)-1.0)) ! = x
! f = c -c*abs(mod(hh,2.0)-1.0)
t = f + p ! x + m
! t = c -c*abs(mod(hh,2.0)-1.0) + vv -vv*ss


select case (i)
 case(0); r = nint(vv*255.); g = nint(t*255.); b = nint(p*255.)
 case(1); r = nint(t*255.); g = nint(vv*255.); b = nint(p*255.)
 case(2); r = nint(p*255.); g = nint(vv*255.); b = nint(t*255.)
 case(3); r = nint(p*255.); g = nint(t*255.); b = nint(vv*255.)
 case(4); r = nint(t*255.); g = nint(p*255.); b = nint(vv*255.)
 case(5); r = nint(vv*255.); g = nint(p*255.); b = nint(t*255.)
! case(0); r = nint(vv*255.); g = nint(t*255.); b = nint(p*255.)
! case(1); r = nint(q*255.); g = nint(vv*255.); b = nint(p*255.)
! case(2); r = nint(p*255.); g = nint(vv*255.); b = nint(t*255.)
! case(3); r = nint(p*255.); g = nint(q*255.); b = nint(vv*255.)
! case(4); r = nint(t*255.); g = nint(p*255.); b = nint(vv*255.)
! case(5); r = nint(vv*255.); g = nint(p*255.); b = nint(q*255.)
end select

end subroutine HSV2RGB !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
!  SHAPES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
!  point slope form of line !{{{
! returns y given an x that falls on a line from point 1 to 2.
integer function PointSlope(i,x1,y1,x2,y2)
implicit none
integer(kind=4) :: i,x1,x2,y1,y2
real(8) :: tmp
!I forgot point slope form, so I programmed it.
   tmp=dble((x2-i)*(y2-y1))/dble(x2-x1)
   PointSlope=y2-nint(tmp)
end function PointSlope !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_filltriangle( ps(2,3), px )
!      Draw a filled trianlge with verticies p1, p2, p3 with color, px
subroutine fb_filltriangle( x1, y1, x2, y2, x3, y3, px ) !{{{
implicit none
integer(4), dimension(2,3) :: ps
integer(4) :: x1, x2, x3, y1, y2, y3, xs, xe, y, r, x
character(4) :: px

! sort point from top to bottom
if (y1.le.y2 .and.y1.le.y3) then
 ps(:,1) = (/ x1, y1 /)
 if (y2 < y3) then;   ps(:,2) = (/ x2, y2 /);   ps(:,3) = (/ x3, y3 /)
 else;                ps(:,2) = (/ x3, y3 /);   ps(:,3) = (/ x2, y2 /)
 endif
elseif (y2.le.y1 .and.y2.le.y3) then
 ps(:,1) = (/ x2, y2 /)
 if (y1 < y3) then;   ps(:,2) = (/ x1, y1 /);   ps(:,3) = (/ x3, y3 /)
 else;                ps(:,2) = (/ x3, y3 /);   ps(:,3) = (/ x1, y1 /)
 endif
else
 ps(:,1) = (/ x3, y3 /)
 if (y1 < y2) then;   ps(:,2) = (/ x1, y1 /);   ps(:,3) = (/ x2, y2 /)
 else;                ps(:,2) = (/ x2, y2 /);   ps(:,3) = (/ x1, y1 /)
 endif
endif

! lower triangle
do y=ps(2,1), ps(2,2) !only raster down to medium y point
  xs = PointSlope(y,ps(2,1),ps(1,1),ps(2,2),ps(1,2)) !point 1 to 2
  xe = PointSlope(y,ps(2,1),ps(1,1),ps(2,3),ps(1,3)) !point 1 to 3
  do x = min(xs,xe), max(xs,xe)
    r = getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, xs, xe
    !write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',i,';',PS(i,y1,x1,y2,x2),'H', "*"
    if (fb%Lbuff) then
      fb%pxbuff(r:r+3) = px
    else
      write(fb%FID,REC=r) px
    endif
  enddo
end do
! upper triangle
if (ps(2,3) > ps(2,2)) then !only if not horizontal top
do y=ps(2,2)+1, ps(2,3) !only raster down from medium y point to bottom
  xs = PointSlope(y,ps(2,2),ps(1,2),ps(2,3),ps(1,3)) !point 2 to 3
  xe = PointSlope(y,ps(2,1),ps(1,1),ps(2,3),ps(1,3)) !point 1 to 3
  do x = min(xs,xe), max(xs,xe)
    r = getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, xs, xe
    !write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',i,';',PS(i,y1,x1,y2,x2),'H', "*"
    if (fb%Lbuff) then
      fb%pxbuff(r:r+3) = px
    else
      write(fb%FID,REC=r) px
    endif
  enddo
end do
endif


end subroutine fb_filltriangle !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_filltriangle3c( ps(2,3), px(3), [sb(2,2)] )
!      Draw a filled trianlge with verticies p1, p2, p3 
!      each vertex has color px1, px2, px3, fill colour interpolates
!      within bounds, sb
subroutine fb_filltriangle3c( x1,y1, x2,y2, x3,y3, px1,px2,px3, sb, z1,z2,z3 ) !{{{
implicit none
integer(4), dimension(2,3) :: ps
integer(4), intent(in) :: x1, x2, x3, y1, y2, y3
character(4), intent(in) :: px1, px2, px3
character(4) :: pxl1, pxl2, pxl3
integer, dimension(2,2), optional, intent(in) :: sb
real(4), optional, intent(in) :: z1,z2,z3
integer(4) :: xs, xe, y, r, x, idenom
character(4) :: px
real(4), dimension(3) :: w
integer(4)  :: dy23, dy31, dx32, dx13, dy13
real(4) :: denom, z, zp(3)

! sort point from top to bottom
if (present(z1)) then !{{{
  if (y1.le.y2 .and.y1.le.y3) then
   ps(:,1) = (/ x1, y1 /); zp(1) = z1; pxl1 = px1
   if (y2<y3) then; ps(:,2)=(/x2,y2/); ps(:,3)=(/x3,y3/); zp(2:3)=(/z2,z3/); pxl2=px2; pxl3=px3
   else;            ps(:,2)=(/x3,y3/); ps(:,3)=(/x2,y2/); zp(2:3)=(/z3,z2/); pxl2=px3; pxl3=px2
   endif
  elseif (y2.le.y1 .and.y2.le.y3) then
   ps(:,1) = (/ x2, y2 /); zp(1) = z2; pxl1 = px2
   if (y1<y3) then; ps(:,2)=(/x1,y1/); ps(:,3)=(/x3,y3/); zp(2:3)=(/z1,z3/); pxl2=px1;pxl3=px3
   else;            ps(:,2)=(/x3,y3/); ps(:,3)=(/x1,y1/); zp(2:3)=(/z3,z1/); pxl2=px3;pxl3=px1
   endif
  else
   ps(:,1) = (/ x3, y3 /); zp(1) = z3; pxl1 = px3
   if (y1<y2) then; ps(:,2)=(/x1,y1/); ps(:,3)=(/x2,y2/); zp(2:3)=(/z1,z2/); pxl2=px1;pxl3=px2
   else;            ps(:,2)=(/x2,y2/); ps(:,3)=(/x1,y1/); zp(2:3)=(/z2,z1/); pxl2=px2;pxl3=px1
   endif
  endif
else
  if (y1.le.y2 .and.y1.le.y3) then
   ps(:,1) = (/ x1, y1 /); pxl1 = px1
   if (y2 < y3) then; ps(:,2) = (/x2,y2/); ps(:,3) = (/x3,y3/); pxl2=px2; pxl3=px3
   else;              ps(:,2) = (/x3,y3/); ps(:,3) = (/x2,y2/); pxl2=px3; pxl3=px2
   endif
  elseif (y2.le.y1 .and.y2.le.y3) then
   ps(:,1) = (/ x2, y2 /); pxl1 = px2
   if (y1 < y3) then; ps(:,2) = (/x1,y1/); ps(:,3) = (/x3,y3/); pxl2=px1;pxl3=px3
   else;              ps(:,2) = (/x3,y3/); ps(:,3) = (/x1,y1/); pxl2=px3;pxl3=px1
   endif
  else
   ps(:,1) = (/ x3, y3 /); pxl1 = px3 
   if (y1 < y2) then; ps(:,2) = (/x1,y1/); ps(:,3) = (/x2,y2/); pxl2=px1;pxl3=px2
   else;              ps(:,2) = (/x2,y2/); ps(:,3) = (/x1,y1/); pxl2=px2;pxl3=px1
   endif
  endif
endif !}}}

! save these convinient vlues for weight calculation
dy23 = ps(2,2)-ps(2,3) !y2-y3
dy31 = ps(2,3)-ps(2,1) !y3-y1
dy13 = ps(2,1)-ps(2,3) !y1-y3
dx32 = ps(1,3)-ps(1,2) !x3-x2
dx13 = ps(1,1)-ps(1,3) !x1-x3
idenom = dy23*dx13+dx32*dy13
if (idenom.eq.0) then
 !write(0,*) "ERROR triangle:", ps(:,1), ":", ps(:,2),":",ps(:,3)
 !write(0,*) "denom=0",dy23,dx13,dx32,dy13
 Return
endif

denom=real(idenom)

! top triangle
if (ps(2,2) > ps(2,1)) then !only if not horizontal top
do y=ps(2,1), ps(2,2) !only raster down to medium y point
  xs = PointSlope(y,ps(2,1),ps(1,1),ps(2,2),ps(1,2)) !point 1 to 2
  xe = PointSlope(y,ps(2,1),ps(1,1),ps(2,3),ps(1,3)) !point 1 to 3
  do x = min(xs,xe), max(xs,xe)
    if (present(sb)) then
     if (x.lt.sb(1,1)) then; cycle !x too low
     elseif (x.gt.sb(1,2)) then; cycle; endif !x too large
     if (y.lt.sb(2,1)) then; cycle !y too low
     elseif (y.gt.sb(2,2)) then; cycle; endif !y too large
    endif
    r = getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, xs, xe
    ! calculate each vertex's position weight on (x,y)
    !w(1) = (dy23*(x-x3) + dx32*(y-y3))/denom
    !w(2) = (dy31*(x-x3) + dx13*(y-y3))/denom
    w(1) = (dy23*(x-ps(1,3)) + dx32*(y-ps(2,3)))/denom
    w(2) = (dy31*(x-ps(1,3)) + dx13*(y-ps(2,3)))/denom
    w(3) = 1.0 -w(1) -w(2)
    if (minval(w).lt.0.0) cycle !outside of triangle
    if (fb%Lzbuff) then
      !z = z1*w(1) +z2*w(2) +z3*w(3)
      z = zp(1)*w(1) +zp(2)*w(2) +zp(3)*w(3)
      if (fb%zbuff(x,y).lt.0.0) then
        fb%zbuff(x,y) = z !save this to the Z-buffer and render it
      else
        if (z.lt.fb%zbuff(x,y)) then !then it's behind what is already rendered
          fb%zbuff(x,y) = z !save this to the Z-buffer and render it, since it's the closest
        else
          cycle
        endif
      endif
    endif
    ! use the weights to interpolate each RGB value
    !px(1:1) = char(nint( ichar(px1(1:1))*w(1) +ichar(px2(1:1))*w(2) +ichar(px3(1:1))*w(3)))
    !px(2:2) = char(nint( ichar(px1(2:2))*w(1) +ichar(px2(2:2))*w(2) +ichar(px3(2:2))*w(3)))
    !px(3:3) = char(nint( ichar(px1(3:3))*w(1) +ichar(px2(3:3))*w(2) +ichar(px3(3:3))*w(3)))
    px(1:1) = char(nint( ichar(pxl1(1:1))*w(1) +ichar(pxl2(1:1))*w(2) +ichar(pxl3(1:1))*w(3)))
    px(2:2) = char(nint( ichar(pxl1(2:2))*w(1) +ichar(pxl2(2:2))*w(2) +ichar(pxl3(2:2))*w(3)))
    px(3:3) = char(nint( ichar(pxl1(3:3))*w(1) +ichar(pxl2(3:3))*w(2) +ichar(pxl3(3:3))*w(3)))
    if (fb%Lbuff) then
      fb%pxbuff(r:r+3) = px
    else
      write(fb%FID,REC=r) px
    endif
  enddo
end do
endif
! Lower triangle
if (ps(2,3) > ps(2,2)) then !only if not horizontal bottom
do y=ps(2,2), ps(2,3) !only raster down from medium y point to bottom
  xs = PointSlope(y,ps(2,2),ps(1,2),ps(2,3),ps(1,3)) !point 2 to 3
  xe = PointSlope(y,ps(2,1),ps(1,1),ps(2,3),ps(1,3)) !point 1 to 3
  do x = min(xs,xe), max(xs,xe)
    if (present(sb)) then
     if (x.lt.sb(1,1)) then; cycle !x too low
     elseif (x.gt.sb(1,2)) then; cycle; endif !x too large
     if (y.lt.sb(2,1)) then; cycle !y too low
     elseif (y.gt.sb(2,2)) then; cycle; endif !y too large
    endif
    r = getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, xs, xe
    ! calculate each vertex's position weight on (x,y)
    !w(1) = (dy23*(x-x3) + dx32*(y-y3))/denom
    !w(2) = (dy31*(x-x3) + dx13*(y-y3))/denom
    w(1) = (dy23*(x-ps(1,3)) + dx32*(y-ps(2,3)))/denom
    w(2) = (dy31*(x-ps(1,3)) + dx13*(y-ps(2,3)))/denom
    w(3) = 1.0 -w(1) -w(2)
    if (minval(w).lt.0.0) cycle !outside of triangle
    if (fb%Lzbuff) then
      !z = z1*w(1) +z2*w(2) +z3*w(3)
      z = zp(1)*w(1) +zp(2)*w(2) +zp(3)*w(3)
      if (fb%zbuff(x,y).lt.0.0) then
        fb%zbuff(x,y) = z !save this to the Z-buffer and render it
      else
        if (z.lt.fb%zbuff(x,y)) then !then it's behind what is already rendered
          fb%zbuff(x,y) = z !save this to the Z-buffer and render it, since it's the closest
        else
          cycle
        endif
      endif
    endif
    ! use the weights to interpolate each RGB value
    !px(1:1) = char(nint( ichar(px1(1:1))*w(1) +ichar(px2(1:1))*w(2) +ichar(px3(1:1))*w(3) ))
    !px(2:2) = char(nint( ichar(px1(2:2))*w(1) +ichar(px2(2:2))*w(2) +ichar(px3(2:2))*w(3) ))
    !px(3:3) = char(nint( ichar(px1(3:3))*w(1) +ichar(px2(3:3))*w(2) +ichar(px3(3:3))*w(3) ))
    px(1:1) = char(nint( ichar(pxl1(1:1))*w(1) +ichar(pxl2(1:1))*w(2) +ichar(pxl3(1:1))*w(3) ))
    px(2:2) = char(nint( ichar(pxl1(2:2))*w(1) +ichar(pxl2(2:2))*w(2) +ichar(pxl3(2:2))*w(3) ))
    px(3:3) = char(nint( ichar(pxl1(3:3))*w(1) +ichar(pxl2(3:3))*w(2) +ichar(pxl3(3:3))*w(3) ))
    if (fb%Lbuff) then
      fb%pxbuff(r:r+3) = px
    else
      write(fb%FID,REC=r) px
    endif
  enddo
end do
endif


end subroutine fb_filltriangle3c !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_line( x1, y1, x2, y2, px )
!      Draw a line from point to point to the terminal directly
!      Bresenham algorithm
subroutine fb_line(x1,y1,x2,y2,px) !{{{
implicit none
integer(kind=4) :: x1,x2,y1,y2, x, y, r, k
character(4) :: px
logical :: LX, LY
LX=.false.
LY=.false.
if ((x2-x1).ne.0) LX=.true.
if ((y2-y1).ne.0) LY=.true.
! check for same starting and end point
if (.not.LX .and. .not.LY) then
  if (fb%Lbuff) then
    k = getrec(x1,y1)
    fb%pxbuff(k:k+3) = px
  else
    write(fb%FID,REC=getrec(x1,y1)) px
  endif
  RETURN
endif

if (LX .and. abs(real(y2-y1)).lt.abs(real(x2-x1))) then
  do x=min(x1,x2),max(x1,x2)
    y = PointSlope(x,x1,y1,x2,y2)
    r = getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, x1, y1, x2, y2
    !write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',PS(i,x1,y1,x2,y2),';',i,'H', "*"
    if (fb%Lbuff) then
      fb%pxbuff(r:r+3) = px
    else
      write(fb%FID,REC=r) px
    endif
  end do
else
  do y=min(y1,y2),max(y1,y2)
    x = PointSlope(y,y1,x1,y2,x2)
    r = getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, x1, y1, x2, y2
    !write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',i,';',PS(i,y1,x1,y2,x2),'H', "*"
    if (fb%Lbuff) then
      fb%pxbuff(r:r+3) = px
    else
      write(fb%FID,REC=r) px
    endif
  end do
endif
end subroutine fb_line !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_line2c( x1, y1, x2, y2, px(2), [sb(2,2)], [z1, z2] )
!      Draw a line from point to point to the terminal directly
!      Bresenham algorithm
!      use 'sb' to check bounds
subroutine fb_line2c(x1,y1,x2,y2,px1,px2,sb,z1,z2) !{{{
implicit none
integer(kind=4) :: x1,x2,y1,y2, x, y, r, k, m1, m2
character(4) :: px1,px2, px
real(4), optional, intent(in) :: z1,z2
real(4) :: z, w(2), zp(2)
integer, dimension(2,2), optional, intent(in) :: sb
logical :: LX, LY
LX=.false.
LY=.false.
if ((x2-x1).ne.0) LX=.true.
if ((y2-y1).ne.0) LY=.true.
! check for same starting and end point
if (.not.LX .and. .not.LY) then
  if (fb%Lbuff) then
    k = getrec(x1,y1)
    fb%pxbuff(k:k+3) = px1
  else
    write(fb%FID,REC=getrec(x1,y1)) px1
  endif
  RETURN
endif

if (LX .and. abs(real(y2-y1)).lt.abs(real(x2-x1))) then
  m1=min(x1,x2)
  m2=max(x1,x2)
  if (present(z1)) then
    if (m1.eq.x1) then; zp(1) = z1; zp(2) = z2
    else; zp(1) = z2; zp(2) = z1; endif
  endif
  do x=m1,m2
    y = PointSlope(x,x1,y1,x2,y2)
    if (present(sb)) then
     if (x.lt.sb(1,1)) then; cycle !x too low
     elseif (x.gt.sb(1,2)) then; cycle; endif !x too large
     if (y.lt.sb(2,1)) then; cycle !y too low
     elseif (y.gt.sb(2,2)) then; cycle; endif !y too large
    endif
    r = getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, x1, y1, x2, y2
    w(1) = real(m2-x)/real(m2-m1) !1-0, full on the minimum, m1
    if (m1.eq.x1) then; w(2) = 1.0-w(1)
    else;  w(2) = w(1); w(1) = 1.0-w(2)
    endif
    if (fb%Lzbuff) then
      z = z1*w(1) +z2*w(2) 
      if (fb%zbuff(x,y).gt.0.0.and.z.gt.fb%zbuff(x,y)) then !then it's behind what is already rendered
        cycle
      else
        fb%zbuff(x,y) = z !save this to the Z-buffer and render it, since it's the closest
      endif
    endif
    ! use the weights to interpolate each RGB value
    px(1:1) = char(nint( ichar(px1(1:1))*w(1) +ichar(px2(1:1))*w(2) ))
    px(2:2) = char(nint( ichar(px1(2:2))*w(1) +ichar(px2(2:2))*w(2) ))
    px(3:3) = char(nint( ichar(px1(3:3))*w(1) +ichar(px2(3:3))*w(2) ))
    !write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',PS(i,x1,y1,x2,y2),';',i,'H', "*"
    if (fb%Lbuff) then
      fb%pxbuff(r:r+3) = px
    else
      write(fb%FID,REC=r) px
    endif
  end do
else
  m1=min(y1,y2)
  m2=max(y1,y2)
  if (present(z1)) then
    if (m1.eq.x1) then; zp(1) = z1; zp(2) = z2
    else; zp(1) = z2; zp(2) = z1; endif
  endif
  do y=m1,m2
    x = PointSlope(y,y1,x1,y2,x2)
    if (present(sb)) then
     if (y.lt.sb(2,1)) then; cycle !y too low
     elseif (y.gt.sb(2,2)) then; cycle; endif !y too large
     if (x.lt.sb(1,1)) then; cycle !x too low
     elseif (x.gt.sb(1,2)) then; cycle; endif !x too large
    endif
    r = getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, x1, y1, x2, y2
    w(1) = real(m2-y)/real(m2-m1) !1-0, full on the minimum, m1
    if (m1.eq.y1) then; w(2) = 1.0-w(1)
    else;  w(2) = w(1); w(1) = 1.0-w(2)
    endif
    if (fb%Lzbuff) then
      z = z1*w(1) +z2*w(2) 
      if (fb%zbuff(x,y).gt.0.0.and.z.gt.fb%zbuff(x,y)) then !then it's behind what is already rendered
        cycle
      else
        fb%zbuff(x,y) = z !save this to the Z-buffer and render it, since it's the closest
      endif
    endif
    ! use the weights to interpolate each RGB value
    px(1:1) = char(nint( ichar(px1(1:1))*w(1) +ichar(px2(1:1))*w(2) ))
    px(2:2) = char(nint( ichar(px1(2:2))*w(1) +ichar(px2(2:2))*w(2) ))
    px(3:3) = char(nint( ichar(px1(3:3))*w(1) +ichar(px2(3:3))*w(2) ))
    if (fb%Lbuff) then
      fb%pxbuff(r:r+3) = px
    else
      write(fb%FID,REC=r) px
    endif
  end do
endif
end subroutine fb_line2c !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_rec( x1, y1, x2, y2, px )
!      Draw a rectangle 
subroutine fb_rec(x1,y1,x2,y2,px) !{{{
implicit none
integer(kind=4) :: x1,x2,y1,y2, lx, hx, ly,hy
character(4) :: px
lx = min(x1,x2)
hx = max(x1,x2)
ly = min(y1,y2)
hy = max(y1,y2)
call fb_line(lx,ly,hx,ly,px)
call fb_line(lx,hy,hx,hy,px)
call fb_line(lx,ly+1,lx,hy-1,px)
call fb_line(hx,ly+1,hx,hy-1,px)
end subroutine fb_rec !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_fillrec( x1, y1, x2, y2, px )
!      Draw a filled rectangle 
subroutine fb_fillrec(x1,y1,x2,y2,px) !{{{
implicit none
integer(kind=4) :: x1,x2,y1,y2, i, lx, hx
character(4) :: px
lx = min(x1,x2)
hx = max(x1,x2)
do i = min(y1,y2), max(y1,y2)
  call fb_line(lx,i,hx,i,px)
enddo
end subroutine fb_fillrec !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_putpic( x, y, pb, alpha )
!      Draw a PixBuffType, pb (BGRA) to the location (x,y) of screen
!      If alpha=.True. then use Alpha channel, if False then ignore.
subroutine fb_putpic( x, y, pb, alpha )
implicit none
integer(kind=4) :: x,y, k
logical, optional :: alpha
type(PixBuffType) :: pb
if (present(alpha)) then !do alpha channel mixing
else !just put the pic to the screen
 do l = 1, pb%h !loop over pixbuff lines
  if (fb%Lbuff) then
   k = getrec(x,y)
   ! copy full lines at a time (faster than pixel by pixel)
   fb%pxbuff(k:k+pb%w*3) = pb%pb( (l-1)*pb%w+1 : l*pb%w )
  else
   write(fb%FID,REC=getrec(x,y)) pb%pb( (l-1)*pb%w+1 : l*pb%w )
  endif
 enddo
endif
end subroutine fb_putpic
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_blurRec( x1, y1, x2, y2, r )
!      Blur existing pixels in rectangle 
subroutine fb_blurRec(x1,y1,x2,y2,r)  !{{{
implicit none
integer(kind=4), intent(in) :: x1,y1,x2,y2
real, intent(in) :: r
integer :: i, j, x, y, k, l, ir, v(4)
real :: dist, kcoef
character(4) :: px
integer, allocatable, dimension(:,:,:) :: rbuf
! make a buffer for this rectangle
allocate( rbuf(4,x2-x1+1,y2-y1+1) )
! read screen buffer into this rectangle
rbuf = 0
do j = y1, y2
 do i = x1, x2
  call fb_getPixel(i,j,px)
  ! local coordinates
  x = i-x1+1
  y = j-y1+1
  rbuf(1,x,y) = iachar(px(1:1))
  rbuf(2,x,y) = iachar(px(2:2))
  rbuf(3,x,y) = iachar(px(3:3))
  rbuf(4,x,y) = iachar(px(4:4))
 enddo
enddo
! read over the pixels in this buffer to determine how to blur the location
!  on the screen
ir = int(r)
kcoef = 315.0/(64.0*3.1415926535*r**9)*r
do j = y1, y2
 do i = x1, x2
  ! local coordinates
  x = i-x1+1
  y = j-y1+1
  v(:) = 0
  ! loop over local region 
  do k = -ir,ir
   if (y+k.gt.y2-y1+1) cycle !out of range
   if (y+k.lt.1) cycle !out of range
   do l = -ir,ir
    if (x+l.gt.x2-x1+1) cycle !out of range
    if (x+l.lt.1) cycle !out of range
    dist = real(l*l+k*k)
    if (dist.gt.r*r) cycle !out of range
    ! scale influence of this value by a 6poly kernel
    v = v + nint(real(rbuf(:,x+l,y+k))*kcoef*(r*r-dist)**3)
   enddo
  enddo
  ! now have the average values
  v(1) = min(v(1),255)
  v(2) = min(v(2),255)
  v(3) = min(v(3),255)
  v(4) = min(v(4),255)
  px = char(v(1))//char(v(2))//char(v(3))//char(v(4))
  ! write it to screen buffer
  call fb_pixel(i,j,px)
 enddo
enddo
end subroutine fb_blurRec !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_linepolar( x, y, r, theta, px )
!      Draw a line from point with radius and angle to the terminal directly
!   Angle in degrees
subroutine fb_linepolar(x,y,r,theta,px) !{{{
implicit none
integer(kind=4) :: x,y,theta,r
character(4) :: px
real(kind=4) :: deg
 deg=theta/57.2957795130823
 !write(6,*) nint(r*cos(deg)), x,y, r, deg
 !    write(6,*) nint(r*sin(deg))
  call fb_line(x,y,x+nint(r*cos(deg)),y-nint(r*sin(deg)), px)
end subroutine fb_linepolar !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_circle( x, y, r, px )
!   draw a circle centered at location (x,y) with radius of, r. and color, px
subroutine fb_circle( x, y, r, px ) !{{{
implicit none
integer, intent(in) :: x, y, r
character(4), intent(in) :: px
integer :: i, i1, j
i1 = nint(real(r)/sqrt(2.0))
! draw upper and lower curves first
! draw left and right curves second
do i = -i1, i1
  j = nint(sqrt(real(r*r-i*i)))
  call fb_pixel( i+x, j+y, px)
  call fb_pixel( i+x,-j+y, px)
  call fb_pixel( j+x, i+y, px)
  call fb_pixel(-j+x,-i+y, px)
enddo

end subroutine fb_circle !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_fillcircle( x, y, r, px )
!   draw a circle centered at location (x,y) with radius of, r. and color, px
subroutine fb_fillcircle( x, y, r, px ) !{{{
implicit none
integer, intent(in) :: x, y, r
character(4), intent(in) :: px
integer :: i, j
real :: ra
! augment radius by a fraction of a pixel to prevent single pixel top and bottom
ra = real(r)+0.4
ra = ra*ra

call fb_line(-r+x, y,  r+x, y, px) !center horizontal
! draw upper and lower curves 
do i = 1, r
  j = nint(sqrt(ra-real(i*i)))
  call fb_line(-j+x,-i+y,  j+x,-i+y, px) !upper
  call fb_line(-j+x, i+y,  j+x, i+y, px) !lower
enddo

end subroutine fb_fillcircle !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_sphere( x, y, r, px )
!   draw a circle centered at location (x,y) with radius of, r. and color, px
subroutine fb_sphere( x, y, r, px ) !{{{
implicit none
integer, intent(in) :: x, y, r
character(4), intent(in) :: px
character(4) :: lp ! local pixel
integer :: i, j, j1, ic(3)
real :: ra, d
! augment radius by a fraction of a pixel to prevent single pixel top and bottom
ra = real(r)+0.4
ra = ra*ra

call fb_line(-r+x, y,  r+x, y, px) !center horizontal
! draw upper and lower curves 
do i = -r, r ! vertical raster
  j1 = nint(sqrt(ra-real(i*i)))
  do j = -j1, j1 !horizonal raster
    d = sqrt( 1.0-real(j*j+i*i)/real(r*r) )
    ic(1) = int(real(iachar(px(1:1)))*d)
    ic(2) = int(real(iachar(px(2:2)))*d)
    ic(3) = int(real(iachar(px(3:3)))*d)
    lp(:) = char(ic(1))//char(ic(2))//char(ic(3))//px(4:4)
    call fb_pixel( j+x, i+y, lp )
  enddo
enddo

end subroutine fb_sphere !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
!  PLOTS & FONTS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_plot( XY, pr, sr, px )
!   plot data XY within the plot range pr into a pixel domain sr
subroutine fb_plot( XY, pr, sr, px ) !{{{
implicit none
real(4), dimension(:,:), intent(in) :: XY, pr
integer, dimension(2,2), intent(in) :: sr
character(4), intent(in) :: px
integer :: i, x, y, k
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
   if (fb%Lbuff) then ! writing to buffer
     k = getrec(x,y)
     fb%pxbuff(k:k+3) = px
   else    !writing directly to frame buffer device.
     write(fb%FID,REC=getrec(x,y)) px
   endif
   !call tput( "@", x, y ) !this line was used for "fcurses"
enddo

end subroutine fb_plot !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_mplot( XY, pr, sr, px )
!   plot multiple data XY within the plot range pr into a pixel domain sr
subroutine fb_mplot( XY, pr, sr, px ) !{{{
implicit none
real(4), dimension(:,:), intent(in) :: XY
real(4), dimension(2,2), intent(in) :: pr
integer, dimension(2,2), intent(in) :: sr
character(4), dimension(:), intent(in) :: px
integer :: i, j, x, y, n, ox, oy
real(4) :: dx, dy

n = size(px)

! if there's too much data to fit, it must be binned
dx = (pr(1,2)-pr(1,1))/float(sr(1,2)-sr(1,1)-1)
dy = (pr(2,2)-pr(2,1))/float(sr(2,2)-sr(2,1)-1)

do j = 2, n+1
  do i=1, size(XY,2) 
   if (i.gt.1) then
     ox=x; oy=y
   endif
   x = int((XY(1,i)-pr(1,1))/dx) + sr(1,1) + 1
   y = sr(2,2) -int((XY(j,i)-pr(2,1))/dy) + 1 
   if (i.gt.1) then
     ! check if out or on terminal domain bounds
     if ( (x.ge.sr(1,2).or.x.le.sr(1,1)).and. &
        & (y.ge.sr(2,2).or.y.le.sr(2,1)) ) cycle
     call fb_line( ox,oy, x,y, px(j-1) )
     !write(0,*) x, y, sr(2,2), XY(j,i)/dy
     !write(fb%FID,REC=getrec(x,y)) px(j)
   endif
   !call tput( "@", x, y )
  enddo
enddo

end subroutine fb_mplot !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_heatplot( XYZ, pr, sr, cr )
!   plot data XYZ within the plot range pr into a pixel domain sr
subroutine fb_heatplot( XYZ, pr, sr, cr ) !{{{
implicit none
real(4), dimension(:,:), intent(in) :: XYZ, pr
integer, dimension(2,2), intent(in) :: sr
character(4), intent(in) :: cr !color range type
character(4) :: px
integer :: i, x, y, k
real(4) :: dx, dy, dz

! if there's too much data to fit, it must be binned
dx = (pr(1,2)-pr(1,1))/float(sr(1,2)-sr(1,1)-1)
dy = (pr(2,2)-pr(2,1))/float(sr(2,2)-sr(2,1)-1)
dz = (pr(3,2)-pr(3,1))/255.0

do i=1, size(XYZ,2) 
   x = int((XYZ(1,i)-pr(1,1))/dx) + sr(1,1) + 1
   y = -int((XYZ(2,i)+pr(2,2))/dy) + sr(2,2) - 1
   select case(cr)
   case("bw  ") 
      k = int((XYZ(3,i)-pr(3,1))/dz)
      px = char(k)//char(k)//char(k)//char(0)
   case("rgb ") 
      k = int((XYZ(3,i)-pr(3,1))*2.0/dz)
      if (k.le.255) then
        px = char(255-k)//char(k)//char(0)//char(0)
      else 
        k = k-255
        px = char(0)//char(255-k)//char(k)//char(0)
      endif
   case default
      k = int((XYZ(3,i)-pr(3,1))/dz)
      px = char(k)//char(k)//char(k)//char(0)
   end select
   ! check if out or on terminal domain bounds
   if ( (x.ge.sr(1,2).or.x.le.sr(1,1)).or. &
      & (y.ge.sr(2,2).or.y.le.sr(2,1)) ) cycle
   if (fb%Lbuff) then ! writing to buffer
     k = getrec(x,y)
     fb%pxbuff(k:k+3) = px
   else    !writing directly to frame buffer device.
     write(fb%FID,REC=getrec(x,y)) px
   endif
enddo

end subroutine fb_heatplot !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_matrixplot( XYZ, ar, sr, cr )
!   plot data XYZ within the plot range pr into a pixel domain sr
subroutine fb_matrixplot( XYZ, ar, sr, cr ) !{{{
implicit none
real(4), dimension(:,:), intent(in) :: XYZ
integer, dimension(2,2), intent(in) :: sr
real(4), dimension(2), intent(in) :: ar
character(4), intent(in) :: cr !color range type
character(4) :: px
integer :: i, j, x, y, k
real(4) :: dx, dy, dz

! if there's too much data to fit, it must be binned
dx = float(size(XYZ,1))/float(sr(1,2)-sr(1,1)-1)
dy = float(size(XYZ,2))/float(sr(2,2)-sr(2,1)-1)
dz = (ar(2)-ar(1))/255.0

do i=1, size(XYZ,1) 
 do j=1, size(XYZ,2)
   x = int(real(i)/dx) + sr(1,1) + 1
   y = -int(real(j)/dy) + sr(2,2) - 1
   select case(cr)
   case("bw  ") 
      k = int((XYZ(i,j)-ar(1))/dz)
      px = char(k)//char(k)//char(k)//char(0)
   case("rgb ") 
      k = int((XYZ(i,j)-ar(1))*2.0/dz)
      if (k.le.255) then
        px = char(255-k)//char(k)//char(0)//char(0)
      else 
        k = k-255
        px = char(0)//char(255-k)//char(k)//char(0)
      endif
   case default
      k = int((XYZ(i,j)-ar(1))/dz)
      px = char(k)//char(k)//char(k)//char(0)
   end select
   ! check if out or on terminal domain bounds
   if ( (x.ge.sr(1,2).or.x.le.sr(1,1)).or. &
      & (y.ge.sr(2,2).or.y.le.sr(2,1)) ) cycle
   if (fb%Lbuff) then ! writing to buffer
     k = getrec(x,y)
     fb%pxbuff(k:k+3) = px
   else    !writing directly to frame buffer device.
     write(fb%FID,REC=getrec(x,y)) px
   endif
 enddo
enddo

end subroutine fb_matrixplot !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_printNumber(n, x, y, s, fpx, bpx)
!   print a monochrome digit, n, at location, x, y, with size, s, and color, px.
subroutine fb_printNumber(n, x, y, s, px, bpx) !{{{
implicit none
integer, intent(in) :: n, x, y, s
character(4), intent(in) :: px
character(4), optional, intent(in) :: bpx
logical, dimension(3,5) :: N1
logical, dimension(4,7) :: N2
logical, dimension(5,9) :: N3
logical :: T, F, BG
integer :: i, j

T = .true.
F = .false.

if (present(bpx)) then; BG=.true.
else; BG=.false.; endif

if (s.eq.1) then
select case (n) !{{{
case(0)
N1(:,1) = (/ F, T, F /)
N1(:,2) = (/ T, F, T /)
N1(:,3) = (/ T, F, T /)
N1(:,4) = (/ T, F, T /)
N1(:,5) = (/ F, T, F /)
case(1)
N1(:,1) = (/ F, T, F /)
N1(:,2) = (/ F, T, F /)
N1(:,3) = (/ F, T, F /)
N1(:,4) = (/ F, T, F /)
N1(:,5) = (/ F, T, F /)
case(2)
N1(:,1) = (/ F, T, F /)
N1(:,2) = (/ T, F, T /)
N1(:,3) = (/ F, F, T /)
N1(:,4) = (/ T, T, F /)
N1(:,5) = (/ T, T, T /)
case(3)
N1(:,1) = (/ T, T, F /)
N1(:,2) = (/ F, F, T /)
N1(:,3) = (/ F, T, F /)
N1(:,4) = (/ F, F, T /)
N1(:,5) = (/ T, T, F /)
case(4)
N1(:,1) = (/ T, F, T /)
N1(:,2) = (/ T, F, T /)
N1(:,3) = (/ T, T, T /)
N1(:,4) = (/ F, F, T /)
N1(:,5) = (/ F, F, T /)
case(5)
N1(:,1) = (/ T, T, T /)
N1(:,2) = (/ T, F, F /)
N1(:,3) = (/ T, T, F /)
N1(:,4) = (/ F, F, T /)
N1(:,5) = (/ T, T, F /)
case(6)
N1(:,1) = (/ F, T, T /)
N1(:,2) = (/ T, F, F /)
N1(:,3) = (/ T, T, F /)
N1(:,4) = (/ T, F, T /)
N1(:,5) = (/ F, T, F /)
case(7)
N1(:,1) = (/ T, T, T /)
N1(:,2) = (/ F, F, T /)
N1(:,3) = (/ F, T, F /)
N1(:,4) = (/ F, T, F /)
N1(:,5) = (/ F, T, T /)
case(8)
N1(:,1) = (/ F, T, F /)
N1(:,2) = (/ T, F, T /)
N1(:,3) = (/ F, T, F /)
N1(:,4) = (/ T, F, T /)
N1(:,5) = (/ F, T, F /)
case(9)
N1(:,1) = (/ F, T, F /)
N1(:,2) = (/ T, F, T /)
N1(:,3) = (/ F, T, T /)
N1(:,4) = (/ F, F, T /)
N1(:,5) = (/ T, T, F /)
end select !}}}

  do j = 1, 5
    do i = 1, 3
      if (N1(i,j)) then; call fb_pixel(x-1+i,y-1+j,px)
      elseif (BG) then; call fb_pixel(x-1+i,y-1+j,bpx); endif
    enddo
  enddo

elseif (s.eq.2) then  
select case (n) !{{{
case(0)
N2(:,1) = (/ F, T, T, F /)
N2(:,2) = (/ T, F, F, T /)
N2(:,3) = (/ T, F, F, T /)
N2(:,4) = (/ T, F, F, T /)
N2(:,5) = (/ T, F, F, T /)
N2(:,6) = (/ T, F, F, T /)
N2(:,7) = (/ F, T, T, F /)
case(1)
N2(:,1) = (/ F, T, F, F /)
N2(:,2) = (/ F, T, F, F /)
N2(:,3) = (/ F, T, F, F /)
N2(:,4) = (/ F, T, F, F /)
N2(:,5) = (/ F, T, F, F /)
N2(:,6) = (/ F, T, F, F /)
N2(:,7) = (/ F, T, F, F /)
case(2)
N2(:,1) = (/ F, T, T, F /)
N2(:,2) = (/ T, F, F, T /)
N2(:,3) = (/ F, F, F, T /)
N2(:,4) = (/ F, F, T, F /)
N2(:,5) = (/ F, T, F, F /)
N2(:,6) = (/ T, F, F, F /)
N2(:,7) = (/ T, T, T, T /)
case(3)
N2(:,1) = (/ T, T, T, F /)
N2(:,2) = (/ F, F, F, T /)
N2(:,3) = (/ F, F, F, T /)
N2(:,4) = (/ F, T, T, F /)
N2(:,5) = (/ F, F, F, T /)
N2(:,6) = (/ F, F, F, T /)
N2(:,7) = (/ T, T, T, F /)
case(4)
N2(:,1) = (/ T, F, T, F /)
N2(:,2) = (/ T, F, T, F /)
N2(:,3) = (/ T, F, T, F /)
N2(:,4) = (/ T, T, T, T /)
N2(:,5) = (/ F, F, T, F /)
N2(:,6) = (/ F, F, T, F /)
N2(:,7) = (/ F, F, T, F /)
case(5)
N2(:,1) = (/ T, T, T, T /)
N2(:,2) = (/ T, F, F, F /)
N2(:,3) = (/ T, F, F, F /)
N2(:,4) = (/ T, T, T, F /)
N2(:,5) = (/ F, F, F, T /)
N2(:,6) = (/ T, F, F, T /)
N2(:,7) = (/ F, T, T, F /)
case(6)
N2(:,1) = (/ F, F, T, T /)
N2(:,2) = (/ F, T, F, F /)
N2(:,3) = (/ T, F, F, F /)
N2(:,4) = (/ T, T, T, F /)
N2(:,5) = (/ T, F, F, T /)
N2(:,6) = (/ T, F, F, T /)
N2(:,7) = (/ F, T, T, F /)
case(7)
N2(:,1) = (/ T, T, T, T /)
N2(:,2) = (/ F, F, F, T /)
N2(:,3) = (/ F, F, F, T /)
N2(:,4) = (/ F, F, T, F /)
N2(:,5) = (/ F, F, T, F /)
N2(:,6) = (/ F, F, T, F /)
N2(:,7) = (/ F, F, T, F /)
case(8)
N2(:,1) = (/ F, T, T, F /)
N2(:,2) = (/ T, F, F, T /)
N2(:,3) = (/ T, F, F, T /)
N2(:,4) = (/ F, T, T, F /)
N2(:,5) = (/ T, F, F, T /)
N2(:,6) = (/ T, F, F, T /)
N2(:,7) = (/ F, T, T, F /)
case(9)
N2(:,1) = (/ F, T, T, F /)
N2(:,2) = (/ T, F, F, T /)
N2(:,3) = (/ T, F, F, T /)
N2(:,4) = (/ F, T, T, T /)
N2(:,5) = (/ F, F, F, T /)
N2(:,6) = (/ F, F, T, F /)
N2(:,7) = (/ T, T, F, F /)
end select !}}}

  do j = 1, 7
    do i = 1, 4
      if (N2(i,j)) then; call fb_pixel(x-1+i,y-1+j,px)
      elseif (BG) then; call fb_pixel(x-1+i,y-1+j,bpx); endif
    enddo
  enddo
elseif (s.eq.3) then  
select case (n) !{{{
case(0)
N3(:,1) = (/ F, F, T, F, F /)
N3(:,2) = (/ F, T, F, T, F /)
N3(:,3) = (/ T, F, F, F, T /)
N3(:,4) = (/ T, F, F, F, T /)
N3(:,5) = (/ T, F, F, F, T /)
N3(:,6) = (/ T, F, F, F, T /)
N3(:,7) = (/ T, F, F, F, T /)
N3(:,8) = (/ F, T, F, T, F /)
N3(:,9) = (/ F, F, T, F, F /)
case(1)
N3(:,1) = (/ F, F, T, F, F /)
N3(:,2) = (/ F, T, T, F, F /)
N3(:,3) = (/ F, F, T, F, F /)
N3(:,4) = (/ F, F, T, F, F /)
N3(:,5) = (/ F, F, T, F, F /)
N3(:,6) = (/ F, F, T, F, F /)
N3(:,7) = (/ F, F, T, F, F /)
N3(:,8) = (/ F, F, T, F, F /)
N3(:,9) = (/ F, T, T, T, F /)
case(2)
N3(:,1) = (/ F, T, T, T, F /)
N3(:,2) = (/ T, F, F, F, T /)
N3(:,3) = (/ F, F, F, F, T /)
N3(:,4) = (/ F, F, F, T, F /)
N3(:,5) = (/ F, F, T, F, F /)
N3(:,6) = (/ F, T, F, F, F /)
N3(:,7) = (/ T, F, F, F, F /)
N3(:,8) = (/ T, F, F, F, F /)
N3(:,9) = (/ T, T, T, T, T /)
case(3)
N3(:,1) = (/ F, T, T, T, F /)
N3(:,2) = (/ T, F, F, F, T /)
N3(:,3) = (/ F, F, F, F, T /)
N3(:,4) = (/ F, F, F, F, T /)
N3(:,5) = (/ F, T, T, T, F /)
N3(:,6) = (/ F, F, F, F, T /)
N3(:,7) = (/ F, F, F, F, T /)
N3(:,8) = (/ T, F, F, F, T /)
N3(:,9) = (/ F, T, T, T, F /)
case(4)
N3(:,1) = (/ F, F, F, T, F /)
N3(:,2) = (/ F, F, T, T, F /)
N3(:,3) = (/ F, T, F, T, F /)
N3(:,4) = (/ T, F, F, T, F /)
N3(:,5) = (/ T, T, T, T, T /)
N3(:,6) = (/ F, F, F, T, F /)
N3(:,7) = (/ F, F, F, T, F /)
N3(:,8) = (/ F, F, F, T, F /)
N3(:,9) = (/ F, F, F, T, F /)
case(5)
N3(:,1) = (/ T, T, T, T, T /)
N3(:,2) = (/ T, F, F, F, F /)
N3(:,3) = (/ T, F, F, F, F /)
N3(:,4) = (/ T, F, F, F, F /)
N3(:,5) = (/ T, T, T, T, F /)
N3(:,6) = (/ F, F, F, F, T /)
N3(:,7) = (/ F, F, F, F, T /)
N3(:,8) = (/ T, F, F, F, T /)
N3(:,9) = (/ F, T, T, T, F /)
case(6)
N3(:,1) = (/ F, F, T, T, F /)
N3(:,2) = (/ F, T, F, F, F /)
N3(:,3) = (/ T, F, F, F, F /)
N3(:,4) = (/ T, F, F, F, F /)
N3(:,5) = (/ T, T, T, T, F /)
N3(:,6) = (/ T, F, F, F, T /)
N3(:,7) = (/ T, F, F, F, T /)
N3(:,8) = (/ T, F, F, F, T /)
N3(:,9) = (/ F, T, T, T, F /)
case(7)
N3(:,1) = (/ T, T, T, T, T /)
N3(:,2) = (/ F, F, F, F, T /)
N3(:,3) = (/ F, F, F, F, T /)
N3(:,4) = (/ F, F, F, T, F /)
N3(:,5) = (/ F, F, F, T, F /)
N3(:,6) = (/ F, F, T, F, F /)
N3(:,7) = (/ F, F, T, F, F /)
N3(:,8) = (/ F, F, T, F, F /)
N3(:,9) = (/ F, F, T, F, F /)
case(8)
N3(:,1) = (/ F, T, T, T, F /)
N3(:,2) = (/ T, F, F, F, T /)
N3(:,3) = (/ T, F, F, F, T /)
N3(:,4) = (/ T, F, F, F, T /)
N3(:,5) = (/ F, T, T, T, F /)
N3(:,6) = (/ T, F, F, F, T /)
N3(:,7) = (/ T, F, F, F, T /)
N3(:,8) = (/ T, F, F, F, T /)
N3(:,9) = (/ F, T, T, T, F /)
case(9)
N3(:,1) = (/ F, T, T, T, F /)
N3(:,2) = (/ T, F, F, F, T /)
N3(:,3) = (/ T, F, F, F, T /)
N3(:,4) = (/ T, F, F, F, T /)
N3(:,5) = (/ F, T, T, T, T /)
N3(:,6) = (/ F, F, F, F, T /)
N3(:,7) = (/ F, F, F, F, T /)
N3(:,8) = (/ F, F, F, T, F /)
N3(:,9) = (/ F, T, T, F, F /)
end select !}}}

  do j = 1, 9
    do i = 1, 5
      if (N3(i,j)) then; call fb_pixel(x-1+i,y-1+j,px)
      elseif (BG) then; call fb_pixel(x-1+i,y-1+j,bpx); endif
    enddo
  enddo
else
  write(0,*) "fb_printNumber: ERROR: unsupported font size, ",s
  STOP
endif

end subroutine fb_printNumber !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_printString(str, x, y, s, px, bpx)
!   print a monochrome alphastring, str, at location, x, y, with size, s, and color, px.
subroutine fb_printString(str, x, y, s, px, bpx) !{{{
implicit none
integer, intent(in) :: x, y, s
character(4), intent(in) :: px
character(4), optional, intent(in) :: bpx
character(*), intent(in) :: str
character(1) :: n
logical, dimension(5,5) :: L1
logical :: T, F, BG
integer :: i, j, l

T = .true.
F = .false.

if (present(bpx)) then; BG=.true.
else; BG=.false.; endif

if (s.eq.1) then
 do l = 1, len(str)
 n = str(l:l)
select case (n) !{{{
case(char(16)) ! theta (narrow)
L1(:,1) = (/ F, F, T, T, F /)
L1(:,2) = (/ F, T, F, F, T /)
L1(:,3) = (/ F, T, T, T, T /)
L1(:,4) = (/ F, T, F, F, T /)
L1(:,5) = (/ F, F, T, T, F /)
case(char(17)) ! phi  (stupidly triangular)
L1(:,1) = (/ F, F, T, F, F /)
L1(:,2) = (/ F, T, T, T, F /)
L1(:,3) = (/ T, F, T, F, T /)
L1(:,4) = (/ F, T, T, T, F /)
L1(:,5) = (/ F, F, T, F, F /)
case(char(18)) ! pi (lower case)
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ T, T, T, T, T /)
L1(:,3) = (/ F, T, F, T, F /)
L1(:,4) = (/ F, T, F, T, F /)
L1(:,5) = (/ F, T, F, T, T /)
case(' ')
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, F, F, F, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, F, F, F, F /)
L1(:,5) = (/ F, F, F, F, F /)
case('(')
L1(:,1) = (/ F, F, F, T, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, F, T, F /)
case(')')
L1(:,1) = (/ F, T, F, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, T, F, F, F /)
case('.')
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, F, F, F, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, F, F, F /)
case(':')
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, F, F, F /)
case(',')
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, F, F, F, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, T, F, F, F /)
case('=')
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, T, T, T, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, T, T, T, F /)
L1(:,5) = (/ F, F, F, F, F /)
case('-')
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, F, F, F, F /)
L1(:,3) = (/ F, T, T, T, F /)
L1(:,4) = (/ F, F, F, F, F /)
L1(:,5) = (/ F, F, F, F, F /)
case('1')
L1(:,1) = (/ F, F, T, F, F /)
L1(:,2) = (/ F, T, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, T, T, T, F /)
case('2')
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ F, F, T, T, F /)
L1(:,4) = (/ F, T, F, F, F /)
L1(:,5) = (/ T, T, T, T, T /)
case('3')
L1(:,1) = (/ T, T, T, T, F /)
L1(:,2) = (/ F, F, F, F, T /)
L1(:,3) = (/ F, T, T, T, F /)
L1(:,4) = (/ F, F, F, F, T /)
L1(:,5) = (/ T, T, T, T, F /)
case('4')
L1(:,1) = (/ T, F, F, T, F /)
L1(:,2) = (/ T, F, F, T, F /)
L1(:,3) = (/ T, T, T, T, T /)
L1(:,4) = (/ F, F, F, T, F /)
L1(:,5) = (/ F, F, F, T, F /)
case('5')
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ F, F, F, F, T /)
L1(:,5) = (/ T, T, T, T, F /)
case('6')
L1(:,1) = (/ F, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ F, T, T, T, F /)
case('7')
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ F, F, F, T, F /)
L1(:,3) = (/ F, F, F, T, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, T, F, F /)
case('8')
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ F, T, T, T, F /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ F, T, T, T, F /)
case('9')
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ F, T, T, T, T /)
L1(:,4) = (/ F, F, F, F, T /)
L1(:,5) = (/ F, F, T, T, F /)
case('A','a')
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, T, T, T, T /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ T, F, F, F, T /)
case('B','b')
L1(:,1) = (/ T, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ T, T, T, T, F /)
case('C','c')
L1(:,1) = (/ F, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, F, F, F, F /)
L1(:,4) = (/ T, F, F, F, F /)
L1(:,5) = (/ F, T, T, T, T /)
case('D','d')
L1(:,1) = (/ T, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, F, F, F, T /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ T, T, T, T, F /)
case('E','e')
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ T, F, F, F, F /)
L1(:,5) = (/ T, T, T, T, T /)
case('F','f')
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ T, F, F, F, F /)
L1(:,5) = (/ T, F, F, F, F /)
case('G','g')
L1(:,1) = (/ F, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, F, F, T, T /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ F, T, T, T, T /)
case('H','h')
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, T, T, T, T /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ T, F, F, F, T /)
case('I','i')
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ T, T, T, T, T /)
case('J','j')
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ T, T, F, F, F /)
case('K','k')
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, F, F, T, F /)
L1(:,3) = (/ T, T, T, F, F /)
L1(:,4) = (/ T, F, F, T, F /)
L1(:,5) = (/ T, F, F, F, T /)
case('L','l')
L1(:,1) = (/ T, F, F, F, F /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, F, F, F, F /)
L1(:,4) = (/ T, F, F, F, F /)
L1(:,5) = (/ T, T, T, T, T /)
case('M','m')
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, T, F, T, T /)
L1(:,3) = (/ T, F, T, F, T /)
L1(:,4) = (/ T, F, T, F, T /)
L1(:,5) = (/ T, F, F, F, T /)
case('N','n')
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, T, F, F, T /)
L1(:,3) = (/ T, F, T, F, T /)
L1(:,4) = (/ T, F, F, T, T /)
L1(:,5) = (/ T, F, F, F, T /)
case('O','o','0')
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, F, F, F, T /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ F, T, T, T, F /)
case('P','p')
L1(:,1) = (/ T, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ T, F, F, F, F /)
L1(:,5) = (/ T, F, F, F, F /)
case('Q','q')
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, F, T, F, T /)
L1(:,4) = (/ T, F, F, T, T /)
L1(:,5) = (/ F, T, T, T, T /)
case('R','r')
L1(:,1) = (/ T, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ T, F, F, T, F /)
L1(:,5) = (/ T, F, F, F, T /)
case('S','s')
L1(:,1) = (/ F, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ F, T, T, T, F /)
L1(:,4) = (/ F, F, F, F, T /)
L1(:,5) = (/ T, T, T, T, F /)
case('T','t')
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, T, F, F /)
case('U','u')
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, F, F, F, T /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ F, T, T, T, F /)
case('V','v')
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ F, T, F, T, F /)
L1(:,4) = (/ F, T, F, T, F /)
L1(:,5) = (/ F, F, T, F, F /)
case('W','w')
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, F, T, F, T /)
L1(:,3) = (/ T, F, T, F, T /)
L1(:,4) = (/ T, T, F, T, T /)
L1(:,5) = (/ T, F, F, F, T /)
case('X','x')
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ F, T, F, T, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, T, F, T, F /)
L1(:,5) = (/ T, F, F, F, T /)
case('Y','y')
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ F, T, F, T, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, T, F, F /)
case('Z','z')
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ F, F, F, T, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, T, F, F, F /)
L1(:,5) = (/ T, T, T, T, T /)
end select !}}}

  do j = 1, 5
    do i = 1, 5
      if (L1(i,j)) then; call fb_pixel(x-7+i+6*l,y-1+j,px)
      elseif (BG) then; call fb_pixel(x-7+i+6*l,y-1+j,bpx); endif
    enddo
  enddo
 enddo
else
  write(0,*) "fb printLetter: ERROR: unsupported font size, ",s
  STOP
endif
end subroutine fb_printString !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80

end module fbMod
