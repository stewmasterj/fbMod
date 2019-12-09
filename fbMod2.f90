! vim:fdm=marker
! Fortran module for interacting with the framebuffer /dev/fb0
!  and pixel map objects.
! August 2019
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
Module fbMod
implicit none

! pixel buffer type
type PixBuffType
 integer :: w, h, len, lline
 character(len=:), allocatable :: pb !(BGRA)
 logical :: Lzbuff
 real(4), allocatable, dimension(:,:) :: zbuff
contains
 procedure :: init => pb_initPixBuff
 procedure :: save => pb_dump
 procedure :: getrec 
 procedure :: loadPPM => pb_PPM2PixBuff
 procedure :: putPixel => pb_pixel
 procedure :: getPixel => pb_getPixel
 procedure :: clear => pb_clear
 procedure :: fillTriangle => pb_fillTriangle
 procedure :: fillTriangle3c => pb_fillTriangle3c
 procedure :: line => pb_line
 procedure :: line2c => pb_line2c
 procedure :: linePolar => pb_linepolar
 procedure :: linePolar2 => pb_linepolar2
 procedure :: rec => pb_rec
 procedure :: fillRec => pb_fillRec
 procedure :: putPixBuff => pb_putpic
 procedure :: getPixBuff => pb_getpic
 procedure :: blurRec => pb_blurRec
 procedure :: HSVScaleRec => pb_HSVScaleRec
 procedure :: circle => pb_circle
 procedure :: fillCircle => pb_fillcircle
 procedure :: sphere => pb_sphere
 procedure :: plot => pb_plot
 procedure :: mplot => pb_mplot
 procedure :: heatPlot => pb_heatPlot
 procedure :: matrixPlot => pb_matrixPlot
 procedure :: matrixPlot4 => pb_matrixPlot4
 procedure :: putNumber => pb_printNumber
 procedure :: putString => pb_printString
end type

! frame buffer is a pixel buffer type but with some extra stuff
type, extends(PixBuffType) :: FrameBufferType
 integer :: FID !, w, h, line
 character(80) :: devicePath !, mode
! character(len=:), allocatable :: pb !pixel buffer: full frame buffer
 ! the fb_line2c and fb_filltriangle3c routines can use the Z-Buffer
contains
 procedure :: fbinit => fb_init  !run this first
 procedure :: close => fb_close  !run this last
 procedure :: display => fb_write
 procedure :: loadScreen => fb_read
end type
type(FrameBufferType) :: fb

! procedural texture type for texturing triangles
type ProcTexType
 integer :: typ !texture pattern type
 ! parameters are unique to the texture pattern type
 integer, dimension(4) :: ip !integer parameters
 real(4), dimension(4) :: rp !real parameters
end type


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fb_init(i,dev)  set variables and open framebuffer device
!    i   file descriptor
!    dev  device path of framebuffer, usually /dev/fb0
subroutine fb_init(fbo, i,dev, wd,ht,ln, zbu) !{{{
implicit none
class(FrameBufferType) :: fbo
integer :: i
character(*) :: dev
integer, optional, intent(in) :: wd,ht,ln
logical, optional, intent(in) :: zbu

fbo%FID = i
fbo%devicePath = trim(dev)
!fb%Lbuff = .true.
!fb%mode = trim(mode)
! this can be found with fbset
fbo%w  = 1440 !width
fbo%h =  900  !height
fbo%lline   = 1472 !for some reason line length is not the same as width, WTF?
if (present(wd)) fbo%w = wd
if (present(ht)) fbo%h = ht
if (present(ln)) fbo%lline = ln !overwrite the initialization

fbo%Lzbuff = .false. !use Z-buffer?
!if (present(zbu)) fbo%Lzbuff = zbu
! mode options:
!   direct    each function write is directly to frame buffer device
!   buffer    each function write is to the buffer: "fb%pxbuff"
!              this mode must be explicitly written to the device.
!if (trim(mode).eq."direct") then
!   fb%Lbuff = .false.
!! 32 bits per pixel means 4 bytes per pixel
!! each record in "file" is a pixel
!   open(fb%FID,file=fb%devicePath,ACCESS='DIRECT',RECL=4,FORM='UNFORMATTED')
!elseif (trim(mode).eq."buffer") then
if (present(zbu)) then;   call fbo%init( fbo%w, fbo%h, fbo%lline, zbu )
else;                     call fbo%init( fbo%w, fbo%h, fbo%lline )
endif

! open as stream to be written to in one go. fastest rendering option
   open(fbo%FID,file=fbo%devicePath,ACCESS='STREAM',FORM='UNFORMATTED')
!else
!   write(0,*) "ERROR: fb_init: no mode option: "//trim(mode)
!   STOP
!endif

end subroutine fb_init !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine fb_write( fb ) !{{{
! write buffer to framebuffer device file to render to screen
class(FrameBufferType) :: fb
rewind(fb%FID)
write(fb%FID) fb%pb
end subroutine fb_write !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine fb_read( fb ) !{{{
! read framebuffer device file to to buffer
class(FrameBufferType) :: fb
rewind(fb%FID)
read(fb%FID) fb%pb
end subroutine fb_read !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine pb_dump(pb,FL,mode) !{{{
class(PixBuffType) :: pb
character(*), intent(in) :: FL
integer, intent(in) :: mode
! mode : description
!  1     full buffer dump, as BGRA and excess columns
!  2     full screen binary P6-PPM
!  3     only non NULL pixels printed in ASCII: X Y R G B
integer :: i, k, j, b, g, r, x, y, itmp
character(80) :: frmtstr
character :: rgb(3,pb%lline,pb%h)
open(11,file=trim(FL))
select case(mode)
  case(1); write(11) pb%pb
  case(2);
    !write(11,'(''P6'', 2(1x,i4),'' 255 '',$)') fb%lline, fb%height
    write(11,'(A2)') "P6"
    write(11,'(2(1x,i4))') pb%lline, pb%h
    write(11,'(i3)') 255
    itmp = pb%lline*pb%h*3
    !write(frmtstr,'(''('',i8.8,''A,$)'')') itmp
    write(frmtstr,'(''('',i8.8,''A)'')') itmp
    do i = 1, pb%lline*pb%h
      k = i*4-3
      y = int(real(i-1)/real(pb%lline)) +1
      x = i -(y-1)*pb%lline
if (x.le.0 .or. y.le.0) then
  write(0,*) "ERROR: pixel point of bounds (x,y)=",x, y, i, k
  STOP
endif
      rgb(3,x,y) = pb%pb(k:k)
      rgb(2,x,y) = pb%pb(k+1:k+1)
      rgb(1,x,y) = pb%pb(k+2:k+2)
    enddo
    write(11,fmt=frmtstr,advance="no") (((rgb(k,i,j),k=1,3),i=1,pb%lline),j=1,pb%h)
  case(3); 
    do i = 1, pb%lline*pb%h
      k = i*4-3
      b = ichar(pb%pb(k:k))
      g = ichar(pb%pb(k+1:k+1))
      r = ichar(pb%pb(k+2:k+2))
      if (b.eq.0 .and. g.eq.0 .and. r.eq.0) cycle
      y = int(real(i)/real(pb%lline)) +1
      x = i -(y-1)*pb%lline
      write(11,'(5i5)') x, y, r, g, b
    enddo
end select
close(11)
end subroutine pb_dump !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine pb_PPM2PixBuff( pb, fil ) !{{{
implicit none
class(PixBuffType) :: pb
character(*) :: fil
character(1) :: c
character(2) :: ty
character(20) :: c20 !frmtstr
integer :: w, h, i, k !, itmp
character, allocatable :: rgb(:,:)
open(11,file=trim(fil),ACCESS='STREAM',FORM='UNFORMATTED')
read(11)  ty, c !the type(2) and 0a
!write(0,'(3(z2,x))') ty(1:1), ty(2:2), c; call flush(0)
c20 = ""; k=1
do i = 1, 20
 read(11) c !read one byte at a time
 if (c.eq.char(10)) then !new line
   k = i; exit; endif ! done with the width height line
 c20(i:i) = c
enddo
read(c20(1:k),*) w, h
!write(0,'(20(z2,x),x,2i6)') (c20(i:i),i=1,20), w, h; call flush(0)
c20 = ""
do i = 1, 20
 read(11) c !read one byte at a time
 if (c.eq.char(10)) then !new line
   k = i; exit; endif ! done with the bit depth
 c20(i:i) = c
enddo
read(c20,*) k
!write(0,'(20(z2,x),x,i6)') (c20(i:i),i=1,20), k; call flush(0)
!read(11,*)  w, h
!read(11,*) ! 255
 call pb%init( w, h, w )
! itmp = w*h*3
 allocate( rgb(3,w*h) ) !assume 24 bit
 !rgb = char(0)
! write(frmtstr,'(''('',i8.8,''A)'')') itmp !this should be 3x w*h
!write(0,*) frmtstr,"  ",ty, w, h, i, "    ", itmp
!call flush(0)
 !read(11,fmt=frmtstr,advance="no") (rgb(1:3,i),i=1,w*h)
 !read(11,fmt=frmtstr,advance="no") (rgb(1:3,i),i=1,w*h)
! do i = 1, w*h
!  read(11,fmt='(3A)',advance="no",iostat=er) rgb(1:3,i)
!  if (er.ne.0) then
!   write(0,'(a,i3,a,i6,x,3(z2,x),x,3(z2,x))') "ERROR: ",er," Pixel ", i, &
!      & iachar(rgb(1,i-1)), iachar(rgb(2,i-1)), iachar(rgb(3,i-1)),  &
!      & iachar(rgb(1,i)),   iachar(rgb(2,i)),   iachar(rgb(3,i)) 
!   call flush(0)
!  endif
! enddo
 read(11) ((rgb(k,i),k=1,3),i=1,w*h)
 do i = 1, w*h !read each pixel
  k = i*4-3
  pb%pb(k:k)     = rgb(3,i)
  pb%pb(k+1:k+1) = rgb(2,i)
  pb%pb(k+2:k+2) = rgb(1,i)
 enddo
close(11)
end subroutine pb_PPM2PixBuff !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine fb_close( fb ) !{{{
class(FrameBufferType) :: fb
close(fb%FID)
end subroutine fb_close !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine pb_initPixBuff( pb, w, h, l, zb ) !{{{
class(PixBuffType) :: pb
integer :: w, h, l
logical, optional, intent(in) :: zb
pb%w = w
pb%h = h
pb%lline = l !for compatibility with framebuffer object, should be 'w'
pb%len = h*l
if (allocated( pb%pb )) deallocate( pb%pb )
   allocate( character(len=4*pb%len) :: pb%pb )

pb%Lzbuff = .false. !use Z-buffer?
if (present(zb)) pb%Lzbuff = zb !use Z-buffer?
if (allocated( pb%zbuff )) deallocate( pb%zbuff )
if (pb%Lzbuff) allocate( pb%zbuff(pb%w, pb%h) )
!if (.not.pb%Lzbuff) write(0,*) "pb_init:Zbuffer not activated"
call pb%clear
end subroutine pb_initPixBuff !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! returns the 4Byte record number for an XY screen position.
!  or the byte offset in "buffer" mode.
integer function getrec(pb,x,y) !{{{
class(PixBuffType) :: pb
integer :: x, y
getrec = (y-1)*pb%lline + x  !pixel location
getrec = getrec*4-3    !byte location
end function getrec !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine pb_pixel(pb, x,y,px) !{{{
implicit none
class(PixBuffType) :: pb
integer, intent(in) :: x, y
integer :: k
character(4), intent(in) :: px
!if (fb%Lbuff) then
 k = pb%getrec(x,y)
 pb%pb(k:k+3) = px
!else
! write(fb%FID,REC=getrec(x,y)) px
!endif
end subroutine !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine pb_getPixel(pb, x,y,px) !{{{
implicit none
class(PixBuffType) :: pb
integer, intent(in) :: x, y
integer :: k
character(4), intent(out) :: px
k = pb%getrec(x,y)
!if (pb%Lbuff) then
 px = pb%pb(k:k+3)
!else
! read(fb%FID,REC=k) px
!endif
end subroutine pb_getPixel !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine pb_clear( pb, b ) !{{{
class(PixBuffType) :: pb
integer, optional, intent(in) :: b
integer :: bb
bb = 0 !default byte to write is NULL
if (present(b)) bb=b
pb%pb = repeat(char(bb),4*pb%lline*pb%h)
if (pb%Lzbuff) pb%zbuff = -1.0 !clear Z-Buffer as well (-1 is INF)
end subroutine pb_clear !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine RGB2HSV( r,g,b, h,s,v ) !{{{
! from: https://www.cs.rit.edu/~ncs/color/t_convert.html
implicit none 
integer, intent(in) :: r, g, b  !range 0-255
integer, intent(out) :: h, s, v !
real :: mn, mx, dl, rh

mn = real(min(r, g, b))
mx = real(max(r, g, b))
v = max(r,g,b) !nint(mx) !value

dl = mx-mn

if (mx.gt.1.e-3) then
 s = nint(dl/mx*255.0)
else
 s = 0
 h = -1
 return
endif

if (r==v) then !nint(mx)) then ! if red is max
 rh = real(g-b)/dl
elseif (g==v) then !nint(mx)) then ! if green is max
 rh = (2.0+real(b-r)/dl)
else !if blue is max
 rh = (4.0+real(r-g)/dl)
endif

rh = rh * 60.0 !degrees
if (rh.lt.0.0) rh = rh + 360.0
h = nint(rh *255./360.)

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
subroutine getProcTexture( px1,px2,px3, w, tx, px ) !{{{
! returns the pixel value (px) of a procedural texture pattern (tx)
! for the triangular fractional position (w) with corresponding colours (px123)
implicit none
character(4), intent(in) :: px1, px2, px3
real(4), dimension(3), intent(in) :: w
type(ProcTexType) :: tx
character(4), intent(out) :: px

integer, dimension(3) :: p, r, g, b, a, h, s, v, hsv !for integer colours, each corner
real(4), dimension(3) :: f
real(4) :: ss
integer :: val

r = (/ ichar(px1(3:3)), ichar(px2(3:3)), ichar(px3(3:3)) /)
g = (/ ichar(px1(2:2)), ichar(px2(2:2)), ichar(px3(2:2)) /)
b = (/ ichar(px1(1:1)), ichar(px2(1:1)), ichar(px3(1:1)) /)
a = (/ ichar(px1(4:4)), ichar(px2(4:4)), ichar(px3(4:4)) /)

f = w
select case (tx%typ)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(1) ! stripes from node 1 to 2 ( nstripes = tx%ip(1) )
 ! modify the w(1) parameter to produce stripes between point 1 and the others.
 f(1) = real(tx%ip(1))*w(1)
 f(1) = f(1) - int(f(1))
 ! one component is changed, but scale the other two for sum(f) = 1.0
 ! f1+s*f2+s*f3=1 : solve for s
 ! f1+s*(f2+f3)=1 : s = (1-f1)/(f2+f3)
 ss = (1.0-f(1))/sum(f(2:3))
 f(2:3) = ss*f(2:3)
 px(1:1) = char(nint( b(1)*f(1) +b(2)*f(2) +b(3)*f(3) ))
 px(2:2) = char(nint( g(1)*f(1) +g(2)*f(2) +g(3)*f(3) ))
 px(3:3) = char(nint( r(1)*f(1) +r(2)*f(2) +r(3)*f(3) ))
 px(4:4) = char(nint( a(1)*f(1) +a(2)*f(2) +a(3)*f(3) ))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(2) ! checkerboard half gradient from node 1 to 2 and 3 to 2 (nstripes = tx%ip(1:2))
 ! modify the w(1:2) parameters to produce checkerboard pattern.
 f(1:2) = real(tx%ip(1:2))*w(1:2)
 f(1:2) = f(1:2) - int(f(1:2))
 ! two components are changed, but scale the other two for sum(f) = 1.0
 ! s*(f1+f2)+f3=1 : solve for:  s = (1-f3)/(f2+f1)
 ss = (1.0-f(3))/sum(f(1:2))
 f(1:2) = ss*f(1:2)
 px(1:1) = char(nint( b(1)*f(1) +b(2)*f(2) +b(3)*f(3) ))
 px(2:2) = char(nint( g(1)*f(1) +g(2)*f(2) +g(3)*f(3) ))
 px(3:3) = char(nint( r(1)*f(1) +r(2)*f(2) +r(3)*f(3) ))
 px(4:4) = char(nint( a(1)*f(1) +a(2)*f(2) +a(3)*f(3) ))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(3) ! checkerboard triangle half gradient (nstripes = tx%ip(1:3))
 ! modify the w(1:3) parameters to produce checkerboard pattern.
 f = real(tx%ip(1:3))*w
 f = f - int(f)
 ! all components are changed, so scale them for sum(f) = 1.0
 ! s*(f1+f2+f3)=1 : solve for:  s = 1.0/sum(f)
 ss = 1.0/sum(f)
 f = ss*f
 px(1:1) = char(nint( b(1)*f(1) +b(2)*f(2) +b(3)*f(3) ))
 px(2:2) = char(nint( g(1)*f(1) +g(2)*f(2) +g(3)*f(3) ))
 px(3:3) = char(nint( r(1)*f(1) +r(2)*f(2) +r(3)*f(3) ))
 px(4:4) = char(nint( a(1)*f(1) +a(2)*f(2) +a(3)*f(3) ))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(4) ! inner boarder along 1-2-3 not 1-3 edges.
 ! boarder fraction outer tx%rp(1), hue&val of px1
 ! boarder fraction inner tx%rp(2), hue&val of px2
 ! inner block is hue&val of px3
 call RGB2HSV(r(1),g(1),b(1), h(1),s(1),v(1))
 call RGB2HSV(r(2),g(2),b(2), h(2),s(2),v(2))
 call RGB2HSV(r(3),g(3),b(3), h(3),s(3),v(3))
 val = s(1)*w(1) +s(2)*w(2) +s(3)*w(3)
 if (w(1).lt.tx%rp(1) .or. w(3).lt.tx%rp(1)) then
  call HSV2RGB(h(1),val,v(1), p(1),p(2),p(3))
  px = char(p(1))//char(p(2))//char(p(3))//px1(4:4)
 elseif (w(1).lt.tx%rp(2) .or. w(3).lt.tx%rp(2)) then
  call HSV2RGB(h(2),val,v(2), p(1),p(2),p(3))
  px = char(p(1))//char(p(2))//char(p(3))//px2(4:4)
 else
  call HSV2RGB(h(3),val,v(3), p(1),p(2),p(3))
  px = char(p(1))//char(p(2))//char(p(3))//px3(4:4)
 endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(5) !inner scribed ellipse?
 ! center of circle at w=(0.5,0.0,0.5)
 ! circle tangents at w=(0.5,0.0,1.0), w=(1.0,0.5,0.0)
!1\
 ! \
 !  \
 !   A
 !C   \
 !     \
!2__U___\3
 f = w-(/ 0.5, 0.0, 0.5 /) ! current - center
 !ss = sum(f*f) ! sum of squares
 ss = f(1)*f(1)+f(3)*f(3)
 if (ss .lt. tx%rp(1)*tx%rp(1)) then ! in circle?
  ! blend node 2 and 3
  ss = ss/(tx%rp(1)*tx%rp(1)) !normalize to 1.0
 px(1:1) = char(nint(b(2)*ss +b(3)*(1.0-ss) ))
 px(2:2) = char(nint(g(2)*ss +g(3)*(1.0-ss) ))
 px(3:3) = char(nint(r(2)*ss +r(3)*(1.0-ss) ))
 px(4:4) = char(nint(a(2)*ss +a(3)*(1.0-ss) ))
 else
  px = px1
 endif 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(6) !Perlin Noise for saturation and value, not Hue
 ! requires a 2D vector w(1,3)
 ! frequency: tx%rp(1)
 ! amplitude: tx%rp(2)
 ! weight the vertext colors together, like default
 p(1) = nint( b(1)*w(1) +b(2)*w(2) +b(3)*w(3) )
 p(2) = nint( g(1)*w(1) +g(2)*w(2) +g(3)*w(3) )
 p(3) = nint( r(1)*w(1) +r(2)*w(2) +r(3)*w(3) )
 ! convert to HSV
 call RGB2HSV(p(1),p(2),p(3), hsv(1),hsv(2),hsv(3))
 ! modify the saturation and value by noise
 !hsv(2:3) = hsv(2:3) + nint(tx%rp(2)*(noise((/ w(1), w(3) /), tx%rp(1))-0.5))
 hsv(2:3) = hsv(2:3) + nint(tx%rp(2)*(random((/ w(1), w(3) /))-0.5))
 hsv(2) = min(max(hsv(2),0),255);  hsv(3) = min(max(hsv(3),0),255)
 ! convert HSV back to RGB
 call HSV2RGB(hsv(1),hsv(2),hsv(3), p(1),p(2),p(3))

 px = char(p(1))//char(p(2))//char(p(3))
 px(4:4) = char(nint( a(1)*w(1) +a(2)*w(2) +a(3)*w(3) ))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(7) !fractal brownian motion Perlin Noise for saturation and value, not Hue
 ! requires a 2D vector w(1,3)
 ! frequency: tx%rp(1)
 ! amplitude: tx%rp(2)
 ! weight the vertext colors together, like default
 p(1) = nint( b(1)*w(1) +b(2)*w(2) +b(3)*w(3) )
 p(2) = nint( g(1)*w(1) +g(2)*w(2) +g(3)*w(3) )
 p(3) = nint( r(1)*w(1) +r(2)*w(2) +r(3)*w(3) )
 ! convert to HSV
 call RGB2HSV(p(1),p(2),p(3), hsv(1),hsv(2),hsv(3))
 ! modify the saturation and value by noise
 !hsv(2:3) = hsv(2:3) + nint(tx%rp(2)*(noise((/ w(1), w(3) /), tx%rp(1))-0.5))
 !hsv(2:3) = hsv(2:3) + nint(tx%rp(2)*(random((/ w(1), w(3) /))-0.5))
 hsv(2:3) = hsv(2:3) + nint(tx%rp(4)*(fbm((/ w(1), w(3) /), tx%rp(1), tx%rp(2), tx%ip(1), tx%rp(3), tx%ip(2))-0.5))
 hsv(2) = min(max(hsv(2),0),255);  hsv(3) = min(max(hsv(3),0),255)
 ! convert HSV back to RGB
 call HSV2RGB(hsv(1),hsv(2),hsv(3), p(1),p(2),p(3))

 px = char(p(1))//char(p(2))//char(p(3))
 px(4:4) = char(nint( a(1)*w(1) +a(2)*w(2) +a(3)*w(3) ))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case default !simple interpolation
 px(1:1) = char(nint( b(1)*w(1) +b(2)*w(2) +b(3)*w(3) ))
 px(2:2) = char(nint( g(1)*w(1) +g(2)*w(2) +g(3)*w(3) ))
 px(3:3) = char(nint( r(1)*w(1) +r(2)*w(2) +r(3)*w(3) ))
 px(4:4) = char(nint( a(1)*w(1) +a(2)*w(2) +a(3)*w(3) ))
end select

end subroutine getProcTexture !}}}
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
! pb_filltriangle( ps(2,3), px )
!      Draw a filled trianlge with verticies p1, p2, p3 with color, px
subroutine pb_filltriangle( pb, x1, y1, x2, y2, x3, y3, px ) !{{{
implicit none
class(PixBuffType) :: pb
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
if (ps(2,2) > ps(2,1)) then !only if not horizontal top
do y=ps(2,1), ps(2,2) !only raster down to medium y point
  xs = PointSlope(y,ps(2,1),ps(1,1),ps(2,2),ps(1,2)) !point 1 to 2
  xe = PointSlope(y,ps(2,1),ps(1,1),ps(2,3),ps(1,3)) !point 1 to 3
  do x = min(xs,xe), max(xs,xe)
    r = pb%getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, xs, xe
    !write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',i,';',PS(i,y1,x1,y2,x2),'H', "*"
    !if (fb%Lbuff) then
      pb%pb(r:r+3) = px
    !else
    !  write(fb%FID,REC=r) px
    !endif
  enddo
end do
endif
! upper triangle
if (ps(2,3) > ps(2,2)) then !only if not horizontal top
do y=ps(2,2)+1, ps(2,3) !only raster down from medium y point to bottom
  xs = PointSlope(y,ps(2,2),ps(1,2),ps(2,3),ps(1,3)) !point 2 to 3
  xe = PointSlope(y,ps(2,1),ps(1,1),ps(2,3),ps(1,3)) !point 1 to 3
  do x = min(xs,xe), max(xs,xe)
    r = pb%getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, xs, xe
    !write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',i,';',PS(i,y1,x1,y2,x2),'H', "*"
    !if (fb%Lbuff) then
      pb%pb(r:r+3) = px
    !else
    !  write(fb%FID,REC=r) px
    !endif
  enddo
end do
endif


end subroutine pb_filltriangle !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_filltriangle3c( ps(2,3), px(3), [sb(2,2)] )
!      Draw a filled trianlge with verticies p1, p2, p3 
!      each vertex has color px1, px2, px3, fill colour interpolates
!      within bounds, sb
!      Can use with zbuffer.
subroutine pb_filltriangle3c( pb, x1,y1, x2,y2, x3,y3, px1,px2,px3, sb, z1,z2,z3, tx)!{{{
implicit none
class(PixBuffType) :: pb
integer(4), intent(in) :: x1, x2, x3, y1, y2, y3
character(4), intent(in) :: px1, px2, px3
integer, dimension(2,2), optional, intent(in) :: sb
real(4), optional, intent(in) :: z1,z2,z3
type(ProcTexType), optional, intent(in) :: tx

integer(4), dimension(2,3) :: ps
character(4) :: pxl1, pxl2, pxl3
logical :: texture
integer(4) :: xs, xe, y, r, x, idenom
character(4) :: px
real(4), dimension(3) :: w
integer(4)  :: dy23, dy31, dx32, dx13, dy13, ord(3)
real(4) :: denom, z, zp(3)

texture = .false.
if (present(tx)) texture = .true.

! sort point from top to bottom
if (present(z1)) then !{{{
  if (y1.le.y2 .and.y1.le.y3) then
   ps(:,1) = (/ x1, y1 /); zp(1) = z1; pxl1 = px1; ord(1) = 1
   if (y2<y3) then; ps(:,2)=(/x2,y2/); ps(:,3)=(/x3,y3/); zp(2:3)=(/z2,z3/); pxl2=px2; pxl3=px3; ord(2:3) = (/2,3/)
   else;            ps(:,2)=(/x3,y3/); ps(:,3)=(/x2,y2/); zp(2:3)=(/z3,z2/); pxl2=px3; pxl3=px2; ord(2:3) = (/3,2/)
   endif
  elseif (y2.le.y1 .and.y2.le.y3) then
   ps(:,1) = (/ x2, y2 /); zp(1) = z2; pxl1 = px2; ord(1) = 2
   if (y1<y3) then; ps(:,2)=(/x1,y1/); ps(:,3)=(/x3,y3/); zp(2:3)=(/z1,z3/); pxl2=px1;pxl3=px3; ord(2:3) = (/1,3/)
   else;            ps(:,2)=(/x3,y3/); ps(:,3)=(/x1,y1/); zp(2:3)=(/z3,z1/); pxl2=px3;pxl3=px1; ord(2:3) = (/3,1/)
   endif
  else
   ps(:,1) = (/ x3, y3 /); zp(1) = z3; pxl1 = px3; ord(1) = 3
   if (y1<y2) then; ps(:,2)=(/x1,y1/); ps(:,3)=(/x2,y2/); zp(2:3)=(/z1,z2/); pxl2=px1;pxl3=px2; ord(2:3) = (/1,2/)
   else;            ps(:,2)=(/x2,y2/); ps(:,3)=(/x1,y1/); zp(2:3)=(/z2,z1/); pxl2=px2;pxl3=px1; ord(2:3) = (/2,1/)
   endif
  endif
else
  if (y1.le.y2 .and.y1.le.y3) then
   ps(:,1) = (/ x1, y1 /); pxl1 = px1; ord(1) = 1
   if (y2 < y3) then; ps(:,2) = (/x2,y2/); ps(:,3) = (/x3,y3/); pxl2=px2; pxl3=px3; ord(2:3) = (/2,3/)
   else;              ps(:,2) = (/x3,y3/); ps(:,3) = (/x2,y2/); pxl2=px3; pxl3=px2; ord(2:3) = (/3,2/)
   endif
  elseif (y2.le.y1 .and.y2.le.y3) then
   ps(:,1) = (/ x2, y2 /); pxl1 = px2; ord(1) = 2
   if (y1 < y3) then; ps(:,2) = (/x1,y1/); ps(:,3) = (/x3,y3/); pxl2=px1;pxl3=px3; ord(2:3) = (/1,3/)
   else;              ps(:,2) = (/x3,y3/); ps(:,3) = (/x1,y1/); pxl2=px3;pxl3=px1; ord(2:3) = (/3,1/)
   endif
  else
   ps(:,1) = (/ x3, y3 /); pxl1 = px3 ; ord(1) = 3
   if (y1 < y2) then; ps(:,2) = (/x1,y1/); ps(:,3) = (/x2,y2/); pxl2=px1;pxl3=px2; ord(2:3) = (/1,2/)
   else;              ps(:,2) = (/x2,y2/); ps(:,3) = (/x1,y1/); pxl2=px2;pxl3=px1; ord(2:3) = (/2,1/)
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
    r = pb%getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, xs, xe
    ! calculate each vertex's position weight on (x,y)
    !w(1) = (dy23*(x-x3) + dx32*(y-y3))/denom
    !w(2) = (dy31*(x-x3) + dx13*(y-y3))/denom
    w(1) = (dy23*(x-ps(1,3)) + dx32*(y-ps(2,3)))/denom
    w(2) = (dy31*(x-ps(1,3)) + dx13*(y-ps(2,3)))/denom
    w(3) = 1.0 -w(1) -w(2)
    if (minval(w).lt.0.0) cycle !outside of triangle
    if (pb%Lzbuff) then
      !z = z1*w(1) +z2*w(2) +z3*w(3)
      z = zp(1)*w(1) +zp(2)*w(2) +zp(3)*w(3)
      if (pb%zbuff(x,y).lt.0.0) then
        pb%zbuff(x,y) = z !save this to the Z-buffer and render it
      else
        if (z.lt.pb%zbuff(x,y)) then !then it's behind what is already rendered
          pb%zbuff(x,y) = z !save this to the Z-buffer and render it, since it's the closest
        else
          cycle
        endif
      endif
    endif
    if (.not.texture) then
    ! use the weights to interpolate each RGB value
    !px(1:1) = char(nint( ichar(px1(1:1))*w(1) +ichar(px2(1:1))*w(2) +ichar(px3(1:1))*w(3)))
    !px(2:2) = char(nint( ichar(px1(2:2))*w(1) +ichar(px2(2:2))*w(2) +ichar(px3(2:2))*w(3)))
    !px(3:3) = char(nint( ichar(px1(3:3))*w(1) +ichar(px2(3:3))*w(2) +ichar(px3(3:3))*w(3)))
    px(1:1) = char(nint( ichar(pxl1(1:1))*w(1) +ichar(pxl2(1:1))*w(2) +ichar(pxl3(1:1))*w(3)))
    px(2:2) = char(nint( ichar(pxl1(2:2))*w(1) +ichar(pxl2(2:2))*w(2) +ichar(pxl3(2:2))*w(3)))
    px(3:3) = char(nint( ichar(pxl1(3:3))*w(1) +ichar(pxl2(3:3))*w(2) +ichar(pxl3(3:3))*w(3)))
    else
      ! reorder the weights according to input order not height order
      call getProcTexture( px1,px2,px3, (/w(ord(1)),w(ord(2)),w(ord(3))/), tx, px )
    endif
    !if (fb%Lbuff) then
      pb%pb(r:r+3) = px
    !else
    !  write(fb%FID,REC=r) px
    !endif
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
    r = pb%getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, xs, xe
    ! calculate each vertex's position weight on (x,y)
    !w(1) = (dy23*(x-x3) + dx32*(y-y3))/denom
    !w(2) = (dy31*(x-x3) + dx13*(y-y3))/denom
    w(1) = (dy23*(x-ps(1,3)) + dx32*(y-ps(2,3)))/denom
    w(2) = (dy31*(x-ps(1,3)) + dx13*(y-ps(2,3)))/denom
    w(3) = 1.0 -w(1) -w(2)
    if (minval(w).lt.0.0) cycle !outside of triangle
    if (pb%Lzbuff) then
      !z = z1*w(1) +z2*w(2) +z3*w(3)
      z = zp(1)*w(1) +zp(2)*w(2) +zp(3)*w(3)
      if (pb%zbuff(x,y).lt.0.0) then
        pb%zbuff(x,y) = z !save this to the Z-buffer and render it
      else
        if (z.lt.pb%zbuff(x,y)) then !then it's behind what is already rendered
          pb%zbuff(x,y) = z !save this to the Z-buffer and render it, since it's the closest
        else
          cycle
        endif
      endif
    endif
    if (.not.texture) then
    ! use the weights to interpolate each RGB value
    !px(1:1) = char(nint( ichar(px1(1:1))*w(1) +ichar(px2(1:1))*w(2) +ichar(px3(1:1))*w(3) ))
    !px(2:2) = char(nint( ichar(px1(2:2))*w(1) +ichar(px2(2:2))*w(2) +ichar(px3(2:2))*w(3) ))
    !px(3:3) = char(nint( ichar(px1(3:3))*w(1) +ichar(px2(3:3))*w(2) +ichar(px3(3:3))*w(3) ))
    px(1:1) = char(nint( ichar(pxl1(1:1))*w(1) +ichar(pxl2(1:1))*w(2) +ichar(pxl3(1:1))*w(3) ))
    px(2:2) = char(nint( ichar(pxl1(2:2))*w(1) +ichar(pxl2(2:2))*w(2) +ichar(pxl3(2:2))*w(3) ))
    px(3:3) = char(nint( ichar(pxl1(3:3))*w(1) +ichar(pxl2(3:3))*w(2) +ichar(pxl3(3:3))*w(3) ))
    else
      ! reorder the weights according to input order not height order
      call getProcTexture( px1,px2,px3, (/w(ord(1)),w(ord(2)),w(ord(3))/), tx, px )
    endif
    !if (fb%Lbuff) then
      pb%pb(r:r+3) = px
    !else
    !  write(fb%FID,REC=r) px
    !endif
  enddo
end do
endif


end subroutine pb_filltriangle3c !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_line( x1, y1, x2, y2, px )
!      Draw a line from point to point to the terminal directly
!      Bresenham algorithm
subroutine pb_line(pb, x1,y1,x2,y2,px) !{{{
implicit none
class(PixBuffType) :: pb
integer(kind=4) :: x1,x2,y1,y2, x, y, r, k
character(4) :: px
logical :: LX, LY
LX=.false.
LY=.false.
if ((x2-x1).ne.0) LX=.true.
if ((y2-y1).ne.0) LY=.true.
! check for same starting and end point
if (.not.LX .and. .not.LY) then
  !if (fb%Lbuff) then
    k = pb%getrec(x1,y1)
    pb%pb(k:k+3) = px
  !else
  !  write(fb%FID,REC=getrec(x1,y1)) px
  !endif
  RETURN
endif

if (LX .and. abs(real(y2-y1)).lt.abs(real(x2-x1))) then
  do x=min(x1,x2),max(x1,x2)
    y = PointSlope(x,x1,y1,x2,y2)
    r = pb%getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, x1, y1, x2, y2
    !write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',PS(i,x1,y1,x2,y2),';',i,'H', "*"
    !if (fb%Lbuff) then
      pb%pb(r:r+3) = px
    !else
    !  write(fb%FID,REC=r) px
    !endif
  end do
else
  do y=min(y1,y2),max(y1,y2)
    x = PointSlope(y,y1,x1,y2,x2)
    r = pb%getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, x1, y1, x2, y2
    !write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',i,';',PS(i,y1,x1,y2,x2),'H', "*"
    !if (fb%Lbuff) then
      pb%pb(r:r+3) = px
    !else
    !  write(fb%FID,REC=r) px
    !endif
  end do
endif
end subroutine pb_line !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_line2c( x1, y1, x2, y2, px(2), [sb(2,2)], [z1, z2] )
!      Draw a line from point to point to the terminal directly
!      Bresenham algorithm
!      use 'sb' to check bounds
!      Can use with zbuffer.
subroutine pb_line2c(pb, x1,y1,x2,y2,px1,px2,sb,z1,z2) !{{{
implicit none
class(PixBuffType) :: pb
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
  !if (fb%Lbuff) then
    if (pb%Lzbuff) then
      if (pb%zbuff(x1,y1).gt.0.0.and.min(z1,z2).gt.pb%zbuff(x1,y1)) then !then it's behind what is already rendered
        Return
      else
        pb%zbuff(x1,y1) = min(z1,z2) !save this to the Z-buffer and render it, since it's the closest
      endif
    endif
    k = pb%getrec(x1,y1)
    pb%pb(k:k+3) = px1
  !else
  !  write(fb%FID,REC=getrec(x1,y1)) px1
  !endif
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
    r = pb%getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, x1, y1, x2, y2
    w(1) = real(m2-x)/real(m2-m1) !1-0, full on the minimum, m1
    if (m1.eq.x1) then; w(2) = 1.0-w(1)
    else;  w(2) = w(1); w(1) = 1.0-w(2)
    endif
    if (pb%Lzbuff) then
      z = z1*w(1) +z2*w(2) 
      if (pb%zbuff(x,y).gt.0.0.and.z.gt.pb%zbuff(x,y)) then !then it's behind what is already rendered
        cycle
      else
        pb%zbuff(x,y) = z !save this to the Z-buffer and render it, since it's the closest
      endif
    endif
    ! use the weights to interpolate each RGB value
    px(1:1) = char(nint( ichar(px1(1:1))*w(1) +ichar(px2(1:1))*w(2) ))
    px(2:2) = char(nint( ichar(px1(2:2))*w(1) +ichar(px2(2:2))*w(2) ))
    px(3:3) = char(nint( ichar(px1(3:3))*w(1) +ichar(px2(3:3))*w(2) ))
    !write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',PS(i,x1,y1,x2,y2),';',i,'H', "*"
    !if (fb%Lbuff) then
      pb%pb(r:r+3) = px
    !else
    !  write(fb%FID,REC=r) px
    !endif
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
    r = pb%getrec(x,y)
    if (r.le.0) write(0,*) "fb Byte < 0. (x,y)=",x, y, x1, y1, x2, y2
    w(1) = real(m2-y)/real(m2-m1) !1-0, full on the minimum, m1
    if (m1.eq.y1) then; w(2) = 1.0-w(1)
    else;  w(2) = w(1); w(1) = 1.0-w(2)
    endif
    if (pb%Lzbuff) then
      z = z1*w(1) +z2*w(2) 
      if (pb%zbuff(x,y).gt.0.0.and.z.gt.pb%zbuff(x,y)) then !then it's behind what is already rendered
        cycle
      else
        pb%zbuff(x,y) = z !save this to the Z-buffer and render it, since it's the closest
      endif
    endif
    ! use the weights to interpolate each RGB value
    px(1:1) = char(nint( ichar(px1(1:1))*w(1) +ichar(px2(1:1))*w(2) ))
    px(2:2) = char(nint( ichar(px1(2:2))*w(1) +ichar(px2(2:2))*w(2) ))
    px(3:3) = char(nint( ichar(px1(3:3))*w(1) +ichar(px2(3:3))*w(2) ))
    !if (fb%Lbuff) then
      pb%pb(r:r+3) = px
    !else
    !  write(fb%FID,REC=r) px
    !endif
  end do
endif
end subroutine pb_line2c !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_rec( x1, y1, x2, y2, px )
!      Draw a rectangle 
subroutine pb_rec(pb, x1,y1,x2,y2,px) !{{{
implicit none
class(PixBuffType) :: pb
integer(kind=4) :: x1,x2,y1,y2, lx, hx, ly,hy
character(4) :: px
lx = min(x1,x2)
hx = max(x1,x2)
ly = min(y1,y2)
hy = max(y1,y2)
call pb%line(lx,ly,hx,ly,px)
call pb%line(lx,hy,hx,hy,px)
call pb%line(lx,ly+1,lx,hy-1,px)
call pb%line(hx,ly+1,hx,hy-1,px)
end subroutine pb_rec !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_fillrec( x1, y1, x2, y2, px )
!      Draw a filled rectangle 
subroutine pb_fillrec(pb, x1,y1,x2,y2,px) !{{{
implicit none
class(PixBuffType) :: pb
integer(kind=4) :: x1,x2,y1,y2, i, lx, hx
character(4) :: px
lx = min(x1,x2)
hx = max(x1,x2)
do i = min(y1,y2), max(y1,y2)
  call pb%line(lx,i,hx,i,px)
enddo
end subroutine pb_fillrec !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_putpic( pb, x, y, pb2, alpha )
!      Draw a PixBuffType, pb2 (BGRA) to the location (x,y) of this PixBuff, pb
!      If alpha=.True. then use Alpha channel, if False then ignore.
subroutine pb_putpic( pb, x, y, pb2, alpha ) !{{{
implicit none
class(PixBuffType) :: pb
integer(kind=4) :: x,y, k, l, m
logical, optional :: alpha
type(PixBuffType) :: pb2
logical :: al

al = .false.
if (present(alpha)) al = alpha

if (al) then !do alpha channel mixing
else !just put the pic to the screen
 do l = 1, pb2%h !loop over pixbuff lines
  !if (fb%Lbuff) then
   k = pb%getrec(x,y+(l-1))
   m = pb2%getrec(1,l)
   ! copy full lines at a time (faster than pixel by pixel)
   pb%pb(k:k+pb2%w*4-1) = pb2%pb( m : m+pb2%w*4-1 )
  !else
  ! write(fb%FID,REC=fb%getrec(x,y)) pb2%pb( (l-1)*pb2%w+1 : l*pb2%w )
  !endif
 enddo
endif
end subroutine pb_putpic !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_getpic( pb, x, y, pb2 )
!      Extract a PixBuffType, pb2 (BGRA) from the location (x,y) of this PixBuff, pb
subroutine pb_getpic( pb, x, y, w, h, pb2 )  !{{{
implicit none
class(PixBuffType) :: pb
integer(kind=4) :: x,y, w, h, k, l, m
type(PixBuffType) :: pb2

call pb2%init( w, h, w, .false. )
 do l = 1, pb2%h !loop over pixbuff lines
  !if (fb%Lbuff) then
   k = pb%getrec(x,y+(l-1))
   m = pb2%getrec(1,l)
   ! copy full lines at a time (faster than pixel by pixel)
   pb2%pb( m : m+pb2%w*4-1 ) = pb%pb(k:k+pb2%w*4-1)
  !else
  ! write(fb%FID,REC=fb%getrec(x,y)) pb2%pb( (l-1)*pb2%w+1 : l*pb2%w )
  !endif
 enddo
end subroutine pb_getpic !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_blurRec( x1, y1, x2, y2, r )
!      Blur existin pixels in rectangle 
subroutine pb_blurRec(pb, x1,y1,x2,y2,r)  !{{{
implicit none
class(PixBuffType) :: pb
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
  call pb%getPixel(i,j,px)
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
  call pb%putPixel(i,j,px)
 enddo
enddo
end subroutine pb_blurRec !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_HSVScaleRec( x1, y1,  x2, y2, md(3) )
!      Modify the Hugh, Saturation and/or brightness within this rectangle
!      md(3) scales each component of the HSV value.
subroutine pb_HSVScaleRec( pb, x1,y1, x2,y2, md ) !{{{
implicit none
class(PixBuffType) :: pb
integer :: x1, y1, x2, y2, i, j, ic(4), h(3)
real :: md(3)
character(4) :: px

do i = x1, x2
 do j = y1, y2
  call pb%getPixel( i, j, px )
  ic(1) = iachar(px(1:1))
  ic(2) = iachar(px(2:2))
  ic(3) = iachar(px(3:3))
  ic(4) = iachar(px(4:4))
  call RGB2HSV( ic(1),ic(2),ic(3), h(1),h(2),h(3) )   
  h(:) = nint(real(h(:))*md(:))
  call HSV2RGB( h(1),h(2),h(3), ic(1),ic(2),ic(3) )
  px = char(ic(1))//char(ic(2))//char(ic(3))//char(ic(4))
  call pb%putPixel( i, j, px )
 enddo
enddo

end subroutine pb_HSVScaleRec !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_linepolar( x, y, r, theta, px )
!      Draw a line from point with radius and angle to the terminal directly
!   Angle in degrees
subroutine pb_linepolar(pb, x,y,r,theta,px) !{{{
implicit none
class(PixBuffType) :: pb
integer(kind=4) :: x,y,theta,r
character(4) :: px
real(kind=4) :: deg
 deg=theta/57.2957795130823
 !write(6,*) nint(r*cos(deg)), x,y, r, deg
 !    write(6,*) nint(r*sin(deg))
  call pb%line(x,y,x+nint(r*cos(deg)),y-nint(r*sin(deg)), px)
end subroutine pb_linepolar !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_linepolar2( x, y, r1, r2, theta, px )
!      Draw a line from point with radius and angle to the terminal directly
!   Angle in degrees
subroutine pb_linepolar2(pb, x,y,r1,r2,theta,px) !{{{
implicit none
class(PixBuffType) :: pb
integer(kind=4) :: x,y,theta,r1,r2
character(4) :: px
real(kind=4) :: deg
 deg=theta/57.2957795130823
 !write(6,*) nint(r*cos(deg)), x,y, r, deg
 !    write(6,*) nint(r*sin(deg))
  call pb%line(x+nint(r1*cos(deg)),y-nint(r1*sin(deg)), &
          &    x+nint(r2*cos(deg)),y-nint(r2*sin(deg)), px)
end subroutine pb_linepolar2 !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_circle( x, y, r, px )
!   draw a circle centered at location (x,y) with radius of, r. and color, px
subroutine pb_circle( pb, x, y, r, px ) !{{{
implicit none
class(PixBuffType) :: pb
integer, intent(in) :: x, y, r
character(4), intent(in) :: px
integer :: i, i1, j
i1 = nint(real(r)/sqrt(2.0))
! draw upper and lower curves first
! draw left and right curves second
do i = -i1, i1
  j = nint(sqrt(real(r*r-i*i)))
  call pb%putPixel( i+x, j+y, px)
  call pb%putPixel( i+x,-j+y, px)
  call pb%putPixel( j+x, i+y, px)
  call pb%putPixel(-j+x,-i+y, px)
enddo

end subroutine pb_circle !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_semicircle( x, y, r, px )
!   draw a semicircle centered at location (x,y) with radius of, r. 
!   from angle t1 to t2, and color, px
subroutine pb_semicircle( pb, x, y, r, t1, t2, px ) !{{{
implicit none
class(PixBuffType) :: pb
integer, intent(in) :: x, y, r
character(4), intent(in) :: px
integer :: i, i1, j, t1, t2
real(kind=4) :: d1, d2, a
d1 = tan(t1/57.2957795130823) ! radian
d2 = tan(t2/57.2957795130823)
i1 = nint(real(r)/sqrt(2.0))
! draw upper and lower curves first
! draw left and right curves second
do i = -i1, i1
  j = nint(sqrt(real(r*r-i*i)))
  a = (real(j)/real(i))
  if (a.ge.d1 .and. a.le.d2)   call pb%putPixel( i+x, j+y, px)
  a = (real(-j)/real(i))
  if (a.ge.d1 .and. a.le.d2)   call pb%putPixel( i+x,-j+y, px)
  a = (real(i)/real(j))
  if (a.ge.d1 .and. a.le.d2)   call pb%putPixel( j+x, i+y, px)
  a = (real(-i)/real(-j))
  if (a.ge.d1 .and. a.le.d2)   call pb%putPixel(-j+x,-i+y, px)
enddo

end subroutine pb_semicircle !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_fillcircle( x, y, r, px )
!   draw a circle centered at location (x,y) with radius of, r. and color, px
subroutine pb_fillcircle( pb, x, y, r, px ) !{{{
implicit none
class(PixBuffType) :: pb
integer, intent(in) :: x, y, r
character(4), intent(in) :: px
integer :: i, j
real :: ra
! augment radius by a fraction of a pixel to prevent single pixel top and bottom
ra = real(r)+0.4
ra = ra*ra

call pb%line(-r+x, y,  r+x, y, px) !center horizontal
! draw upper and lower curves 
do i = 1, r
  j = nint(sqrt(ra-real(i*i)))
  call pb%line(-j+x,-i+y,  j+x,-i+y, px) !upper
  call pb%line(-j+x, i+y,  j+x, i+y, px) !lower
enddo

end subroutine pb_fillcircle !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_sphere( x, y, r, px )
!   draw a circle centered at location (x,y) with radius of, r. and color, px
subroutine pb_sphere( pb, x, y, r, px ) !{{{
implicit none
class(PixBuffType) :: pb
integer, intent(in) :: x, y, r
character(4), intent(in) :: px
character(4) :: lp ! local pixel
integer :: i, j, j1, ic(3)
real :: ra, d
! augment radius by a fraction of a pixel to prevent single pixel top and bottom
ra = real(r)+0.4
ra = ra*ra

call pb%line(-r+x, y,  r+x, y, px) !center horizontal
! draw upper and lower curves 
do i = -r, r ! vertical raster
  j1 = nint(sqrt(ra-real(i*i)))
  do j = -j1, j1 !horizonal raster
    d = sqrt( 1.0-real(j*j+i*i)/real(r*r) )
    ic(1) = int(real(iachar(px(1:1)))*d)
    ic(2) = int(real(iachar(px(2:2)))*d)
    ic(3) = int(real(iachar(px(3:3)))*d)
    lp(:) = char(ic(1))//char(ic(2))//char(ic(3))//px(4:4)
    call pb%putPixel( j+x, i+y, lp )
  enddo
enddo

end subroutine pb_sphere !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
!  PLOTS & FONTS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_plot( XY, pr, sr, px )
!   plot data XY within the plot range pr into a pixel domain sr
subroutine pb_plot( pb, XY, pr, sr, px ) !{{{
implicit none
class(PixBuffType) :: pb
real(4), dimension(:,:), intent(in) :: XY, pr
integer, dimension(2,2), intent(in) :: sr
character(4), intent(in) :: px
integer :: i, x, y, k
real(4) :: dx, dy

! if there's too much data to fit, it must be binned
dx = (pr(1,2)-pr(1,1))/float(sr(1,2)-sr(1,1)-1)
dy = (pr(2,2)-pr(2,1))/float(sr(2,2)-sr(2,1)-1)

do i=1, size(XY,2) 
   x = int( (XY(1,i)-pr(1,1))/dx ) + sr(1,1) + 1
   y = sr(2,2) -int((XY(2,i)-pr(2,1))/dy) + 1
   ! check if out or on terminal domain bounds
   if ( (x.ge.sr(1,2).or.x.le.sr(1,1)).or. &
      & (y.ge.sr(2,2).or.y.le.sr(2,1)) ) cycle
   !if (fb%Lbuff) then ! writing to buffer
     k = pb%getrec(x,y)
     pb%pb(k:k+3) = px
   !else    !writing directly to frame buffer device.
   !  write(fb%FID,REC=getrec(x,y)) px
   !endif
   !call tput( "@", x, y ) !this line was used for "fcurses"
enddo

end subroutine pb_plot !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_mplot( XY, pr, sr, px )
!   plot multiple data XY within the plot range pr into a pixel domain sr
subroutine pb_mplot( pb, XY, pr, sr, px ) !{{{
implicit none
class(PixBuffType) :: pb
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
     if ( (x.ge.sr(1,2).or.x.le.sr(1,1)).or. &
        & (y.ge.sr(2,2).or.y.le.sr(2,1)) ) cycle
     call pb%line( ox,oy, x,y, px(j-1) )
     !write(0,*) x, y, sr(2,2), XY(j,i)/dy
     !write(fb%FID,REC=getrec(x,y)) px(j)
   endif
   !call tput( "@", x, y )
  enddo
enddo

end subroutine pb_mplot !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_heatplot( XYZ, pr, sr, cr )
!   plot data XYZ within the plot range pr into a pixel domain sr
subroutine pb_heatplot( pb, XYZ, pr, sr, cr ) !{{{
implicit none
class(PixBuffType) :: pb
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
   !if (fb%Lbuff) then ! writing to buffer
     k = pb%getrec(x,y)
     pb%pb(k:k+3) = px
   !else    !writing directly to frame buffer device.
   !  write(fb%FID,REC=getrec(x,y)) px
   !endif
enddo

end subroutine pb_heatplot !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_matrixplot( XYZ, ar, sr, cr )
!   plot data XYZ within the plot range pr into a pixel domain sr
subroutine pb_matrixplot( pb, XYZ, ar, sr, cr ) !{{{
implicit none
class(PixBuffType) :: pb
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
      k = int((max(min(XYZ(i,j),ar(2)),ar(1))-ar(1))/dz)
      px = char(k)//char(k)//char(k)//char(0)
   case("rgb ") 
      k = int((max(min(XYZ(i,j),ar(2)),ar(1))-ar(1))*2.0/dz)
      if (k.le.255) then
        px = char(255-k)//char(k)//char(0)//char(0)
      else 
        k = k-255
        px = char(0)//char(255-k)//char(k)//char(0)
      endif
   case default
      k = int((max(min(XYZ(i,j),ar(2)),ar(1))-ar(1))/dz)
      px = char(k)//char(k)//char(k)//char(0)
   end select
   ! check if out or on terminal domain bounds
   if ( (x.ge.sr(1,2).or.x.le.sr(1,1)).or. &
      & (y.ge.sr(2,2).or.y.le.sr(2,1)) ) cycle
   !if (fb%Lbuff) then ! writing to buffer
     k = pb%getrec(x,y)
     pb%pb(k:k+3) = px
   !else    !writing directly to frame buffer device.
   !  write(fb%FID,REC=getrec(x,y)) px
   !endif
 enddo
enddo

end subroutine pb_matrixplot !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_matrixplot4( XYZU, ar, sr, cr )
!   plot data XYZU within the plot range pr into a pixel domain sr
! two outputs for a given (x,y) coordinate, Z and U
subroutine pb_matrixplot4( pb, XYZ, ar, sr, hr ) !{{{
implicit none
class(PixBuffType) :: pb
real(4), dimension(:,:,:), intent(in) :: XYZ
integer, dimension(2,2), intent(in) :: sr
real(4), dimension(2,2), intent(in) :: ar
real(4), dimension(2), intent(in) :: hr !hue range [0:1)
!character(4), intent(in) :: cr !color range type
character(4) :: px
integer :: i, j, x, y, k, h, s, v, r, g, b
real(4) :: dx, dy, dz, du

! if there's too much data to fit, it must be binned
dx = float(size(XYZ,1))/float(sr(1,2)-sr(1,1)-1)
dy = float(size(XYZ,2))/float(sr(2,2)-sr(2,1)-1)
dz = (ar(1,2)-ar(1,1))/(255.0*(hr(2)-hr(1)))
du = (ar(2,2)-ar(2,1))/255.0

do i=1, size(XYZ,1) 
 do j=1, size(XYZ,2)
   x = int(real(i)/dx) + sr(1,1) + 1
   y = -int(real(j)/dy) + sr(2,2) - 1
   ! check if out or on terminal domain bounds
   if ( (x.ge.sr(1,2).or.x.le.sr(1,1)).or. &
      & (y.ge.sr(2,2).or.y.le.sr(2,1)) ) cycle
   h = int((max(min(XYZ(i,j,1),ar(1,2)),ar(1,1))-ar(1,1))/dz +hr(1)*255.0)
   s =  255
   v = int((max(min(XYZ(i,j,2),ar(2,2)),ar(2,1))-ar(2,1))/du)
   call HSV2RGB(h,s,v, r,g,b)
   px = char(b)//char(g)//char(r)//char(0)
   !if (fb%Lbuff) then ! writing to buffer
     k = pb%getrec(x,y)
     pb%pb(k:k+3) = px
   !else    !writing directly to frame buffer device.
   !  write(fb%FID,REC=getrec(x,y)) px
   !endif
 enddo
enddo

end subroutine pb_matrixplot4 !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_printNumber(n, x, y, s, fpx, bpx)
!   print a monochrome digit, n, at location, x, y, with size, s, and color, px.
subroutine pb_printNumber( pb, n, x, y, s, px, bpx) !{{{
implicit none
class(PixBuffType) :: pb
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
      if (N1(i,j)) then; call pb%putPixel(x-1+i,y-1+j,px)
      elseif (BG) then;  call pb%putPixel(x-1+i,y-1+j,bpx); endif
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
      if (N2(i,j)) then; call pb%putPixel(x-1+i,y-1+j,px)
      elseif (BG) then;  call pb%putPixel(x-1+i,y-1+j,bpx); endif
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
      if (N3(i,j)) then; call pb%putPixel(x-1+i,y-1+j,px)
      elseif (BG) then;  call pb%putPixel(x-1+i,y-1+j,bpx); endif
    enddo
  enddo
else
  write(0,*) "fb_printNumber: ERROR: unsupported font size, ",s
  STOP
endif

end subroutine pb_printNumber !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! pb_printString(str, x, y, s, px, bpx)
!   print a monochrome alphastring, str, at location, x, y, with size, s, and color, px.
subroutine pb_printString( pb, str, x, y, s, px, bpx) !{{{
implicit none
class(PixBuffType) :: pb
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
case(char(16)) ! theta (narrow) !{{{
L1(:,1) = (/ F, F, T, T, F /)
L1(:,2) = (/ F, T, F, F, T /)
L1(:,3) = (/ F, T, T, T, T /)
L1(:,4) = (/ F, T, F, F, T /)
L1(:,5) = (/ F, F, T, T, F /) !}}}
case(char(17)) ! phi  (stupidly triangular) !{{{
L1(:,1) = (/ F, F, T, F, F /)
L1(:,2) = (/ F, T, T, T, F /)
L1(:,3) = (/ T, F, T, F, T /)
L1(:,4) = (/ F, T, T, T, F /)
L1(:,5) = (/ F, F, T, F, F /) !}}}
case(char(18)) ! pi (lower case) !{{{
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ T, T, T, T, T /)
L1(:,3) = (/ F, T, F, T, F /)
L1(:,4) = (/ F, T, F, T, F /)
L1(:,5) = (/ F, T, F, T, T /) !}}}
case(' ') !{{{
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, F, F, F, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, F, F, F, F /)
L1(:,5) = (/ F, F, F, F, F /) !}}}
case('`') !{{{
L1(:,1) = (/ F, T, F, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, F, T, F /)
L1(:,4) = (/ F, F, F, F, F /)
L1(:,5) = (/ F, F, F, F, F /) !}}}
case('~') !{{{
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, T, T, F, T /)
L1(:,3) = (/ T, F, F, T, F /)
L1(:,4) = (/ F, F, F, F, F /)
L1(:,5) = (/ F, F, F, F, F /) !}}}
case('!') !{{{
L1(:,1) = (/ F, F, T, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, F, F, F /)
L1(:,5) = (/ F, F, T, F, F /) !}}}
case('@') !{{{
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, T, F, T /)
L1(:,3) = (/ T, F, T, T, T /)
L1(:,4) = (/ T, F, F, F, F /)
L1(:,5) = (/ F, T, T, T, T /) !}}}
case('#') !{{{
L1(:,1) = (/ F, T, F, T, F /)
L1(:,2) = (/ T, T, T, T, T /)
L1(:,3) = (/ F, T, F, T, F /)
L1(:,4) = (/ T, T, T, T, T /)
L1(:,5) = (/ F, T, F, T, F /) !}}}
case('$') !{{{
L1(:,1) = (/ F, T, T, T, T /)
L1(:,2) = (/ T, F, T, F, F /)
L1(:,3) = (/ F, T, T, T, F /)
L1(:,4) = (/ F, F, T, F, T /)
L1(:,5) = (/ T, T, T, T, F /) !}}}
case('%') !{{{
L1(:,1) = (/ T, T, F, F, T /)
L1(:,2) = (/ T, T, F, T, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, T, F, T, T /)
L1(:,5) = (/ T, F, F, T, T /) !}}}
case('^') !{{{
L1(:,1) = (/ F, F, T, F, F /)
L1(:,2) = (/ F, T, F, T, F /)
L1(:,3) = (/ T, F, F, F, T /)
L1(:,4) = (/ F, F, F, F, F /)
L1(:,5) = (/ F, F, F, F, F /) !}}}
case('&') ! this looks stupid !{{{
L1(:,1) = (/ F, T, T, F, F /)
L1(:,2) = (/ F, T, F, T, F /)
L1(:,3) = (/ F, T, T, T, F /)
L1(:,4) = (/ T, F, F, T, F /)
L1(:,5) = (/ F, T, T, F, T /) !}}}
case('*') !{{{
L1(:,1) = (/ F, F, T, F, F /)
L1(:,2) = (/ T, F, T, F, T /)
L1(:,3) = (/ F, T, T, T, F /)
L1(:,4) = (/ T, F, T, F, T /)
L1(:,5) = (/ F, F, T, F, F /) !}}}
case('(') !{{{
L1(:,1) = (/ F, F, F, T, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, F, T, F /) !}}}
case(')') !{{{
L1(:,1) = (/ F, T, F, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, T, F, F, F /) !}}}
case('[') !{{{
L1(:,1) = (/ F, F, T, T, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, T, T, F /) !}}}
case(']') !{{{
L1(:,1) = (/ F, T, T, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, T, T, F, F /) !}}}
case('{') !{{{
L1(:,1) = (/ F, F, T, T, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, T, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, T, T, F /) !}}}
case('}') !{{{
L1(:,1) = (/ F, T, T, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, T, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, T, T, F, F /) !}}}
case('<') !{{{
L1(:,1) = (/ F, F, F, T, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, T, F, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, F, T, F /) !}}}
case('>') !{{{
L1(:,1) = (/ F, T, F, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, F, T, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, T, F, F, F /) !}}}
case('.') !{{{
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, F, F, F, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, F, F, F /) !}}}
case(':') !{{{
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, F, F, F /) !}}}
case(',') !{{{
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, F, F, F, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, T, F, F, F /) !}}}
case(';') !{{{
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, T, F, F, F /) !}}}
case('+') !{{{
L1(:,1) = (/ F, F, T, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ T, T, T, T, T /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, T, F, F /) !}}}
case('=') !{{{
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, T, T, T, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, T, T, T, F /)
L1(:,5) = (/ F, F, F, F, F /) !}}}
case('_') !{{{
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, F, F, F, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, F, F, F, F /)
L1(:,5) = (/ T, T, T, T, T /) !}}}
case('-') !{{{
L1(:,1) = (/ F, F, F, F, F /)
L1(:,2) = (/ F, F, F, F, F /)
L1(:,3) = (/ F, T, T, T, F /)
L1(:,4) = (/ F, F, F, F, F /)
L1(:,5) = (/ F, F, F, F, F /) !}}}
case('?') !{{{
L1(:,1) = (/ F, F, T, F, F /)
L1(:,2) = (/ F, T, F, T, F /)
L1(:,3) = (/ F, F, F, T, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, T, F, F /) !}}}
case('/') !{{{
L1(:,1) = (/ F, F, F, F, T /)
L1(:,2) = (/ F, F, F, T, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, T, F, F, F /)
L1(:,5) = (/ T, F, F, F, F /) !}}}
case('\') !{{{
L1(:,1) = (/ T, F, F, F, F /)
L1(:,2) = (/ F, T, F, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, F, T, F /)
L1(:,5) = (/ F, F, F, F, T /) !}}}
case('|') !{{{
L1(:,1) = (/ F, F, T, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, T, F, F /) !}}}
case('"') !{{{
L1(:,1) = (/ F, T, F, T, F /)
L1(:,2) = (/ F, T, F, T, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, F, F, F, F /)
L1(:,5) = (/ F, F, F, F, F /) !}}}
case("'") !{{{
L1(:,1) = (/ F, F, T, F, F /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, F, F, F /)
L1(:,4) = (/ F, F, F, F, F /)
L1(:,5) = (/ F, F, F, F, F /) !}}}
case('1') !{{{
L1(:,1) = (/ F, F, T, F, F /)
L1(:,2) = (/ F, T, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, T, T, T, F /) !}}}
case('2') !{{{
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ F, F, T, T, F /)
L1(:,4) = (/ F, T, F, F, F /)
L1(:,5) = (/ T, T, T, T, T /) !}}}
case('3') !{{{
L1(:,1) = (/ T, T, T, T, F /)
L1(:,2) = (/ F, F, F, F, T /)
L1(:,3) = (/ F, T, T, T, F /)
L1(:,4) = (/ F, F, F, F, T /)
L1(:,5) = (/ T, T, T, T, F /) !}}}
case('4') !{{{
L1(:,1) = (/ T, F, F, T, F /)
L1(:,2) = (/ T, F, F, T, F /)
L1(:,3) = (/ T, T, T, T, T /)
L1(:,4) = (/ F, F, F, T, F /)
L1(:,5) = (/ F, F, F, T, F /) !}}}
case('5') !{{{
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ F, F, F, F, T /)
L1(:,5) = (/ T, T, T, T, F /) !}}}
case('6') !{{{
L1(:,1) = (/ F, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ F, T, T, T, F /) !}}}
case('7') !{{{
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ F, F, F, T, F /)
L1(:,3) = (/ F, F, F, T, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, T, F, F /) !}}}
case('8') !{{{
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ F, T, T, T, F /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ F, T, T, T, F /) !}}}
case('9') !{{{
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ F, T, T, T, T /)
L1(:,4) = (/ F, F, F, F, T /)
L1(:,5) = (/ F, F, T, T, F /) !}}}
case('A','a') !{{{
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, T, T, T, T /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ T, F, F, F, T /) !}}}
case('B','b') !{{{
L1(:,1) = (/ T, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ T, T, T, T, F /) !}}}
case('C','c') !{{{
L1(:,1) = (/ F, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, F, F, F, F /)
L1(:,4) = (/ T, F, F, F, F /)
L1(:,5) = (/ F, T, T, T, T /) !}}}
case('D','d') !{{{
L1(:,1) = (/ T, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, F, F, F, T /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ T, T, T, T, F /) !}}}
case('E','e') !{{{
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ T, F, F, F, F /)
L1(:,5) = (/ T, T, T, T, T /) !}}}
case('F','f') !{{{
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ T, F, F, F, F /)
L1(:,5) = (/ T, F, F, F, F /) !}}}
case('G','g') !{{{
L1(:,1) = (/ F, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, F, F, T, T /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ F, T, T, T, T /) !}}}
case('H','h') !{{{
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, T, T, T, T /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ T, F, F, F, T /) !}}}
case('I','i') !{{{
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ T, T, T, T, T /) !}}}
case('J','j') !{{{
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ T, T, F, F, F /) !}}}
case('K','k') !{{{
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, F, F, T, F /)
L1(:,3) = (/ T, T, T, F, F /)
L1(:,4) = (/ T, F, F, T, F /)
L1(:,5) = (/ T, F, F, F, T /) !}}}
case('L','l') !{{{
L1(:,1) = (/ T, F, F, F, F /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ T, F, F, F, F /)
L1(:,4) = (/ T, F, F, F, F /)
L1(:,5) = (/ T, T, T, T, T /) !}}}
case('M','m') !{{{
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, T, F, T, T /)
L1(:,3) = (/ T, F, T, F, T /)
L1(:,4) = (/ T, F, T, F, T /)
L1(:,5) = (/ T, F, F, F, T /) !}}}
case('N','n') !{{{
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, T, F, F, T /)
L1(:,3) = (/ T, F, T, F, T /)
L1(:,4) = (/ T, F, F, T, T /)
L1(:,5) = (/ T, F, F, F, T /) !}}}
case('O','o','0') !{{{
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, F, F, F, T /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ F, T, T, T, F /) !}}}
case('P','p') !{{{
L1(:,1) = (/ T, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ T, F, F, F, F /)
L1(:,5) = (/ T, F, F, F, F /) !}}}
case('Q','q') !{{{
L1(:,1) = (/ F, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, F, T, F, T /)
L1(:,4) = (/ T, F, F, T, T /)
L1(:,5) = (/ F, T, T, T, T /) !}}}
case('R','r') !{{{
L1(:,1) = (/ T, T, T, T, F /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, T, T, T, F /)
L1(:,4) = (/ T, F, F, T, F /)
L1(:,5) = (/ T, F, F, F, T /) !}}}
case('S','s') !{{{
L1(:,1) = (/ F, T, T, T, T /)
L1(:,2) = (/ T, F, F, F, F /)
L1(:,3) = (/ F, T, T, T, F /)
L1(:,4) = (/ F, F, F, F, T /)
L1(:,5) = (/ T, T, T, T, F /) !}}}
case('T','t') !{{{
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ F, F, T, F, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, T, F, F /) !}}}
case('U','u') !{{{
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ T, F, F, F, T /)
L1(:,4) = (/ T, F, F, F, T /)
L1(:,5) = (/ F, T, T, T, F /) !}}}
case('V','v') !{{{
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, F, F, F, T /)
L1(:,3) = (/ F, T, F, T, F /)
L1(:,4) = (/ F, T, F, T, F /)
L1(:,5) = (/ F, F, T, F, F /) !}}}
case('W','w') !{{{
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ T, F, T, F, T /)
L1(:,3) = (/ T, F, T, F, T /)
L1(:,4) = (/ T, T, F, T, T /)
L1(:,5) = (/ T, F, F, F, T /) !}}}
case('X','x') !{{{
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ F, T, F, T, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, T, F, T, F /)
L1(:,5) = (/ T, F, F, F, T /) !}}}
case('Y','y') !{{{
L1(:,1) = (/ T, F, F, F, T /)
L1(:,2) = (/ F, T, F, T, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, F, T, F, F /)
L1(:,5) = (/ F, F, T, F, F /) !}}}
case('Z','z') !{{{
L1(:,1) = (/ T, T, T, T, T /)
L1(:,2) = (/ F, F, F, T, F /)
L1(:,3) = (/ F, F, T, F, F /)
L1(:,4) = (/ F, T, F, F, F /)
L1(:,5) = (/ T, T, T, T, T /) !}}}
end select !}}}

  do j = 1, 5
    do i = 1, 5
      if (L1(i,j)) then; call pb%putPixel(x-7+i+6*l,y-1+j,px)
      elseif (BG) then;  call pb%putPixel(x-7+i+6*l,y-1+j,bpx); endif
    enddo
  enddo
 enddo
else
  write(0,*) "fb printLetter: ERROR: unsupported font size, ",s
  STOP
endif
end subroutine pb_printString !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80

! Noise routines !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
! routines converted from: https://thebookofshaders.com/13/
!  and the wiki page on Perlin noise.
! I didn't make these, just converted to fortran
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! this isn't that random, but kinda looks it.
! this must be deterministic.
function random( v ) !{{{
implicit none
real(4), dimension(2), intent(in) :: v
real(4) :: random, b
real(4), dimension(2) :: a

!!!!!!!!!! function 1 !!!!!!!!!!!!!
a = (/ 12.9898, 78.233 /)
b = sin(dot_product(v,a))*43758.5453123

!!!!!!!!!! function 2 !!!!!!!!!!!!!
! hash() from https://www.shadertoy.com/view/4dS3Wd
!a = (/ 17.0, 13.0 /)
!b = 1.e4*sin(a(1)*v(1) +v(2)*0.1) *(0.1-abs( sin(v(2)*a(2) +v(1)) ))

!!!!!!!!!! function 3 !!!!!!!!!!!!!
!a = (/ 1.0, 0.5 /)
!b = 1234.5*dot_product(v,a)

random = b -floor(b)

end function random !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! perlin Noise, range from 0-1
function noise( v, q ) !{{{
implicit none
!integer :: seed
real(4), dimension(2), intent(in) :: v
real(4) :: q, noise, a, b, c, d, pi, unt, x1, x2
real(4), dimension(2) :: i, f, u

pi = 3.141596535

unt = 1.0/q  ! unit 
i = floor(v/unt) ! integer of v
f = mod(v,unt)/unt     ! remainder [0:1)
u = 0.5*(1.0-cos(pi*f))  ! Perlin noise?
!u = 1.0-cos(2.0*pi*f)

!a = 1.0; b = 1.0; c = 1.0; d = 1.0
! four corners in 2D of a tile
a = random(i)
b = random(i +(/ 1.0, 0.0 /) )
c = random(i +(/ 0.0, 1.0 /) )
d = random(i +(/ 1.0, 1.0 /) )

!u = f*f*(3.0 -2.0*f)
!u = f
x1 = a*(1.0-u(1)) +b*u(1) ! mix(a,b,u(1))
x2 = c*(1.0-u(1)) +d*u(1) ! mix(c,d,u(1))
noise = x1*(1.0-u(2)) +x2*u(2) ! mix(x1,x2,u(2))

!noise = a*(1.0-u(1)) +b*u(1) +(c-a)*u(2)*(1.0-u(1)) &
!    &   +(d-b)*u(1)*u(2) 

!a = 1.0-(a-0.5)*0.2 !/(seed+3.0)
!b = u(1)-(b-0.5)*0.2 !/(i(1)+12.0)
!c = u(2)-(c-0.5)*0.2  !/(i(2)+12.0)
!noise = a*sin(b*pi*2)*sin(c*pi*2)

end function noise !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fractal brownian motion
function fbm( v, lac, gain, oct, ifreq, pow ) !{{{
implicit none
real(4), dimension(2), intent(in) :: v
real(4), intent(in) :: lac, gain, ifreq
integer, intent(in) :: oct, pow !number of octaves
real(4), dimension(2) :: st
real(4) :: fbm, val, amp, normk, freq
integer :: i

st = v
amp = 1.0

! changing initial frequency does weird things
freq = ifreq !0.5 !4.0

normk=0.
val = 0.0
! loop over octaves
do i = 1, oct
  val = val +amp*noise( st, freq )
  st = st*lac !lacunarity
  freq = freq*lac !lacunarity
  amp = amp*gain !gain
  normk = normk+amp
enddo
fbm = val/normk

! changing this power stratifies highs from lows
fbm = fbm**pow

end function fbm !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 


end module fbMod
