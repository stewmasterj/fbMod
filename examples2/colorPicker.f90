! vim:fdm=marker
! Author: Ross J. Stewart
! DATE: August 2018
! compile: gcc -c ../../fcurses/kbhit.c
! compile: gfortran -c ../fbMod2.f90 ../../fcurses/fcurses.f90 colorPicker.f90
! compile: gfortran -lc kbhit.o fbMod2.o fcurses.o colorPicker.o -o colorPicker
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
program colorPicker
use fbMod
use fcurses
implicit none
!integer :: fbwidth, fbheight, fbline
character(4) :: px, bpx, red, green, blue, cyan, yellow, magenta, black, white, &
  & grey, purple, orange
integer :: i, x, y, w, h, rh, s, v, r, g, b, hsv(3), key, rgb(3)
character, dimension(7) :: ch
character(40) :: word
type(PixBuffType) ::  oldscreen

x = 600
y = 300
w = 110
h = 280 

hsv(:) = (/ 128, 255, 128 /)
key = 0

call init_screen("/tmp") ! for fcurses (must be before fbinit)
! open and set framebuffer
! this can be found with fbset
call fb%fbinit(10,"/dev/fb0",1440,900,1472,.false.)
call fb%loadScreen
call fb%getPixBuff( x, y, w+1, h+1, oldscreen )

red = char(0)//char(0)//char(255)//char(0)
green = char(0)//char(255)//char(0)//char(0)
blue = char(255)//char(0)//char(0)//char(0)
cyan = char(255)//char(255)//char(0)//char(0)
yellow = char(0)//char(255)//char(255)//char(0)
magenta = char(255)//char(0)//char(255)//char(0)
black = char(0)//char(0)//char(0)//char(0)
grey = char(127)//char(127)//char(127)//char(0)
white = char(255)//char(255)//char(255)//char(0)
purple = char(255)//char(0)//char(127)//char(0)
orange = char(0)//char(128)//char(255)//char(0)

! draw window boarder
call fb%fillRec( x+1,y+1, x+w-1,y+h-1, black )
call fb%Rec( x,y, x+w,y+h, white )

! test HSV
call fb%putString("H", x+12, y+10, 1, white, black)
call fb%putString("S", x+32, y+10, 1, white, black)
call fb%putString("V", x+52, y+10, 1, white, black)

do i = 0, 255
 ! Hue
 h = 255-i; s=255; v=255
 call HSV2RGB( h,s,v,  r,g,b )
 px = char(b)//char(g)//char(r)//char(0)
 call fb%line( x+10,y+i+20, x+20,y+i+20, px)
enddo

call fb%putString("K", x+75, y+142, 1, white, black )
call fb%putString("up", x+73, y+162, 1, white, black )
call fb%putString("dn", x+73, y+182, 1, white, black )
call fb%putString("J", x+75, y+202, 1, white, black )
call fb%putString("tab", x+70, y+222, 1, white, black )
call fb%fillTriangle( x+90,y+145, x+94,y+140, x+98,y+145, white )
call fb%fillTriangle( x+90,y+150, x+94,y+145, x+98,y+150, white )
call fb%fillTriangle( x+90,y+167, x+94,y+162, x+98,y+167, white )

call fb%fillTriangle( x+90,y+182, x+94,y+187, x+98,y+182, white )
call fb%fillTriangle( x+90,y+200, x+94,y+205, x+98,y+200, white )
call fb%fillTriangle( x+90,y+205, x+94,y+210, x+98,y+205, white )

call fb%fillTriangle( x+90,y+225, x+94,y+220, x+94,y+230, white )
call fb%fillTriangle( x+102,y+225, x+98,y+220, x+98,y+230, white )

! begin interaction !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do
 ! selected H S V (sh,ss,sv)
 if (key.eq.0) then
 call fb%fillRec( x+21,y+255-hsv(1)+4, x+25,y+255-hsv(1)+36, black )
 call fb%fillTriangle( x+25,y+255-hsv(1)+15, x+21,y+255-hsv(1)+20, x+25,y+255-hsv(1)+25, white)
 elseif (key.eq.1) then
 call fb%fillRec( x+41,y+255-hsv(2)+4, x+45,y+255-hsv(2)+36, black )
 call fb%fillTriangle( x+45,y+255-hsv(2)+15, x+41,y+255-hsv(2)+20, x+45,y+255-hsv(2)+25, white)
 else
 call fb%fillRec( x+61,y+255-hsv(3)+4, x+65,y+255-hsv(3)+36, black )
 call fb%fillTriangle( x+65,y+255-hsv(3)+15, x+61,y+255-hsv(3)+20, x+65,y+255-hsv(3)+25, white)
 endif
 
 call HSV2RGB( hsv(1),hsv(2),hsv(3),  r,g,b )
 rgb(:) = (/ r, g, b /)
  px = char(b)//char(g)//char(r)//char(0)
 call fb%fillRec( x+80,y+20, x+100,y+40, px )
 ! write HSV values
 call fb%putString( "HSV:", x+80, y+45, 1, white, black )
 write(word,'(i3)') hsv(1)
 call fb%putString( trim(word), x+85, y+55, 1, white, black )
 write(word,'(i3)') hsv(2)
 call fb%putString( trim(word), x+85, y+65, 1, white, black )
 write(word,'(i3)') hsv(3)
 call fb%putString( trim(word), x+85, y+75, 1, white, black )
 ! write RGB values
 write(word,'(3(i3,x))') rgb(1:3)
 call fb%putString( "RGB:", x+80, y+90, 1, white, black )
 write(word,'(i3)') rgb(1)
 call fb%putString( trim(word), x+85, y+100, 1, white, black )
 write(word,'(i3)') rgb(2)
 call fb%putString( trim(word), x+85, y+110, 1, white, black )
 write(word,'(i3)') rgb(3)
 call fb%putString( trim(word), x+85, y+120, 1, white, black )
 
 do i = 0, 255
  ! Hue
  !h = 255-i; s=255; v=255
  h = 255-i; s=hsv(2); v=hsv(3)
  call HSV2RGB( h,s,v,  r,g,b )
  px = char(b)//char(g)//char(r)//char(0)
  call fb%line( x+10,y+i+20, x+20,y+i+20, px)
  ! saturation
  !h = hsv(1); s=255-i; v=255
  h = hsv(1); s=255-i; v=hsv(3)
  call HSV2RGB( h,s,v,  r,g,b )
  px = char(b)//char(g)//char(r)//char(0)
  call fb%line( x+30,y+i+20, x+40,y+i+20, px)
  ! value (brightness level)
  !h = hsv(1); s=255; v=255-i
  h = hsv(1); s=hsv(2); v=255-i
  call HSV2RGB( h,s,v,  r,g,b )
  px = char(b)//char(g)//char(r)//char(0)
  call fb%line( x+50,y+i+20, x+60,y+i+20, px)
 enddo
 
 call fb%display

 call getfullkey(ch)
 select case(ch(1))
 case (char(9)) !TAB
  key = mod(key+1,3)
 case (char(27))
  if (ch(2).eq.'[') then
   if     (ch(3).eq.'D') then ! LEFT
    hsv(key+1) = max(0, hsv(key+1) - 1)
   elseif (ch(3).eq.'C') then ! RIGHT
    hsv(key+1) = min(255, hsv(key+1) + 1)
   elseif (ch(3).eq.'A') then !  UP
    hsv(key+1) = min(255, hsv(key+1) + 1)
   elseif (ch(3).eq.'B') then ! DONW
    hsv(key+1) = max(0, hsv(key+1) - 1)
   endif
  endif
 case ('j') ! DOWN
    hsv(key+1) = max(0, hsv(key+1) - 1)
 case ('J') ! DOWN 10
    hsv(key+1) = max(0, hsv(key+1) - 10)
 case ('k') ! UP
    hsv(key+1) = min(255, hsv(key+1) + 1)
 case ('K') ! UP 10
    hsv(key+1) = min(255, hsv(key+1) + 10)
 case (char(13)) !ENTER
  exit
 end select

enddo
! End interaction !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call fb%save("colorPicker.ppm",2)

call fb%putPixBuff( x, y, oldscreen, .false. )
call fb%display !display old screen image


call fb%close
call kill_screen("/tmp")

write(6,*) "HSV: ", hsv
write(6,*) "RGB: ", rgb

end program colorPicker
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
