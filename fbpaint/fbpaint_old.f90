! vim:fdm=marker
! DATE: August 2019
! compile: gcc -c ../../fcurses/kbhit.c
! compile: gfortran -fbounds-check -c fbpaint.f90 ../../fcurses/fcurses.f90 ../../ArgsMod/ArgsMod.f90 ../fbMod2.f90
! compile: gfortran -fbounds-check -lc -o fbpaint fcurses.o kbhit.o fbMod2.o ArgsMod.o fbpaint.o
module optMod
use fbMod
character(80) :: inFile, outFile
integer :: newW, newH
character(4) :: pxUI1, pxUI2, bpxUI1, bpxUI2
character(4), dimension(20) :: pal
type(PixBuffType) :: ib !image buffer
integer :: ln, cl  ! local line and column in image (y,x)
integer :: bc !brush color
integer :: iboff(2) !image buffer position offset (start location on framebuffer)

integer, parameter :: Nicons = 2 !number of lefthand icons that indicate operations
type(PixBuffType) :: icon(2)

type windowType
 integer :: x, y, w, h
 integer :: fbw, fbh, fblline
end type windowType
type(windowType) :: win
end module optMod
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program paint
use fcurses
use optMod
implicit none
integer(kind=4) :: i, ierr, x, y
character, dimension(7) :: ch, ch2
character(80) :: line
integer, dimension(2,10) :: XY

! get commandline options and define defaults
call options

! some input file given.
if (trim(inFile).ne.'NULL') then
  ! input file specified
  call ib%loadPPM( trim(inFILE) )
else !make a new image buffer with defined size
  if (newW.ne.0.and.newH.ne.0) then
    call ib%init( newW, newH, newW, .false. )
  endif
endif

! Framebuffer /dev/fb0: what is shown on screen
! fbMod buffer fb%pb is what is written to before going to screen
!  must call fb%display to put fb%pb to /dev/fb0
! the image to edit is in a different buffer: ib

! primary loop: select an operation via key press, e.g. 1, or 2.
! operation loop: collect a certain number of image positions

call init_screen( "/tmp" ) ! setup the tty console parameters
call cls  !clear the terminal screen
call tput( "^[ ^d EXIT   ^s SAVE   ^f outFile", 1, 1 )
call fb%fbinit(10,"/dev/fb0",win%fbw,win%fbh,win%fblline,.false.) !init the frame buffer
call fb%loadScreen !read the current display into buffer.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!40
! draw the "background" or UI of the program !{{{
! draw the "icons" 20x20 pixel squares are subimages or pixbuffers
!   they should be highlighted when selected
x = win%x+iboff(1)-1
y = win%y+iboff(2)-1
! draw top left corner frame
call fb%line( x,y, x+nint(0.382*real(ib%w)),y, pxUI2)
call fb%line( x,y, x,y+nint(0.382*real(ib%h)), pxUI2)
! top and left line
call fb%line( x+nint(0.382*real(ib%w)),y, x+1+ib%w,y, bpxUI2)
call fb%line( x,y+nint(0.382*real(ib%h)), x,y+1+ib%h, bpxUI2)
x = x +2 +ib%w
y = y +2 +ib%h
! draw bottom right corner frame
call fb%line( x-nint(0.382*real(ib%w)),y, x,y, pxUI2)
call fb%line( x,y-nint(0.382*real(ib%h)), x,y, pxUI2)
! bottom and right line
call fb%line( x-1-ib%w,y, x-nint(0.382*real(ib%w)),y, bpxUI2)
call fb%line( x,y-1-ib%h, x,y-nint(0.382*real(ib%h)), bpxUI2)

! draw the icons
! primary design for all icons
call icon(1)%init( 20,20,20, .false. )
call icon(1)%fillRec( 2,2, 19,19, bpxUI2 )
call icon(1)%line( 1,1, 1,20, bpxUI1 ) ! full left
call icon(1)%line( 2,1, 20,1, bpxUI1 ) ! full top
call icon(1)%line( 20,2, 20,20, bpxUI2 ) ! full right
call icon(1)%line( 2,20, 19,20, bpxUI2 ) ! full bottom
! copy the primary design to all other icons
do i = 2, Nicons
  call icon(i)%init( 20,20,20, .false. )
  icon(i) = icon(1)
enddo
! label them differently
call icon(1)%putString( "PEN", 2,7, 1, pxUI2, bpxUI2 )
call icon(2)%putString( "LIN", 2,7, 1, pxUI2, bpxUI2 )
! draw the icons to the framebuffer
do i = 1, Nicons
  call fb%putPixBuff( win%x+1, win%y+iboff(2)+21*(i-1), icon(i) )
enddo

! draw  color pallete
x = win%x + 1
y = win%y + iboff(2) + ib%h + 10
do i = 1, 10
 call fb%fillRec( x+12*i,y, x+12*i+10,y+10, pal(i+10) )
 call fb%fillRec( x+12*i,y+12, x+12*i+10,y+22, pal(i) )
 call fb%putString( char(48+mod(i,10)), x+12*i+2,y+14, 1, bpxUI2, pal(i) )
enddo
!}}}

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!40
! draw the loaded image in the screen subset (crop if larger than subscreen)
!  image begins around (25,25)
call fb%putPixBuff( win%x+iboff(1),win%y+iboff(2), ib )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! initial display
call fb%display

ln=0;cl=0
bc = 1 !initial brush color

! primary "command mode" loop
do      !main do loop for capturing keys
 call getfullkey(ch)
 x = iboff(1)+win%x-1
 y = iboff(2)+win%y-1
 !!! Interpretation of ctrl chars and such
 select case(ch(1))
 case (char(27),char(4)) !ESCAPE, ^[, ^d
  call tput( "                                                   ", 1, 1 )
  call tput( "QUIT (Y/n)", 1, 1 )
  call getfullkey(ch2)
  if (ch2(1).eq.'Y') then; exit ! exit loop quits program
  else
   call tput( "^[ ^d EXIT   ^s SAVE   ^f outFile", 1, 1 )
  endif
 case (char(13)) !ENTER
 case (char(127)) !BACKSPACE
 case (char(0))  !^@ NULL 
 case (char(1))  !^a SELECT ALL
 case (char(3))  !^c COPY Selection
 case (char(6))  !^f Set Output File Name
  call tput( "                                                   ", 1, 1 )
  call tput( "Specify Output File Name:", 1, 1 )
  call fancygetrawline( line, char(13), .true. )
  outFile = line
  call tput( "^[ ^d EXIT   ^s SAVE   ^f outFile", 1, 1 )
 case (char(16)) !^p full screen shot
  call fb%save("fbpaintShot.ppm",2)
 case (char(19)) !^s SAVE to Output File Name
  call ib%save(trim(outFile),2)
  call tput( "Saved to file:"//trim(outFile), 1, 1 )
 case (char(22)) !^v PASTE
 case (char(24)) !^x CUT
 case (char(26)) !^z UNDO?
 ! brush color selection !{{{
 case ('1'); bc = 1
 case ('2'); bc = 2
 case ('3'); bc = 3
 case ('4'); bc = 4
 case ('5'); bc = 5
 case ('6'); bc = 6
 case ('7'); bc = 7
 case ('8'); bc = 8
 case ('9'); bc = 9
 case ('0'); bc = 10
 case ('!'); bc = 11
 case ('@'); bc = 12
 case ('#'); bc = 13
 case ('$'); bc = 14
 case ('%'); bc = 15
 case ('^'); bc = 16
 case ('&'); bc = 17
 case ('*'); bc = 18
 case ('('); bc = 19
 case (')'); bc = 20 !}}}
 ! transformation options
 case ('p','P') ! PEN, pixel modification
  call fb%putString( "PEN ", x, y+ib%h+2, 1, pxUI1, bpxUI2 )
  do ! stay in this mode until operation aborts
   call imagePoints( 1, XY(1:2,1), ierr )
   if (ierr.eq.0) then ! perform operation
    ! draw pixel to both image buffer and framebuffer
    call ib%putPixel( XY(1,1), XY(2,1), pal(bc) )
    call fb%putPixel( XY(1,1)+x, XY(2,1)+y, pal(bc) )
    call fb%display ! show updated image buffer
   else; exit !done with operation.
   endif
  enddo
 case ('l','L') ! PEN, pixel modification
  call fb%putString( "LINE", x, y+ib%h+2, 1, pxUI1, bpxUI2 )
  do ! stay in this mode until operation aborts
   call imagePoints( 2, XY(1:2,1:2), ierr )
   if (ierr.eq.0) then ! perform operation
    ! draw line to both image buffer and framebuffer
    call ib%line( XY(1,1),XY(2,1), XY(1,2),XY(2,2), pal(bc) )
    call fb%line( XY(1,1)+x,XY(2,1)+y, XY(1,2)+x,XY(2,2)+y, pal(bc) )
    call fb%display ! show updated image buffer
   else; exit !done with operation.
   endif
  enddo
 end select
 call fb%putString( "    ", x, y+ib%h+2, 1, pxUI1, bpxUI2 )
 call fb%display
enddo

write(6,*)
call fb%close
call kill_screen( "/tmp" )
end program paint
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine imagePoints( n, XY, err ) !{{{
! returns array XY of n image positions.
! check 'err' for premature termination of points, i.e. user mistake.
use optMod
use fcurses
implicit none
integer, intent(in) :: n
integer, dimension(2,n), intent(out) :: XY
integer, intent(out) :: err
integer :: i, x, y, ox, oy
character, dimension(7) :: ch
character(3) :: Fc
character(11) :: cpos
type(PixBuffType) :: cur

i = 0
XY = 0
! secondary get points in image loop
do      !main do loop for capturing keys
 call getfullkey(ch)
 ox = max(1, cl-5)
 oy = max(1, ln-5)
 call ib%getPixBuff( ox, oy, 11,11, cur ) 
 ox = cl; oy = ln
 !!! Interpretation of ctrl chars and such
 select case(ch(1))
  ! brush color selection !{{{
  case ('1'); bc = 1
  case ('2'); bc = 2
  case ('3'); bc = 3
  case ('4'); bc = 4
  case ('5'); bc = 5
  case ('6'); bc = 6
  case ('7'); bc = 7
  case ('8'); bc = 8
  case ('9'); bc = 9
  case ('0'); bc = 10
  case ('!'); bc = 11
  case ('@'); bc = 12
  case ('#'); bc = 13
  case ('$'); bc = 14
  case ('%'); bc = 15
  case ('^'); bc = 16
  case ('&'); bc = 17
  case ('*'); bc = 18
  case ('('); bc = 19
  case (')'); bc = 20 !}}}
  case (char(27))  !ESCAPE
  if (ch(2).eq.'[') then
    Fc=ch(3)//ch(4)//ch(5)    !curser keys work
    if (Fc.eq.'D  ') then     ! LEFT
      cl = max(1, cl-1)
    elseif (Fc.eq.'C  ') then ! RIGHT
      cl = min(ib%w, cl+1)
    elseif (Fc.eq.'A  ') then ! UP
      ln = max(1, ln-1)
    elseif (Fc.eq.'B  ') then ! DOWN
      ln = min(ib%h, ln+1)
    elseif (Fc.eq.'21~') then   !F10 Quit Exit
     exit
    endif
  elseif (ch(2).eq.char(0)) then !exit
   err = 1 !abort operation
   exit
  endif
 case ('q','Q',char(4)) ! q, Q, ^d
   err = 1 !abort operation
   exit

 case ('h'); cl = max(1, cl-1)     !LEFT
 case ('j'); ln = min(ib%h, ln+1)  !DOWN
 case ('k'); ln = max(1, ln-1)     !UP
 case ('l'); cl = min(ib%w, cl+1)  !RIGHT

 case ('H'); cl = max(1, cl-10)     !LEFT
 case ('J'); ln = min(ib%h, ln+10)  !DOWN
 case ('K'); ln = max(1, ln-10)     !UP
 case ('L'); cl = min(ib%w, cl+10)  !RIGHT

 ! point selections
 case (char(13)) ! ENTER KEY
  i = i + 1
  XY(:,i) = (/ cl, ln /) !save this selected position
 case (' ') ! SPACE
  i = i + 1
  XY(:,i) = (/ cl, ln /) !save this selected position
 case (char(127)) !BACKSPACE
  i = max(0, i - 1 ) ! don't go < 0
 end select

 ! if current position is acceptable and this is selected, then just exit.
 if (i.eq.n) then
  err=0; exit ! captured enough
 endif

 ! Probably need to draw some curser or something here
 ! as well as update stats about the current curser location
 !  like color, position, or something. 
 write(cpos,'(a,i4,a,i4,a)') "(",cl, ",", ln,")"
 x = win%x+iboff(1)+ib%w-70
 y = win%y+iboff(2)+ib%h+3
 call fb%putString( cpos, x, y, 1, pxUI1, bpxUI2 )

 ! erase previous curser image by restoring the ib subimage
 x = max(1, ox-5) + iboff(1) + win%x -1
 y = max(1, oy-5) + iboff(2) + win%y -1
 call fb%putPixBuff( x, y, cur )
 ! current fb position 
 x = max(1, cl) + iboff(1) + win%x -1
 y = max(1, ln) + iboff(2) + win%y -1
 ! draw the current curser (cross hairs (empty center))
 call fb%line( x-5,y, x-2,y, pxUI1 ) !left
 call fb%line( x+2,y, x+5,y, pxUI1 ) !right
 call fb%line( x,y-5, x,y-2, pxUI1 ) !top
 call fb%line( x,y+2, x,y+5, pxUI1 ) !bottom

 call fb%display

enddo

end subroutine imagePoints !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine options !{{{
use ArgsMod
use optMod
implicit none
character(80) :: line, cerr
character(80), dimension(1) :: charv
integer, dimension(3) :: intv

! set default values
inFile = "NULL"
outFile = "NULL"
newW = 0
newH = 0
! default UI colors
pxUI1 = char(220)//char(200)//char(180)//char(0)
pxUI2 = char(180)//char(160)//char(140)//char(0)
bpxUI1 = char(120)//char(100)//char(80)//char(0)
bpxUI2 = char(80)//char(60)//char(40)//char(0)
! image buffer rendering offset from window offset
iboff(:) = (/ 25, 25 /)
! paint application window size
! useful when using inside tmux or something
win%fbw = 1440
win%fbh = 900
win%fblline = 1472
win%x = 1 ! start location
win%y = 1
win%w = win%fbw
win%h = win%fbh
! default pallete colors
pal(:) = char(0)//char(0)//char(0)//char(0) ! default black
! bright colors
pal(1) = char(255)//char(255)//char(255)//char(0) !white
pal(2) = char(0)//char(0)//char(255)//char(0) !red
pal(3) = char(0)//char(255)//char(255)//char(0) !magenta
pal(4) = char(0)//char(255)//char(0)//char(0) !green
pal(5) = char(255)//char(255)//char(0)//char(0) !cyan
pal(6) = char(255)//char(0)//char(0)//char(0) !blue
pal(7) = char(255)//char(0)//char(255)//char(0) !yellow
! dark colors
pal(11) = char(0)//char(0)//char(0)//char(0) !black
pal(12) = char(0)//char(0)//char(127)//char(0) !red
pal(13) = char(0)//char(127)//char(127)//char(0) !magenta
pal(14) = char(0)//char(127)//char(0)//char(0) !green
pal(15) = char(127)//char(127)//char(0)//char(0) !cyan
pal(16) = char(127)//char(0)//char(0)//char(0) !blue
pal(17) = char(127)//char(0)//char(127)//char(0) !yellow


if (iargc().eq.0) call help

call getarg(1,line)
if (trim(line).eq."--help".or.trim(line).eq."-h") then
  call help
  STOP
endif

! check for options to program
call getOpt( "-i", 1, charv, cerr )
if (cerr(1:1).eq.'0') inFile = trim(charv(1))

call getOpt( "-o", 1, charv, cerr )
if (cerr(1:1).eq.'0') outFile = trim(charv(1))

call getOpt( "-d", 2, intv(1:2), cerr )
if (cerr(1:1).eq.'0') then
  newW = intv(1)
  newH = intv(2)
endif
! first color
call getOpt( "-1", 3, intv, cerr )
if (cerr(1:1).eq.'0') then
  pxUI1 = char(intv(3))//char(intv(2))//char(intv(1))//char(0)
endif
! second color
call getOpt( "-2", 3, intv, cerr )
if (cerr(1:1).eq.'0') then
  pxUI2 = char(intv(3))//char(intv(2))//char(intv(1))//char(0)
endif
! first background color
call getOpt( "-3", 3, intv, cerr )
if (cerr(1:1).eq.'0') then
  bpxUI1 = char(intv(3))//char(intv(2))//char(intv(1))//char(0)
endif
! second background color
call getOpt( "-4", 3, intv, cerr )
if (cerr(1:1).eq.'0') then
  bpxUI2 = char(intv(3))//char(intv(2))//char(intv(1))//char(0)
endif

! check some stuff
if (inFile.eq."NULL") then
  if (newW.eq.0) then
    write(6,*) "specify the width and height of the new image (pixels)"
    read(5,*) newW, newH
  endif
endif

end subroutine options !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine help
implicit none
write(0,*) "Frame Buffer Paint"
write(0,*) "Usage:  fbpaint [options]"
write(0,*) "Options:"
write(0,*) "  -i inFile     specify input file to read (PPM format)"
write(0,*) "  -o outFile    specify output file to write to when saving (PPM format)"
write(0,*) "  -d w h        if no input file, specify width and height of new image."
write(0,*) "  -1 r g b      override primary UI color. (decimal RGB values)"
write(0,*) "  -2 r g b      override secondary UI color."
write(0,*) "  -3 r g b      override primary UI background color."
write(0,*) "  -4 r g b      override secondary UI background color."
end subroutine help
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
