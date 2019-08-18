! DATE: August 5, 2019
! compile: gfortran -fbounds-check ../fbMod2.f90 subimage.f90 -o subimage
! run: ./subimage
program subimage
use fbMod
implicit none
type(PixBuffType) :: si, si2 !subimage
character(4) :: px
integer :: i, j

call fb%fbinit(10,"/dev/fb0", 1440, 900, 1472, .false. )

! make 10x10 subimage
call si%init( 10, 10, 10, .false. )

px = char(200)//char(0)//char(0)//char(0) !blue
call si%fillRec( 1,1, 10,10, px )

px = char(0)//char(0)//char(200)//char(0) !red
call si%fillCircle( 6,6, 2, px )
!call si%save("si.ppm",2)
!call si%save("si.dat",3)

! put the subimage to the frame buffer at (10,10)
call fb%putPixBuff( 10,10, si )

!!!!!!!!!!!!!!
! now let's see if we can copy what we just put.
call fb%getPixBuff( 10,10, 10,10, si2 ) !x,y, w,h
!call si2%save("si2.ppm",2)
!call si2%save("si2.dat",3)
! and put it in a different place.
do i = 20, 100, 20
do j = 20, 100, 20
call fb%putPixBuff( i, j, si2 )
enddo
enddo

!!!!!!!!!!!!!!
! let's save what we drew
call fb%getPixBuff( 1,1, 120,120, si ) !overwrite 'si'
call si%save("subimage.ppm",2)

!!!!!!!!!!!!!!
call fb%display
call fb%close

end program subimage
