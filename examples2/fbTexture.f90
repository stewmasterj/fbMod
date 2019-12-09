! vim:fdm=marker
! Author: Ross J. Stewart
! DATE: October 2019
! compile: gfortran -fbounds-check ../fbMod2.f90 fbTexture.f90 -o fbTexture
! run: ./fbTexture
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
program fbtest
use fbMod
implicit none
integer :: fbwidth, fbheight, fbline
character(4) :: red,green, blue,cyan, yellow,magenta, black,white, dkg,ddkg
character(1024) :: pxline
integer :: i, j, k, r
real(4), dimension(2,400) :: XY
real(4), dimension(2,2) :: pr
integer, dimension(2,2) :: sr
type(ProcTexType) :: tx

! open and set framebuffer
!call fb_init(10,"/dev/fb0","direct", 1440, 900, 1472, .false.)
call fb%fbinit(10,"/dev/fb0", 1440, 900, 1472, .true.)

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

! test triangle fill
call fb%filltriangle( 700,600, 750,550, 780,630, black )
call fb%filltriangle3c( 700,800, 750,750, 780,830, red, green, blue )
call fb%filltriangle3c( 800,800, 850,750, 880,830, cyan, magenta, yellow )
call fb%filltriangle3c( 600,800, 650,750, 680,830, black, white, black )

sr(1,:) = (/ 1, fb%w /)
sr(2,:) = (/ 1, fb%h /)
! test overlaps
call fb%filltriangle3c( 100,600, 100,800, 200,700, white, white, blue, sr, 1.0, 1.0, 0.0 )
call fb%filltriangle3c( 100,600, 100,800, 300,700, white, white, red, sr, 1.0, 1.0, 1.0 )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
! try some textures
tx%typ = 3; tx%ip(:) = 3
call fb%filltriangle3c( 100,100, 100,200, 160,200, red, green, blue, sr, 1.,1.,1., tx )
call fb%filltriangle3c( 110,100, 170,100, 170,200, red, green, blue, sr, 1.,1.,1., tx )

tx%typ = 2; tx%ip(:) = 3
call fb%filltriangle3c( 200,100, 200,200, 260,200, red, green, blue, sr, 1.,1.,1., tx )
call fb%filltriangle3c( 210,100, 270,100, 270,200, red, green, blue, sr, 1.,1.,1., tx )

tx%typ = 1; tx%ip(:) = 3
call fb%filltriangle3c( 300,100, 300,200, 360,200, red, green, blue, sr, 1.,1.,1., tx )
call fb%filltriangle3c( 310,100, 370,100, 370,200, red, green, blue, sr, 1.,1.,1., tx )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
! pine tree-like
tx%ip(1:3) = (/ 5, 3, 1 /)
tx%typ = 1
dkg = char(20)//char(160)//char(20)//char(0)
ddkg = char(20)//char(80)//char(20)//char(0)
call fb%filltriangle3c( 500,100, 500,190, 450,200, dkg, black, black, sr, 1.,1.,1., tx )
call fb%filltriangle3c( 500,100, 500,190, 550,200, dkg, black, black, sr, 1.,1.,1., tx )
! pine tree-like
tx%typ = 2
call fb%filltriangle3c( 600,100, 600,190, 550,200, dkg, ddkg, black,  sr, 1.,1.,1., tx )
call fb%filltriangle3c( 600,100, 600,190, 650,200, dkg, ddkg, black, sr, 1.,1.,1., tx )
! pine tree-like
tx%typ = 3
call fb%filltriangle3c( 700,100, 700,190, 650,200, dkg, ddkg, black,  sr, 1.,1.,1., tx )
call fb%filltriangle3c( 700,100, 700,190, 750,200, dkg, ddkg, black, sr, 1.,1.,1., tx )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
! try some other textures
tx%typ = 4; tx%rp(1:2) = (/ 0.1, 0.20 /)
call fb%filltriangle3c( 100,220, 100,320, 160,320, dkg, green, ddkg, sr, 1.,1.,1., tx )
call fb%filltriangle3c( 110,220, 170,220, 170,320, dkg, green, ddkg, sr, 1.,1.,1., tx )

! inner circles
tx%typ = 5; tx%rp(:) = 0.5
call fb%filltriangle3c( 200,220, 200,280, 260,280, red, green, blue, sr, 1.,1.,1., tx )
call fb%filltriangle3c( 210,220, 270,220, 270,280, red, green, blue, sr, 1.,1.,1., tx )

tx%typ = 6; tx%rp(1:2) = (/ 0.5, 100.0 /) !frequency and amplitude
call fb%filltriangle3c( 300,220, 300,320, 360,320, red, green, blue, sr, 1.,1.,1. )
call fb%filltriangle3c( 310,220, 370,220, 370,320, red, green, blue, sr, 1.,1.,1., tx )

tx%typ = 7; 
tx%rp(1) = 2.0 !lacunarity
tx%rp(2) = 0.5 !Gain
tx%ip(1) = 5 !Octaves
tx%rp(3) = 0.5 !init frequency
tx%rp(4) = 150.0 !fbm amplitude
tx%ip(2) = 2 !fbm power
call fb%filltriangle3c( 400,220, 400,320, 460,320, red, green, blue, sr, 1.,1.,1., tx )
call fb%filltriangle3c( 410,220, 470,220, 470,320, red, green, blue, sr, 1.,1.,1., tx )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
call fb%display

call fb%save("fbtexture.ppm",2)

call fb%close
end program fbtest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
