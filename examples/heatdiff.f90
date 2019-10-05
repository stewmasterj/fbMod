! example of a transient thermal diffusion in 2d, displayed using linux framebuffer
! author: Ross J. Stewart
! date: April 28, 2019
! compile: gfortran fbMod.f90 heatdiff.f90 -o heatdiff
program heatplot
use fbMod
implicit none
real(4), dimension(:,:), allocatable :: dat, dd
integer :: Nx, Ny, i, j, t, Nsteps
real(4) :: dx, dy, x, y, z, pi, dt, Ti, Tj, ddT, ar(2), kappa
integer, dimension(2,2) :: sr
character(8) :: chr

! open and set framebuffer
call fb_init(10,"/dev/fb0","buffer", 1440, 900, 1472, .false.)
call fb_read !grab what's on the screen into the buffer

pi = acos(-1.0)

Nsteps = 10000
dt = 0.006 !time step
kappa = 0.01
Nx = 400
Ny = 400
dx = 2.0*pi/real(Nx) !each grid size distance
dy = 2.0*pi/real(Ny)
allocate( dat(Nx,Ny), dd(Nx,Ny) )
dat = 0.0
dd = 0.0

do i = 1, Nx
 do j = 1, Ny
  x = real(i-Nx/2)*dx
  y = real(j-Ny/2)*dy
  dat(i,j) = sin(x)*sin(y)
 enddo
enddo

write(6,*) minval(dat)
write(6,*) maxval(dat)

! X and Y plot location, screen range
sr(1,:) = (/ 200, 600 /)
sr(2,:) = (/ 200, 600 /)
ar(:) = (/ -1.0, 1.0 /)
! black and white plot range
call fb_matrixplot( dat, ar, sr, "bw  " )
call fb_write

do t = 1, Nsteps
 ! integrate the heat equation through time
 ! calculate thermal gradients from the eight neighbours
 ! bulk domain
 do i = 2, Nx-1
  do j = 2, Ny-1
   dd(i,j) = kappa*( (dat(i+1,j)-2.0*dat(i,j)+dat(i-1,j))/(dx*dx) + &
                     (dat(i,j+1)-2.0*dat(i,j)+dat(i,j-1))/(dy*dy) )
  enddo
 enddo
 ! left and right edges
 do j = 2, Ny-1
   dd(1,j)  = kappa*( (dat(2,j)   -2.0*dat(1,j) +dat(Nx,j)) /(dx*dx) + &
                      (dat(1,j+1) -2.0*dat(1,j) +dat(1,j-1)) /(dy*dy) )
   dd(Nx,j) = kappa*( (dat(1,j)   -2.0*dat(Nx,j)+dat(Nx-1,j))/(dx*dx) + &
                      (dat(Nx,j+1)-2.0*dat(Nx,j)+dat(Nx,j-1))/(dy*dy) )
 enddo
 ! top and bottom edges
 do i = 2, Nx-1
  dd(i,1)  = kappa*( (dat(i+1,1) -2.0*dat(i,1) +dat(i-1,1)) /(dx*dx) + &
                     (dat(i,2)   -2.0*dat(i,1) +dat(i,Ny))  /(dy*dy) )
  dd(i,Ny) = kappa*( (dat(i+1,Ny)-2.0*dat(i,Ny)+dat(i-1,Ny))/(dx*dx) + &
                     (dat(i,1)   -2.0*dat(i,Ny)+dat(i,Ny-1))/(dy*dy) )
 enddo
 dd(1,1)   = kappa*( (dat(2,1) -2.0*dat(1,1)  +dat(Nx,1))   /(dx*dx) + &
                     (dat(1,2) -2.0*dat(1,1)  +dat(1,Ny))   /(dy*dy) )
 dd(1,Ny)  = kappa*( (dat(2,Ny)-2.0*dat(1,Ny) +dat(Nx,Ny))  /(dx*dx) + &
                     (dat(1,1) -2.0*dat(1,Ny) +dat(1,Ny-1)) /(dy*dy) )
 dd(Nx,1)  = kappa*( (dat(1,1) -2.0*dat(Nx,1) +dat(Nx-1,1)) /(dx*dx) + &
                     (dat(Nx,2)-2.0*dat(Nx,1) +dat(Nx,Ny))  /(dy*dy) )
 dd(Nx,Ny) = kappa*( (dat(1,Ny)-2.0*dat(Nx,Ny)+dat(Nx-1,Ny))/(dx*dx) + &
                     (dat(Nx,1)-2.0*dat(Nx,Ny)+dat(Nx,Ny-1))/(dy*dy) )
 ! Apply the change in temperature
 dat = dat +dd*dt

 ! black and white plot range
 if (mod(t,500).eq.0) then
  call fb_read !grab what's on the screen into the buffer
  call fb_matrixplot( dat, ar, sr, "bw  " )
  write(chr,'(i8)') t 
  call fb_printString( chr, 300, 180, 1, char(255)//char(255)//char(255)//char(0), char(0)//char(0)//char(0)//char(0) )
  call fb_write
 endif
enddo

call fb_close

end program heatplot
