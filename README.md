# fbMod - Fortran module for drawing to a linux framebuffer device

## features

Can set individual pixels of your screen, draw lines with gradients, draw rectangles
and triangles. set vertex's colors and interpolate to shade.
Loop them to animate. Plot data with pixel precision. 
The possibilities are endless with Fortran.

## Examples

-`fbhello.f90`
	does not use `fbMod.f90` but uses the basic elements to make a simple demonstration.

-`fbtest.f90`
	demonstrates most of the routines

-`fbbuffertest.f90`
	tests the use of the User Buffer, this is the fastest way to repeatedly draw frames to the screen.

-`colors.f90`
	draws a colorwheel out of six triangles.. not not really a circle.

-`fbplot.f90`
	not just a demo program: 
	fbplot -- Plot data from columns of file to the framebuffer device /dev/fb0 
	Usage: 
	 fbplot  xcol ycol y2col y3col ... yNcol FILE 
	xcol  is the column of FILE to use as x coordinates 
	ycol  is the column of FILE to use as the first dependent variable 
	 subsequent integer arguments are interpreted as additional dependent variables 
	 to plot onto the same plot.
	FILE  is the last argument and is the file path

### Routines for screen drawing.

-`fb_init(i,dev, mode, wd,ht,ln, zbu)`
	set variables and open framebuffer device 
	i   file descriptor 
	dev  device path of framebuffer, usually /dev/fb0 
	mode can be `direct` to device or written to a `buffer` to be dumped later 
	wd   pixel width of framebuffer device 
	ht   pixel height of framebuffer device 
	ln   pixel line length of framebuffer device 
	zbu  use a Z-Buffer?

-`fb_write`
	dumps the user buffer to the fb device.

-`fb_read`
	reads the current device buffer into the user buffer.

-`fb_dump(FL,mode)`
	writes the user buffer to file descriptor `FL` 
	`mode` 1: raw dump  2: PPM  3: X,Y, R,G,B list

-`fb_close`
	closes the framebuffer device file

-`getrec(x,y)`
	returns the 4Byte record number for an XY screen position or the byte offset in "buffer" mode.

-`fb_pixel(x,y,px)`
	set pixel `px` at location (x,y).

-`fb_clear(b)`
	fills the buffer with byte value `b`. default is NULL.

### Shapes

-`PointSlope(i,x1,y1,x2,y2)`
	point slope form of line. returns y given an x that falls on a line from point 1 to 2.

-`fb_filltriangle( x1, y1, x2, y2, x3, y3, px )`
	Draw a filled triangle with verticies p1, p2, p3 with color, px

-`fb_filltriangle3c( x1,y1, x2,y2, x3,y3, px1,px2,px3, sb, z1,z2,z3 )`
	Draw a filled trianlge with verticies p1, p2, p3. 
	each vertex has color px1, px2, px3, fill colour interpolates within bounds, sb. 
	Z values optional and operate if `zbu` was set to true, for correct layering.

-`fb_line(x1,y1,x2,y2,px)`
	Draw a line from point to point to the terminal directly, using the Bresenham algorithm

-`fb_line2c(x1,y1,x2,y2,px1,px2,sb,z1,z2)`
	Draw a line from point to point to the terminal directly with colors px1, px2. 
	using the Bresenham algorithm and 'sb' to check bounds.

-`fb_rec(x1,y1,x2,y2,px)`
	Draw a rectangle from top left point to bottom right point to the terminal directly

-`fb_linepolar(x,y,r,theta,px)`
	Draw a line from point with radius and angle to the terminal directly. Angle in degrees

-`fb_circle( x, y, r, px )`
	draw a circle centered at location (x,y) with radius of, r. and color, px

### PLOTS & FONTS

-`fb_plot( XY, pr, sr, px )`
	plot data XY within the plot range pr into a pixel domain sr

-`fb_mplot( XY, pr, sr, px )`
	plot multiple data XY within the plot range pr into a pixel domain sr

-`fb_printNumber(n, x, y, s, px, bpx)`
	print a monochrome digit, n, at location, x, y, with size, s, and color, px.

-`fb_printString(str, x, y, s, px, bpx)`
	print a monochrome alphastring, str, at location, x, y, with size, s, and color, px.


