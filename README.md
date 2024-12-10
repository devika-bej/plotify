# plotify
plotify is a tool to visualize mathematical graphs just like in Desmos Graphing Calculator, using a simpler and more intuitive language. The repository contains the compiler code for the DSL to Desmos Graph conversion.

## Usage
After cloning the repository  
`cd plotify`  
`racket server.rkt`  
Open calc.html on your web browser and graph :)

## About the DSL
The basic syntax of the language in plotify is  
`shape-op arg-list`  
`feat-op-1 val` ...  
`feat-op-n val`  
where `shape-op` is one of the shape operators and `feat-op` is one of the feature operators.  
### Shape operators
- `point x y` - A point at (x,y)
- `line x1 y1 x2 y2` - A line extending from (x1,y1) to (x2,y2)
- `poly x1 y1 x2 y2 ... xn yn` - A polygon connecting points from (x1,y1) to (xn,yn)
- `circle c_x c_y r` - A circle centered at (c_x,c_y) with radius r
- `ellipse c_x c_y r_x r_y [m]` - An ellipse centered at (c_x,c_y) with radius r_x about x-axis and r_y about y-axis. Optional argument m can tilt the ellipse with m slope
### Feture operators
Feature operators modify the nearest preceding shape operator
- `color #hex` - Colors the relevant shape with the specified hex color
- `opacity val` - Changes the transparency of shapes with fill (circle, ellipse, poly)
- `thick val` - Changes the thickness of the line or border of the shape  
