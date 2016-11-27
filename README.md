## Haskell Pics. An image processing tool written in Haskell.

A simple command line utility performing some basic operations on images.  
Libraries used: JuicyPixels combined with Repa.  
I made this program as a practice in Haskell and image processing. There might be some performance flaws and bugs.

#### USAGE

Compile with:  
ghc hspics.hs

Run the tool in command line like this:  
hsimg.exe [command] [parameters]  
e.g: hsimg.exe grayscale some_picture.jpg

Different command line arguments correspond to different types of image processing.  
Processing can be done on a single image or on two images.  
Possible input formats: same as in JuicyPixels lib.  
Output format: PNG.  
The output of processing is always placed in "output.png" file.  
ATTENTION: if you run program when there is already an "output.png" file, it will be overridden!  


#### POSSIBLE COMMANDS:

##### Basic color transformations:

grayscale [imagePath]  
only_red [imagePath]  
blue [imagePath]  
only_green [imagePath]  
negative [imagePath]  
sepia [imagePah] 

##### YCbCr color coordinates: 
only_y [imagePath]  
only_cb [imagePath]  
only_cr [imagePath]  

##### HSL color coordinates:  
only_h [imagePath]  <-- uses only h, sets s=0.5 and l=0.5  
only_s [imagePath]  <-- uses h and s, sets l=0.5  
only_l [imagePath]  <-- uses only l, sets h=0, s=0  
filter_hue [imagePath] [minValue] [maxValue]  <-- shows only pixels with hue from given range (hue has values from [0, 360])  
filter_skin [imagePath]  <-- tries to filter the color of human skin    