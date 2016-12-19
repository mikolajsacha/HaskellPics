## Haskell Pics. An image processing tool written in Haskell.

A simple command line utility performing some basic operations on images.  
Libraries used: JuicyPixels combined with Repa.  
I made this program as a practice in Haskell and image processing. There might be some performance flaws and bugs.


#### REQUIREMENTS:

GHC compiler
Packages: JuicyPixels, Repa, Criterion


#### COMPILING:

Compile using 'make' or manually by one of the following commands:
ghc hspics.hs
ghc -O -threaded -rtsopts --make hspics.hs
The latter utilizes optimized and multithreaded array operations by Repa


#### USAGE:

Run the tool in command line like this:  
hspics.exe [command] [parameters]  
e.g: hspics.exe grayscale some_picture.jpg  

For multithreading run with the following flags (recommended!):  
hsimg.exe [command] [parameters] +RTS -N2 -H  
where '-N2' means running on 2 cores  

Different command line arguments correspond to different types of image processing.  
Possible input formats: same as in JuicyPixels lib.  
Output format: PNG.  
The output of processing is always placed in "output.png" file.  
ATTENTION: if you run program when there is already an "output.png" file, it will be overridden!  


#### POSSIBLE COMMANDS:

##### Basic color transformations:

grayscale [imagePath]  
only_red [imagePath]  
only_blue [imagePath]  
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
filter_skin [imagePath]  <-- filter the color of human skin (using HSL color space)  
filter_red_eyes [imagePath]  <-- filter red eyes on photography  

##### Noise filters:  
average_rgb_filter [imagePath]  <-- filter noises using moving average on RGB coordinates  
median_rgb_filter [imagePath]  <-- filter noises using moving median on RGB coordinates  
average_y_filter [imagePath]  <-- filter noises using moving average only on Y from YCbCr  
median_y_filter [imagePath]  <-- filter noises using moving median only on Y from YCbCr  

##### Binarization:  
binarize [imagePath] [threshold] <-- binarize image using threshold (from 0 to 256)  
binarize_otsu [imagePath] <-- binarize image using Otsu's method for calculating treshold  
binarize_bernsen [imagePath] <-- binarize image using Bernsen's method for calculating treshold  
binarize_mixed [imagePath] [threshold] <-- binarize image using mixed Otsu's and Bernsen's method with a given threshold

##### Morphology:  
Meanings of arguments in the following commands:  
shape = type of structural element. Can be: "Square", "Circle", "Cross" (write without quotation marks)  
windowSize = non-negative integer. 0 means just 1 element, 1 = 9 elements (3x3), 2 = (5x5) etc.  
erosion [imagePath] [shape] [windowSize] <-- apply erosion (on otsu binarized image)  
dilation [imagePath] [shape] [windowSize] <-- apply dilation (on otsu binarized image)  
rgb_erosion [imagePath] [shape] [windowSize] <-- apply erosion (on original RGB image)  
rgb_dilation [imagePath] [shape] [windowSize] <-- apply dilation (on original RGB image)  
opening [imagePath] [shape] [windowSize] <-- Dilation after erosion (otsu binarized)  
closing [imagePath] [shape] [windowSize] <-- Erosion after dilation (otsu binarized)  
