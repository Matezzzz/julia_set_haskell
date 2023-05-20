# Julia Set Viewer in Haskell and SDL2

This app was made by me, Matěj Mrázek, as a task for the practicals in the subject Non-procedural programming.

## Running

I use the following two packages in my program:
* `sdl2 2.5.5.0` for managing the window, handling input, and displaying the results. I chose this libary mainly due to my familiarity with it from other languages
* `colour 2.3.6` for converting HSL colors to RGB

I chose `cabal` for both managing packages and compiling:
 * I downloaded both packages using `cabal install --lib ...`
 * After that was done, I was able to compile and run the app using `cabal run`

## Controls

The app supports the following controls (**(+Shift)** denotes that holding the left shift achieves the inverse effect):
 * **WASD** to move the viewport
 * **QE** to zoom in and out
 * **U(+Shift)** to increase the real part of the julia set C constant
 * **I(+Shift)** to increase the imaginary part of the julia set C constant
 * **O(+Shift)** to increase julia iterations. Going below 0 makes the iterations infinite - for some values of C (those close to the origin), computation may run indefinitely, be careful.
 * **P(+Shift)** to increase the color period (change colors less often)
 * **Escape** to exit the app

Resizing the window is supported, as well as pressing the X button.





### Whoops!

When exiting the app in any way, I get a segmentation fault on my machine. I tried getting rid of it by commenting out all the lines after the main game loop exits, but the error still occured. For this reason, I believe it is a problem inside of the SDL2 library. I decided not to deal with it further, because it doesn't affect app usage at all - it happens after I quit, after all.