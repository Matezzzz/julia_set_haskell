{-# LANGUAGE OverloadedStrings #-}

module Main where


import SDL
import Control.Monad (unless, join)
import Foreign (Storable(pokeElemOff), Ptr, castPtr, Word8)
import Data.Colour.RGBSpace.HSL
import Data.Colour.RGBSpace (RGB(channelGreen), RGB(channelRed), RGB(channelBlue), Colour, RGB(RGB))
import Foreign.C (CInt)


--A number in the complex plane
data Complex a = a :+ a deriving Show

--Add complex numbers
(|+|) :: Num a => Complex a -> Complex a -> Complex a
(a :+ b) |+| (c :+ d) = (a+c) :+ (b+d)

--Multiply complex numbers
(|*|) :: Num a => Complex a -> Complex a -> Complex a
(a :+ b) |*| (c :+ d) = (a*c-b*d) :+ (a*d+b*c)

--Magnitude of a complex number (distance from origin)
magnitude :: Floating a => Complex a -> a
magnitude (x :+ y) = sqrt(x*x+y*y)

--Julia fractal iteration - compute how many iterations it takes before the julia equation diverges
juliaIter :: Complex Float -> Complex Float -> Int -> Float
juliaIter z c iters
    | iters == 0 = 0 --no iterations are left, return
    | otherwise =
        let new_z = (z |*| z) |+| c     --compute new z according to the julia equation
            len = magnitude new_z
        in if len > 2                   --if the magnitude of the new z is greater than 2, we have diverged
                --approximate the iterations it took as a floating point number for smooth color transitions later
                --Assume the length of a vector is increasing exponentially in the last step, find the place where it is equal to 2
            then let llen0 = log $ magnitude z in (log 2 - llen0) / (log len - llen0)
                --len < 2 -> perform the next iteration
            else juliaIter new_z c (iters-1) + 1


--Convert a double color in [0, 1] to uint8 in [0, 255]
toWord8:: RGB Double -> RGB Word8
toWord8 (RGB r g b) =
    let t = fromIntegral . round . (255*)
    in RGB (t r) (t g) (t b)

--Get the iteration of divergence and color loop frequency, return the current color
paletteColor:: Float -> Float -> RGB Word8
paletteColor i freq = toWord8 (hsl (360/realToFrac freq*realToFrac i) 1.0 0.5)

--Return the color of the julia fractal at the given position
juliaColor :: Complex Float -> Complex Float -> Int -> Float -> RGB Word8
juliaColor z c max_iters color_freq
    | round depth == max_iters = RGB 0 0 0  --return black if we didn't diverge
    | otherwise = Main.paletteColor depth color_freq --otherwise return a palette color
    where depth = juliaIter z c max_iters


--Represents a viewport window rectangle
data ViewportState = ViewportState {
    fromX::Float,
    fromY::Float,
    toX::Float,
    toY::Float
} deriving Show


middleX::ViewportState -> Float
middleX (ViewportState fx _ tx _) = (tx+fx)/2

middleY::ViewportState -> Float
middleY (ViewportState _ fy _ ty) = (ty+fy)/2


sizeX :: ViewportState -> Float
sizeX (ViewportState fx _ tx _) = tx-fx

sizeY :: ViewportState -> Float
sizeY (ViewportState _ fy _ ty) = ty-fy


--linear interpolation between a and b based on k
linInterp :: Num a => a -> a -> a -> a
linInterp k a b = a * (1-k) + b * k

--divide two integers, obtaining a float
divFloat :: Int -> Int -> Float
x `divFloat` y = fromIntegral x / fromIntegral y

--transform texture coordinates, integers in [0, width-1], [0, height-1], to coordinates in the viewport
transformCoordinates::ViewportState->(Int, Int)->(Int, Int)->Complex Float
transformCoordinates (ViewportState fx fy tx ty) (x, y) (w, h) = linInterp (x `divFloat` (w-1)) fx tx :+ linInterp (y `divFloat` (h-1)) fy ty


--zoom in/out based on k
zoomViewport::Float -> ViewportState -> ViewportState
zoomViewport k vs@(ViewportState fx fy tx ty) =
    let lp = linInterp k
        lix = lp $ middleX vs
        liy = lp $ middleY vs
    in ViewportState (lix fx) (liy fy) (lix tx) (liy ty)


--move the viewport. Shifts are specified in percentages, relative to the window size
moveViewport:: (Float, Float) -> ViewportState -> ViewportState
moveViewport (dx, dy) vs@(ViewportState fx fy tx ty) =
    let mx = dx * sizeX vs
        my = dy * sizeY vs
    in ViewportState (fx+mx) (fy+my) (tx+mx) (ty+my)



--Set a pixel value at the given coordinates
writePixel::Ptr () -> (Int, Int) -> (Int, Int) -> RGB Word8 -> IO ()
writePixel ptr (x, y) (w, _) color = do
    let pos = w * y + x
        c = 4
    --Write alpha, then pixel values in the B G R order.
    pokeElemOff (castPtr ptr) (c*pos+0) (255::Word8)
    pokeElemOff (castPtr ptr) (c*pos+1) (channelBlue color)
    pokeElemOff (castPtr ptr) (c*pos+2) (channelGreen color)
    pokeElemOff (castPtr ptr) (c*pos+3) (channelRed color)


--Render the julia fractal into the provided pointer
renderJulia::Ptr () -> ViewportState -> (Int, Int) -> (Int, Int) -> Complex Float -> Int -> Float -> IO ()
renderJulia ptr viewport surf_size@(sw, sh) render_size@(rw, rh) c max_iters color_freq = do
    let indices = [(x, y) | x <- [0..rw-1], y <- [0..rh-1]]
    mapM_ (\pos -> do writePixel ptr pos surf_size (juliaColor (transformCoordinates viewport pos render_size) c max_iters color_freq)) indices



--State of the julia rendering - current vieport, c constant, max iterations and color frequency
data JuliaState = JuliaState{
    viewport :: ViewportState,
    juliaC :: Complex Float,
    juliaIters :: Int,
    colorFreq :: Float
} deriving Show

--App state - the SDL window, frame surface, whether the full resolution is rendered, and current julia state
data AppState = AppState{
    window :: Window,
    frameSurface :: Maybe Surface,
    fullResolution :: Bool,
    juliaState :: JuliaState
}

--create a rendering surface of the same size as the provided window
createSurface :: Window -> IO Surface
createSurface w = do
    ws <- get (windowSize w)
    createRGBSurface ws RGBA8888


--If window size has changed or no surface is available, create a new one
updateSurface :: Window -> Maybe Surface -> IO (Surface, Bool)
--no surface is available, create one
updateSurface wnd Nothing = do
    sf <- createSurface wnd
    return (sf, True)
--surface is available
updateSurface wnd (Just surf) = do
    ws <- get (windowSize wnd)
    ss <- surfaceDimensions surf
    --if the surface has different dimensions than the window, free it and create a new one with the correct size
    if ws /= ss
        then do
            freeSurface surf
            sf <- createSurface wnd
            return (sf, True)
        else return (surf, False)



--Change the viewport in julia state to match the window ratio
matchWindowRatio:: JuliaState -> Window -> IO JuliaState
matchWindowRatio js@(JuliaState vs@(ViewportState fx fy tx ty) c its freq) wnd = do
    (V2 wnd_w wnd_h) <- get (windowSize wnd)
    let (view_w, view_h) = (sizeX vs, sizeY vs)
        wnd_r = fromIntegral wnd_w / fromIntegral wnd_h --window ratio
        view_r = view_w / view_h                        --viewport ratio
    if abs (wnd_r - view_r) < 1e-5  --if both ratios are nearly the same, nothing needs to be changed
        then return js
    else do
        --compute new viewport width and height according to the window ratio
        let new_w = view_h * wnd_r
            new_h = view_w / wnd_r
            midx=middleX vs
            midy=middleY vs
        --Update the viewpoer to keep one dimension the same while making the other one larger
        return (if new_w > view_w
            then JuliaState (ViewportState (midx-new_w/2) fy (midx+new_w/2) ty) c its freq
            else JuliaState (ViewportState fx (midy-new_h/2) tx (midy+new_h/2)) c its freq)

--Axis input - processes input using two buttons each defining movement in the opposite direction. E.g. UP/DOWN arrow. Returns (-1, 0, 1) based on the movement, and True/False if any keys are pressed
axisInput :: Num a => (Scancode -> Bool) -> Scancode -> Scancode -> (a, Bool)
axisInput pressed down up =
    let pu = pressed up
        pd = pressed down
    in ((if pu then 1 else 0) + (if pd then (-1) else 0), pu || pd)

--Axis shift input - active if a button is pressed. If left shift is held, it returns -1, otherwise 1. 
axisInputShift :: Num a => (Scancode -> Bool) -> Scancode -> (a, Bool)
axisInputShift pressed key =
    let sh = pressed ScancodeLShift
        pr = pressed key
    in if pr then (if sh then -1 else 1, True) else (0, False)


--process all keyboard inputs, moving the camera, zooming, updating the number of iterations, and more. Return the new state and whether anything has changed
processInput :: (Scancode -> Bool) -> JuliaState -> (JuliaState, Bool)
processInput pressed (JuliaState vp (cx :+ cy) its freq) =
    let ax = axisInput pressed
        axs = axisInputShift pressed
        --camera movement = WASD keys
        (movex, on1) = ax ScancodeA ScancodeD
        (movey, on2) = ax ScancodeW ScancodeS
        --zooming = Q/E
        (zoom, on3) = ax ScancodeQ ScancodeE
        --changing the julia C constant - U+Shift for the x component, I+Shift for the y component
        (dcx, on4) = axs ScancodeU
        (dcy, on5) = axs ScancodeI
        --Change the number of julia iterations - O+Shift. Use a separate call, since we want dits to be an int, not a float like the others
        (dits, on6) = axisInputShift pressed ScancodeO
        --Change the color frequency - P+Shift
        (dfreq, on7) = axs ScancodeP
        --Constants for all changing variables
        move_speed = 0.05   --move 5% of viewport size per frame
        zoom_speed = 0.1    --zoom by e^0.1 ~ 110% or e^-0.1 ~ 90% each frame
        c_speed = 0.005     --multiply c by e^0.005 ~ 100.5% or e^-0.005 ~ 99.5% each frame
        freq_speed = 0.05   --multiply frequency by e^0.05 of by e^-0.05 each frame
        --transform handling the viewport transformation - move, then zoom
        vp_transform = zoomViewport (exp $ zoom_speed*zoom) . moveViewport (move_speed*movex, move_speed*movey)
        c_tr = exp . (c_speed*) --julia C transformation
    in (
        --compute the new julia state by modifying any components necessary
        JuliaState (vp_transform vp) ((cx*c_tr dcx) :+ (cy*c_tr dcy)) (its+dits) (freq*exp (freq_speed*dfreq)),
        --return true if any inputs have changed (any key is pressed)
        or [on1, on2, on3, on4, on5, on6, on7]
    )





shouldQuit :: IO Bool
shouldQuit = do
    event <- pollEvent
    case event of
        Nothing -> return False
        Just ev ->
            if eventPayload ev == QuitEvent
                then return True
                else do shouldQuit




--perform one application step - process inputs, render the julia set if needed and update the window
appStep :: AppState -> IO (AppState, Bool)
appStep (AppState wnd frame_old full_resolution jstate_old) = do
    pressed_keys <- getKeyboardState    --get pressed keys
    jstate_scaled <- matchWindowRatio jstate_old wnd    --if the window ratio has changed, update the viewport accordingly
    --update julia state according to current input
    let (jstate@(JuliaState view julia_c iters color_freq), params_changed) = processInput pressed_keys jstate_scaled
    --update the surface if the window size has changed
    (frame, surf_changed) <- updateSurface wnd frame_old
    --get render surface dimensions
    (V2 fw fh) <- surfaceDimensions frame
    --if the params are changing currently, render in 4 times lower resolution in each dimension. Otherwise use the surface resolution
    let (rw, rh) = if params_changed || surf_changed then (fw `div` 4, fh `div` 4) else (fw, fh)

    --get the window surface
    screen_surface <- getWindowSurface wnd
    
    --if the surface / viewport have changed render in low resolution. If they have changed the previous frame but not now, render in full resolution
    --If the full resolution is already rendered, do nothing
    if full_resolution && not params_changed && not surf_changed
    then do delay 20    --save some CPU time when not rendering
    else (do
        lockSurface frame
        pixels <- surfacePixels frame
        renderJulia pixels view (fromIntegral fw, fromIntegral fh) (fromIntegral rw, fromIntegral rh) julia_c iters color_freq
        unlockSurface frame)

    --Display the rendered julia fractal to screen
    surfaceBlitScaled frame (Just (Rectangle (P (V2 0 0)) (V2 rw rh))) screen_surface Nothing

    --Update the window to match the newly updated surface
    SDL.updateWindowSurface wnd

    

    --check whether the user clicked the X window button
    pumpEvents
    should_quit <- shouldQuit
    --return the new app state, and true if we should quit (X button / Esc key were pressed)
    return (AppState wnd (Just frame) (not params_changed && not surf_changed) jstate, pressed_keys ScancodeEscape || should_quit)


--The main app loop. Update the state, and call itself, if the app hasn't ended
appLoop :: AppState -> IO ()
appLoop state = do
    (new_state, done) <- appStep state
    unless done $ appLoop new_state
    



main :: IO ()
main = do
    --initialize SDL
    initializeAll
    --create a SDL window
    window <- createWindow "Julia fractal" defaultWindow
    --Create the default app state - viewport between (-1, -1) and (1, 1), c=0.285+0.01i, 100 iterations, 50 color frequency
    let app_state = AppState window Nothing False $ JuliaState (ViewportState (-1) (-1) 1 1) (0.285 :+ 0.01) 100 50

    --run the app
    appLoop app_state
    --destroy the SDL window
    destroyWindow window
