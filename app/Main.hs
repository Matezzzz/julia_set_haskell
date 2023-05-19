{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Main where


import SDL
import Control.Monad (unless, join)
import SDL.Raw (PixelFormat(PixelFormat), lockSurface, unlockSurface, freeSurface, updateWindowSurface, getKeyboardState, Event (keyboardEventKeysym), Keysym (keysymKeycode), blitSurface, createRGBSurface)
import Foreign (Storable(pokeElemOff), Ptr, castPtr, Word8)
import Data.Colour.RGBSpace.HSL
import Data.Colour.RGBSpace (RGB(channelGreen), RGB(channelRed), RGB(channelBlue), Colour)


data MyColor = MyColor {colorR::Word8, colorG::Word8, colorB::Word8}



juliaIter :: Float -> Float -> Float -> Float -> Int -> Int
juliaIter zx zy cx cy iters
    | iters == 0 = 0
    | otherwise =
        let len = sqrt (zx*zx + zy * zy)
            new_zx = zx * zx - zy * zy + cx
            new_zy = 2 * zx * zy + cy
        in if len > 2 then 0 else juliaIter new_zx new_zy cx cy (iters-1) + 1


toMyColor:: RGB Double -> MyColor
toMyColor rgb = MyColor (round (255*channelRed rgb)) (round (255*channelGreen rgb)) (round (255*channelBlue rgb))


paletteColor:: Int -> MyColor
paletteColor i = toMyColor (hsl (360/12*fromIntegral i) 1.0 0.5)

juliaColor :: (Float, Float) -> (Float, Float) -> Int -> MyColor
juliaColor (zx, zy) (cx, cy) max_iters
    | depth == max_iters = MyColor 0 0 0
    | otherwise = Main.paletteColor depth
    where depth = juliaIter zx zy cx cy max_iters


data ViewportState = ViewportState {fromX::Float, fromY::Float, toX::Float, toY::Float}

middleX (ViewportState fx _ tx _) = (tx+fx)/2
middleY (ViewportState _ fy _ ty) = (ty+fy)/2

sizeX (ViewportState fx _ tx _) = tx-fx
sizeY (ViewportState _ fy _ ty) = ty-fy

lerp k a b = a * (1-k) + b * k


divFloat :: Int -> Int -> Float
x `divFloat` y = fromIntegral x / fromIntegral y

transformCoordinates::ViewportState->(Int, Int)->(Int, Int)->(Float, Float)
transformCoordinates (ViewportState fx fy tx ty) (x, y) (w, h) = (Main.lerp (x `divFloat` (w-1)) fx tx, Main.lerp (y `divFloat` (h-1)) fy ty)



zoomViewport::ViewportState -> Float -> ViewportState
zoomViewport vs@(ViewportState fx fy tx ty) k =
    let lp = Main.lerp k
        mx = middleX vs
        my = middleY vs
    in ViewportState (lp fx mx) (lp fy my) (lp tx mx) (lp ty my)


moveViewportX (ViewportState fx fy tx ty) dx = ViewportState (fx+dx) fy (tx+dx) ty

moveViewportY (ViewportState fx fy tx ty) dy = ViewportState fx (fy+dy) tx (ty+dy)



getKeysPressed :: [SDL.Event] -> [Keycode]
getKeysPressed = map (\(KeyboardEvent e) -> SDL.keysymKeycode (SDL.keyboardEventKeysym e)) . filter (\case
    KeyboardEvent e -> keyboardEventKeyMotion e == Pressed
    _ -> False) . map eventPayload


writePixel::Ptr () -> Int -> Int -> Int -> MyColor -> IO ()
writePixel ptr x y w color = do
    let pos = w * y + x
        c = 4
    pokeElemOff (castPtr ptr) (c*pos+0) (255::Word8)
    pokeElemOff (castPtr ptr) (c*pos+1) (colorB color)
    pokeElemOff (castPtr ptr) (c*pos+2) (colorG color)
    pokeElemOff (castPtr ptr) (c*pos+3) (colorR color)






renderJulia::Ptr () -> ViewportState -> (Int, Int) -> (Float, Float) -> Int -> IO ()
renderJulia ptr viewport size@(w, h) c max_iters = do
    let indices = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
    mapM_ (\(x, y) -> do writePixel ptr x y w (juliaColor (transformCoordinates viewport (x, y) size) c max_iters)) indices




data JuliaState = JuliaState{
    viewport :: ViewportState,
    juliaC :: (Float, Float),
    juliaIters :: Int
}

data AppState = AppState{
    window :: Window,
    frameSurface :: Surface,
    juliaState :: JuliaState
}

updateSurface :: AppState -> IO Surface
updateSurface s@(AppState wnd surf _) = do
    ss <- surfaceDimensions surf
    ws <- get (windowSize wnd)
    if ws == ss
        then return surf
        else do SDL.createRGBSurface ws RGBA8888

processInput :: [Keycode] -> JuliaState -> JuliaState
processInput pressed (JuliaState vp c its) =
    let movex = (if KeycodeD `elem` pressed then 0.05 else 0) + (if KeycodeA `elem` pressed then (-0.05) else 0)
        movey = (if KeycodeW `elem` pressed then 0.05 else 0) + (if KeycodeS `elem` pressed then (-0.05) else 0)
        zoom = (if KeycodeE `elem` pressed then (-0.05) else 0) + (if KeycodeQ `elem` pressed then 0.05 else 0)
    in JuliaState (zoomViewport (moveViewportX (moveViewportY vp movey) movex) zoom) c its


appLoop :: AppState -> IO ()
appLoop state = do
    events <- pollEvents
    _ <- pumpEvents
    let pressed_keys = getKeysPressed events
        wnd = window state
        jstate = processInput pressed_keys $ juliaState state
    frame <- updateSurface state
    (V2 fw fh) <- surfaceDimensions frame
    
    screen_surface <- getWindowSurface wnd
    surfaceFillRect screen_surface Nothing (V4 50 50 50 255)
    
    SDL.lockSurface frame
    pixels <- surfacePixels frame
    
    renderJulia pixels (viewport jstate) (fromIntegral fh, fromIntegral fw) (juliaC jstate) (juliaIters jstate)
    SDL.unlockSurface frame

    surfaceBlit frame Nothing screen_surface Nothing

    SDL.updateWindowSurface wnd

    unless (KeycodeEscape `elem` pressed_keys) $ appLoop $ AppState wnd frame jstate




main :: IO ()
main = do
    initializeAll
    window <- createWindow "Julia fractal" defaultWindow

    frame <- SDL.createRGBSurface (V2 300 300) RGBA8888
    let app_state = AppState window frame $ JuliaState (ViewportState (-1) (-1) 1 1) (0.285, 0.01) 256
    appLoop app_state
    destroyWindow window
