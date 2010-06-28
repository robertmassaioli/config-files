{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}
 

-- XMonad configuration file by Thomas ten Cate <ttencate@gmail.com>
-- Edited and extended by Robert Massaioli <robertmassaioli@gmail.com>
-- 
-- Works on xmonad-0.8, NOT on 0.7 or below; and of course
-- xmonad-contrib needs to be installed as well
--
-- This is designed to play nice with a standard Ubuntu Hardy installation.
-- For more explanation please see the readme on github robertmassaioli/xmonad.hs
 
import XMonad
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as S
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Util.WindowProperties
import Control.Monad
import Data.Ratio
import qualified Data.Map as M
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Layout.IndependentScreens
import XMonad.Actions.Warp
import XMonad.Hooks.UrgencyHook
import System.Exit

-- TODO Bitlbee
-- TODO Stack for pushing and popping windows so that I only have to deal with as many as I want to.
 
-- defaults on which we build
-- use e.g. defaultConfig or gnomeConfig
myBaseConfig = gnomeConfig
 
-- display
-- replace the bright red border with a more stylish colour
myBorderWidth = 1
myNormalBorderColor = "white"
myFocusedBorderColor = "purple"
 
-- workspaces
--myWorkspaces = ["web", "editor", "terms"] ++ (miscs 7) ++ ["fullscreen", "im"]
myWorkspaces = ["web"] ++ (miscs 9) ++ ["fullscreen", "im"]
    where miscs = map (("misc" ++) . show) . (flip take) [1..]
isFullscreen = (== "fullscreen")
 
-- layouts
basicLayout = Tall nmaster delta ratio 
  where
    nmaster = 1
    delta   = 3 / 100
    ratio   = 1 / 2
tallLayout = named "tall" . avoidStruts $ basicLayout
wideLayout = named "wide" . avoidStruts $ Mirror basicLayout
singleLayout = named "single" . avoidStruts $ noBorders Full
fullscreenLayout = named "fullscreen" $ noBorders Full
imLayout = avoidStruts . reflectHoriz $ withIMs ratio rosters chatLayout 
  where
    chatLayout      = Grid
    ratio           = 1 % 6
    rosters         = [skypeRoster, pidginRoster]
    pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
    skypeRoster     = (ClassName "Skype") `And` 
                      (Not (Title "Options")) `And` 
                      (Not (Role "Chats")) `And` 
                      (Not (Role "CallWindowForm"))
 
myLayoutHook screens = fullscreen $ im $ normal 
  where
    normal     = tallLayout ||| wideLayout ||| singleLayout
    fullscreen = onWorkspaces (withScreens screens ["fullscreen"]) fullscreenLayout
    im         = onWorkspace "im" imLayout
 
-- special treatment for specific windows:
-- put the Pidgin and Skype windows in the im workspace
myManageHook = imManageHooks <+> manageHook myBaseConfig
  where
    imManageHooks :: ManageHook
    imManageHooks = composeAll [isIM --> moveToIM] 
      where
        isIM     = foldr1 (<||>) [isPidgin, isSkype]
        isPidgin = className =? "Pidgin"
        isSkype  = className =? "Skype"
        moveToIM = doF $ S.shift "im"
 
-- Mod4 is the Super / Windows key
-- alt, is well...alt
myModMask = mod4Mask
altMask   = mod1Mask
 
browserCmd :: String
browserCmd = "x-www-browser"    -- set this up using 'update-alternatives --config x-www-browser' in the terminal
{- 
 - You could also use the following if your system does not have update-alternatives:
 -    browserCmd = "firefox"
 -    browserCmd = "google-chrome"
 -}

-- I think this function can be written as a really nice fold but I cannot see how
-- it seems like I should be able to though.

-- Currently as well the keyMask affects the actual key that is pressed so even if we get a 
-- xK_a in dvorak for example, if we get a shiftMask then we should return different things for
-- the special keys there are keys that are common though so those should only be written once.
-- This should also be auto-generated from something else that is already done. Some other keyboard
-- mapping files.

convertKeys :: (KeySym -> KeySym) -> [((KeyMask, KeySym), X ())] -> [((KeyMask, KeySym), X ())]
convertKeys convertKey (((mod_key, key), action):rest) = 
  ((mod_key, convertKey key), action) : convertKeys convertKey rest

lookupKey :: M.Map KeySym KeySym -> KeySym -> KeySym
lookupKey mapping key = case M.lookup key mapping of
                          Just a -> a
                          Nothing -> key

-- choose which menu runs
runMenu = spawn "dmenu_run"
-- runMenu = gnomeRun

-- better keybindings for dvorak
myKeys conf = M.fromList $
    [ ((myModMask              , xK_Return    ), spawn $ XMonad.terminal conf)
    , ((myModMask              , xK_r         ), runMenu)
    , ((myModMask              , xK_c         ), kill)
    , ((altMask                , xK_space     ), viewEmptyWorkspace)
    , ((altMask .|. shiftMask  , xK_space     ), tagToEmptyWorkspace)
    , ((myModMask              , xK_space     ), sendMessage NextLayout)
    , ((altMask                , xK_Return    ), sendMessage FirstLayout)
    , ((myModMask              , xK_n         ), refresh)
    , ((myModMask              , xK_m         ), windows S.swapMaster)
    -- jumping bettween windows
    , ((altMask                , xK_Tab       ), windows S.focusDown)
    , ((altMask                , xK_BackSpace ), windows S.focusUp)
    , ((myModMask              , xK_Down      ), windows S.swapDown)
    , ((myModMask              , xK_Up        ), windows S.swapUp)
    , ((myModMask              , xK_Left      ), sendMessage Shrink)
    , ((myModMask              , xK_Right     ), sendMessage Expand)
    , ((myModMask              , xK_t         ), withFocused $ windows . S.sink)
    , ((myModMask              , xK_w         ), sendMessage (IncMasterN 1))
    , ((myModMask              , xK_v         ), sendMessage (IncMasterN (-1)))
    , ((myModMask              , xK_q         ), restart "xmonad" True)
    , ((myModMask              , xK_h         ), spawn "pmi action hibernate")
    -- Print Screen
    , ((myModMask              , xK_Print     ), spawn "gnome-screenshot")
    , ((myModMask .|. shiftMask, xK_Print     ), spawn "gnome-screenshot -a")
    -- Save Session
    --, ((myModMask .|. shiftMask, xK_q         ), spawn "gnome-session-save --shutdown-dialog")
    , ((myModMask .|. shiftMask, xK_q         ), io $ exitWith ExitSuccess)
    , ((myModMask .|. shiftMask, xK_w         ), spawn "gnome-screensaver-command -l")
    -- MPC commands
    , ((myModMask               , xK_Page_Up), spawn "mpc next")
    , ((myModMask               , xK_Page_Down), spawn "mpc prev")
    , ((myModMask               , xK_Pause), spawn "mpc toggle")
    -- Open Browser
    , ((myModMask              , xK_x         ), spawn browserCmd)
    -- Alt + Ctrl Left / Right makes the view go left and right
    , ((altMask .|. controlMask, xK_Left       ), prevScreen >> windowCenter)
    , ((altMask .|. controlMask, xK_Right      ), nextScreen >> windowCenter)
    , ((altMask .|. controlMask, xK_Down       ), shiftPrevScreen)
    , ((altMask .|. controlMask, xK_Up         ), shiftNextScreen)
    , ((altMask .|. controlMask .|. shiftMask, xK_Down       ), shiftPrevScreen >> prevScreen >> windowCenter)
    , ((altMask .|. controlMask .|. shiftMask, xK_Up         ), shiftNextScreen >> nextScreen >> windowCenter)
    , ((altMask .|. controlMask .|. shiftMask, xK_Left     ), swapPrevScreen) -- this does not work properly
    , ((altMask .|. controlMask .|. shiftMask, xK_Right     ), swapNextScreen)
    , ((myModMask               , xK_z         ), windowCenter)
    ] ++
    -- Alt+F1..F10 switches to workspace
    -- mod+F1..F10 moves window to workspace
    -- (Alt is in a nicer location for the thumb than the Windows key,
    -- and 1..9 keys are already in use by Firefox, irssi, ...)
    [ ((m, k), f i)
        | (i, k) <- zip (workspaces' conf) workspaceKeys
        , (f, m) <- [(windowsGreedyView, altMask), (windowsShift, myModMask)]
    ] ++
    -- mod+alt+F1..F10 moves window to workspace and switches to that workspace
    [ ((myModMask .|. altMask, k), (windowsShift i) >> (windowsGreedyView i) >> windowCenter) 
        | (i, k) <- zip (workspaces' conf) workspaceKeys
    ]
    where 
        workspaceKeys = [xK_F1 .. xK_F12]
        windowsShift      = windows . onCurrentScreen S.shift
        windowsGreedyView = windows . onCurrentScreen S.greedyView
        windowCenter = warpToWindow (1 % 6) (1 % 6)
 
-- mouse bindings that mimic Gnome's
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((altMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((altMask, button2), (\w -> focus w >> mouseResizeWindow w))
    , ((altMask, button3), (\w -> focus w >> (withFocused $ windows . S.sink)))
    , ((altMask, button4), (const $ windows S.swapUp))
    , ((altMask, button5), (const $ windows S.swapDown))
    ]
 
-- put it all together
main = do
    nScreens <- countScreens    -- just in case you are on a laptop like me count the screens so that you can go
    xmonad =<< xmobar myBaseConfig
      { modMask = myModMask
      , workspaces = withScreens nScreens myWorkspaces
      , layoutHook = myLayoutHook nScreens
      , manageHook = myManageHook
      , borderWidth = myBorderWidth
      , normalBorderColor = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , keys = myKeys
      , mouseBindings = myMouseBindings
      , logHook = myLogHook
      }
    where
        myLogHook = dynamicLogXinerama
 
-- modified version of XMonad.Layout.IM --
 
-- | Data type for LayoutModifier which converts given layout to IM-layout
-- (with dedicated space for the roster and original layout for chat windows)
data AddRosters a = AddRosters Rational [Property] 
                    deriving (Read, Show)
 
instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                = "IMs"
 
-- | Modifier which converts given layout to IMs-layout (with dedicated
-- space for rosters and original layout for chat windows)
withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props
 
-- | IM layout modifier applied to the Grid layout
gridIMs :: Rational -> [Property] -> ModifiedLayout AddRosters Grid a
gridIMs ratio props = withIMs ratio props Grid
 
hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w
 
-- | Internal function for placing the rosters specified by
-- the properties and running original layout for all chat windows
applyIMs :: (LayoutClass l Window) =>
               Rational
            -> [Property]
            -> S.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
    let stack = S.stack wksp
    let ws = S.integrate' $ stack
    rosters <- filterM (hasAnyProperty props) ws
    let n = fromIntegral $ length rosters
    let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
    let rosterRects = splitHorizontally n rostersRect
    let filteredStack = stack >>= S.filter (`notElem` rosters)
    (a,b) <- runLayout (wksp {S.stack = filteredStack}) chatsRect
    return (zip rosters rosterRects ++ a, b)
