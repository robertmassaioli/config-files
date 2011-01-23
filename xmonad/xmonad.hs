{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}
 

-- XMonad configuration file by Thomas ten Cate <ttencate@gmail.com>
-- Edited and extended by Robert Massaioli <robertmassaioli@gmail.com>
-- 
-- Works on whatever XMonad that I happen to be running, usually the latest one.
-- You will need xmonad-contrib and maybe more.
--
-- This is designed to play nice with a standard Ubuntu installation.
--
-- WARNING: On my computers I swap CapsLock and BackSpace around so if you don't then you will need
-- to swap those around in this config.
 
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
import XMonad.Util.Dmenu
import XMonad.Util.Dzen 
import XMonad.Hooks.DynamicLog (dynamicLogXinerama, xmobar)
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Layout.IndependentScreens
import XMonad.Actions.Warp
import XMonad.Hooks.UrgencyHook
import System.Exit

import XMonad.Prompt
import XMonad.Actions.TagWindows

import XMonad.Actions.OnScreen (greedyViewOnScreen, viewOnScreen)
import Data.Maybe (fromMaybe)

import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

-- defaults on which we build
-- use e.g. defaultConfig or gnomeConfig
myBaseConfig = gnomeConfig
 
-- display
-- replace the bright red border with a more stylish colour
myBorderWidth = 1
myNormalBorderColor = "white"
myFocusedBorderColor = "purple"
 
-- workspaces
myWorkspaces = map show [1..12]
 
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
    skypeRoster     = ClassName "Skype" `And` 
                      Not (Title "Options") `And` 
                      Not (Role "Chats") `And` 
                      Not (Role "CallWindowForm")
 
myLayoutHook screens = fullscreen $ im normal 
  where
    normal     = tallLayout ||| wideLayout ||| singleLayout
    fullscreen = onWorkspaces (withScreens screens ["11"]) fullscreenLayout
    im         = onWorkspace "im" imLayout
 
-- special treatment for specific windows:
myManageHook = composeAll userDefinedHooks <+> manageHook myBaseConfig
   where
      userDefinedHooks =
         [ webBrowserHook
         , fileBrowserHook
         , terminalHook
         , imClientHook
         , allowFullscreenHook
         ]

      isTerminal = isApplicationGroup ["Gnome-terminal"]
      isIM     = isApplicationGroup ["Pidgin", "Skype"]
      isWebBrowser = isApplicationGroup ["X-www-browser", "Google-chrome", "Firefox"]
      isFileBrowser = isApplicationGroup ["Nautilus"]

      isApplicationGroup :: [String] -> Query Bool
      isApplicationGroup = foldr1 (<||>) . map (className =?)

      imClientHook = isIM --> addTagsHook ["im_client"]
      terminalHook = isTerminal --> addTagsHook ["terminal"]
      webBrowserHook = isWebBrowser --> addTagsHook ["browser"]
      fileBrowserHook = isFileBrowser --> addTagsHook ["file_browser"]

      -- this will allow anything that says it is fullscreen to go fullscreen: like youtube videos
      allowFullscreenHook = isFullscreen --> doFullFloat

      addTagsHook :: [String] -> ManageHook
      addTagsHook ts = do
         window <- ask
         liftX . sequence . fmap (flip addTag window) $ ts
         idHook
 
-- Mod4 is the Super / Windows key
-- alt, is well...alt
myModMask = mod4Mask
altMask   = mod1Mask
 
{-
 - Default Spawn Commands
 - Update these using update alternatives as in:
 -    $ update-alternatives --config x-www-browser
 -}
myBrowser = "x-www-browser"   
myTerminal = "x-terminal-emulator"

-- choose which menu runs
runMenu = spawn "dmenu_run"
-- runMenu = gnomeRun

-- better keybindings for dvorak
myKeys conf = M.fromList $
    [ ((myModMask              , xK_Return    ), spawn myTerminal)
    , ((myModMask              , xK_x         ), spawn myBrowser)
    , ((myModMask              , xK_r         ), runMenu)
    , ((myModMask              , xK_c         ), kill)
    -- Empty Workspace Movement
    , ((altMask                , xK_space     ), viewEmptyWorkspace)
    , ((altMask .|. shiftMask  , xK_space     ), tagToEmptyWorkspace)
    -- Layout Commands
    , ((myModMask              , xK_space     ), sendMessage NextLayout)
    , ((altMask                , xK_Return    ), sendMessage FirstLayout)
    , ((myModMask              , xK_n         ), refresh)
    , ((myModMask              , xK_m         ), windows S.swapMaster)
    -- controlling window movement, position and location
    , ((altMask                , xK_Tab       ), windows S.focusDown >> windowCenter)
    , ((altMask                , xK_BackSpace ), windows S.focusUp >> windowCenter)
    , ((myModMask              , xK_Down      ), windows S.swapDown)
    , ((myModMask              , xK_Up        ), windows S.swapUp)
    , ((myModMask              , xK_Left      ), sendMessage Shrink)
    , ((myModMask              , xK_Right     ), sendMessage Expand)
    , ((myModMask              , xK_t         ), withFocused $ windows . S.sink)
    , ((myModMask              , xK_w         ), sendMessage (IncMasterN 1))
    , ((myModMask              , xK_v         ), sendMessage (IncMasterN (-1)))
    -- Shutdown commands
    , ((myModMask              , xK_q         ), restart "xmonad" True)
    , ((myModMask              , xK_h         ), spawn "gksudo pm-hibernate")
    , ((myModMask .|. shiftMask, xK_q         ), spawn "gksudo shutdown -P now")
    , ((myModMask .|. shiftMask, xK_w         ), spawn "gnome-screensaver-command -l")
    -- Print Screen
    , ((myModMask              , xK_Print     ), spawn "gnome-screenshot")
    , ((myModMask .|. shiftMask, xK_Print     ), spawn "gnome-screenshot -a")
    -- MPC and Volume commands
    , ((myModMask               , xK_Page_Up), spawn "mpc next")
    , ((myModMask               , xK_Page_Down), spawn "mpc prev")
    , ((myModMask               , xK_Pause), spawn "mpc toggle")
    , ((myModMask               , xK_Home), spawn "amixer -c0 -- sset Master Playback 2dB+")
    , ((myModMask               , xK_End), spawn "amixer -c0 -- sset Master Playback 2dB-")
    -- Screen Movement
    , ((altMask .|. controlMask, xK_Left       ), prevScreen >> windowCenter)
    , ((altMask .|. controlMask, xK_Right      ), nextScreen >> windowCenter)
    , ((altMask .|. controlMask, xK_Down       ), shiftPrevScreen)
    , ((altMask .|. controlMask, xK_Up         ), shiftNextScreen)
    , ((altMask .|. controlMask .|. shiftMask, xK_Down       ), shiftPrevScreen >> prevScreen >> windowCenter)
    , ((altMask .|. controlMask .|. shiftMask, xK_Up         ), shiftNextScreen >> nextScreen >> windowCenter)
    , ((altMask .|. controlMask .|. shiftMask, xK_Left     ), swapPrevScreen) -- this does not work properly
    , ((altMask .|. controlMask .|. shiftMask, xK_Right     ), swapNextScreen)
    , ((myModMask              , xK_z         ), windowCenter)
    -- Tagging Windows
    , ((myModMask              ,   xK_g  ), tagPrompt defaultXPConfig (withFocused . addTag))
    , ((myModMask .|. shiftMask,   xK_g  ), tagDelPrompt defaultXPConfig)
    , ((altMask                ,   xK_g  ), tagPrompt defaultXPConfig (`withTaggedGlobalP` gotoWindow))
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
    [ ((myModMask .|. altMask, k), windowsShift i >> windowsGreedyView i >> windowCenter) 
        | (i, k) <- zip (workspaces' conf) workspaceKeys
    ]
    where 
        workspaceKeys = [xK_F1 .. xK_F12]
        windowsShift      = windows . onCurrentScreen S.shift
        windowsGreedyView = windows . onCurrentScreen S.greedyView
        windowCenter = warpToWindow (1 % 6) (1 % 6)

        gotoWindow :: Window -> WindowSet -> WindowSet
        gotoWindow window ws = case S.findTag window ws of
                                 Just i -> viewOnScreen (screenIdFromTag i) i ws
                                 Nothing -> ws
            where
               screenIdFromTag :: WorkspaceId -> ScreenId
               screenIdFromTag = S . read . takeWhile (/= '_')
 
-- mouse bindings that mimic Gnome's
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    [ ((altMask, button1), \w -> focus w >> mouseMoveWindow w)
    , ((altMask, button2), \w -> focus w >> mouseResizeWindow w)
    , ((altMask, button3), \w -> focus w >> withFocused (windows . S.sink))
    , ((altMask, button4), const $ windows S.swapUp)
    , ((altMask, button5), const $ windows S.swapDown)
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
    let ws = S.integrate' stack
    rosters <- filterM (hasAnyProperty props) ws
    let n = fromIntegral $ length rosters
    let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
    let rosterRects = splitHorizontally n rostersRect
    let filteredStack = stack >>= S.filter (`notElem` rosters)
    (a,b) <- runLayout (wksp {S.stack = filteredStack}) chatsRect
    return (zip rosters rosterRects ++ a, b)
