Robert Massaioli's xmonad.hs
============================

This is my xmonad.hs file that I want to share with everyone, for a full set of its features then I reccomend
opening the file itself and having a read. Meant for programmers that use Dvorak keyboards.

Instalation
-----------

Check out this repository in a convinient location. Then do the following:

    mv ~/.xmonad/xmonad.hs ~/.xmonad/xmonad.hs.old
    ln -s xmonad.hs ~/.xmonad/

And then restart xmonad using whatever key you have restart mapped to. Please note that this xmonad.hs was supposed to be used on a dvorak keyboard.

Command Guide
-------------

XMonad configuration file by Thomas ten Cate <ttencate@gmail.com>  
Edited and extended by Robert Massaioli <robertmassaioli@gmail.com>

Works on xmonad-0.8, NOT on 0.7 or below; and of course
xmonad-contrib needs to be installed as well.

This is designed to play nice with a standard Ubuntu Hardy (or greater) installation.
It gives you 12 workspaces per screen, available through Alt+F1..F12. You can move
windows to a workspace with Win+F1..F12. You can do both at the same time
by pressing Win+Alt+F1..F12. It should work well with other *nix operating systems
too.

All workspaces except F11 respect panels and docks.  
F11 is the fullscreen workspace (for mplayer, etc.).  
F12 is the instant messaging workspace.  
When you switch screens the mouse will go with you.  

Pidgin and Skype windows are automatically placed onto the IM workspace.
Their contact lists each get a column on the right side of the screen,
and all their other windows (chat windows, etc.) go into a grid layout
in the remaining space.
(This uses a copied and modified version of XMonad.Layout.IM.)

Keybindings mostly use the Windows key, but some use Alt to mimic other
window managers. In general: Alt is used for navigation, Win for modification.
Some of the bindings resemble the XMonad defaults, but most don't.
The bindings are set up to be comfortable to use on a dvorak keyboard layout.

Before we have the commands lets make some word definitions:
window = The window that a program runs in.
Workspace = One screen with many windows.
Screen = A physical computer screen that you will look at.

__Screen management:__
>  Ctrl+Alt+Left/Right        move focus to previous/next screen  
>  Ctrl+Alt+Down/Up           switch current window to current workspace on previous/next screen  
>  Ctrl+Alt+Shift+Left/Right  swap all windows in current screen with previous/next screen  

__Navigation:__  
>  Alt+Space            switch to free workspace  
>  Alt+F1..F12          switch to workspace on the same screen  
>  Alt+Tab              focus next window on same screen  
>  Alt+BackSpace        focus previous window on the same screen (Meant to be in the CapsLock position but my layout is different)  
>  Win+Z                The mouse it moved one sixth of the way in from the top left of the window.  

__Window management:__  
>  These commands all keep windows in the same screen.
>  Alt+Shift+Space      move current window to free workspace 
>  Win+F1..F12          move window to workspace
>  Win+Alt+F1..F12      move window to workspace and switch to that workspace  
>  Win+Up/Down          move window up/down on  
>  Win+C                close window  
>  Alt+ScrollUp/Down    move focused window up/down  
>  Win+M                move window to master area  
>  Win+N                refresh the current window  
>  Alt+LMB              move floating window  
>  Alt+MMB              resize floating window  
>  Alt+RMB              unfloat floating window  
>  Win+T                unfloat floating window  
 
__Layout management:__  
>  Win+Left/Right       shrink/expand master area  
>  Win+W/V              move more/less windows into master area  
>  Win+Space            cycle layouts  
>  Alt+Space            Go back to the first layout  

__Other:__  
>  Win+Enter            start a terminal  
>  Win+R                open the dmenu dialogue (you can make it gnomeRun if you wish)  
>  Win+Q                restart XMonad  
>  Win+B                opens your browser  
>  Win+Shift+Q          display Gnome shutdown dialog  
