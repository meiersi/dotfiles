import XMonad
import XMonad.Config.Gnome
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import System.Exit
import System.IO

import XMonad.Actions.CycleWS

import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Reflect
import XMonad.Layout.Named
import XMonad.Layout.IM
import XMonad.Layout.Grid

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.Shell
import XMonad.Prompt.AppLauncher as AL

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)

import Graphics.X11.ExtraTypes.XF86(xF86XK_HomePage,
                                    xF86XK_Search,
                                    xF86XK_Mail,
                                    xF86XK_AudioMute,
                                    xF86XK_AudioLowerVolume,
                                    xF86XK_AudioRaiseVolume,
                                    xF86XK_AudioPlay,
                                    xF86XK_AudioNext,
                                    xF86XK_AudioPrev,
                                    xF86XK_Back,
                                    xF86XK_Forward,
                                    xF86XK_Sleep,
                                    xF86XK_PowerOff)

leftMargin = named "LeftMargin" (noBorders $ reflectHoriz $ withIM (0.7) (Const True) Full)

layout = avoidStruts $ smartBorders $ noBorders Full ||| tiled ||| Mirror tiled ||| leftMargin ||| Grid ||| threecol
  where
     threecol = named "ThreeColMid" (ThreeColMid nmaster delta ratio)
     tiled    = Tall nmaster delta ratio
     nmaster  = 1
     ratio    = 1/2
     delta    = 3/100

withOtherWorkspace f ws = f (otherWorkspace ws) ws
    where
        otherWorkspace = W.tag . W.workspace . head . W.visible

myWorkspaces = map show ([1..9] ++ [0]) ++ ["-"] ++
    map ((++) "F" . show) [1..12]

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ((modMask,               xK_slash ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask .|. shiftMask, xK_h), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask .|. shiftMask, xK_l), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- toggle the status bar gap
    , ((modMask              , xK_b     ), sendMessage ToggleStruts)
    , ((modMask .|. shiftMask, xK_b     ), sendMessage $ ToggleStrut U)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True) -- %! Restart xmonad

    -- find free workspaces
    , ((modMask,                xK_m    ), moveTo Next EmptyWS)
    , ((modMask .|. shiftMask,  xK_m    ), shiftTo Next EmptyWS)

    , ((modMask,                xK_period), moveTo Next HiddenNonEmptyWS)
    , ((modMask,                xK_comma), moveTo Prev HiddenNonEmptyWS)
    , ((modMask .|. shiftMask,  xK_period), nextScreen)
    , ((modMask .|. shiftMask,  xK_comma), prevScreen)

    -- swap workspaces on multihead setup
    , ((modMask .|. shiftMask,  xK_slash), windows $ withOtherWorkspace W.greedyView)

    -- xlock
    , ((modMask .|. shiftMask,  xK_x), spawn "slock")

    -- suspend
    -- , ((modMask .|. shiftMask,  xK_s), spawn "suspend.sh")

    -- nautilus
    --, ((modMask,                xK_n), spawn "nautilus")

    -- media keys
    -- ,((0, xF86XK_AudioMute),        spawn "amixer set Master toggle")
    -- ,((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2%-")
    -- ,((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2%+")
    -- ,((0, xF86XK_AudioPlay),        spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    -- ,((0, xF86XK_AudioNext),        spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
    -- ,((0, xF86XK_AudioPrev),        spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")

    -- prompts
    , ((modMask,                xK_o), windowPromptGoto myXPConfig)
    , ((modMask,                xK_space), shellPrompt myXPConfig)
    --, ((modMask,                xK_i), AL.launchApp myXPConfig "hamster-cli")
    --, ((modMask,                xK_u), AL.launchApp myXPConfig "recoll -q")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf)
        ([xK_1 .. xK_9] ++ [xK_0] ++ [xK_minus] ++ [xK_F1 .. xK_F12])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        -- for normal xinerama
        -- | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        -- for reverse xinerama
        | (key, sc) <- zip [xK_e, xK_r, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myXPConfig = defaultXPConfig
    { bgColor           = "black"
    , fgColor           = "white"
    , bgHLight          = "gray"
    , fgHLight          = "black"
    , borderColor       = "#27508b"
    , promptBorderWidth = 1
    , position          = Top
    , height            = 20
    , font              = "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*"
    --, autoComplete      = Just 1000
    , historySize       = 1000
    , promptKeymap      = M.fromList [((controlMask,xK_c), quit)]
                            `M.union` promptKeymap defaultXPConfig
    }

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad gnomeConfig {
      terminal           = "gnome-terminal"
    , normalBorderColor  = "#303030"
    , focusedBorderColor = "#27508b"
    , layoutHook         = layout --smartBorders (layoutHook gnomeConfig)
    , keys               = myKeys
    , workspaces         = myWorkspaces
    , modMask            = mod1Mask
    , handleEventHook    = fullscreenEventHook
    , startupHook = startupHook gnomeConfig >> setWMName "LG3D"
    , logHook = dynamicLogWithPP xmobarPP {
                ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "#275599" "" . shorten 90
            }
    }
