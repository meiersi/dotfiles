import qualified Data.Map as M

import XMonad
import XMonad.Config.Gnome

import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Actions.CycleWS

import XMonad.Hooks.ManageDocks
import XMonad.Layout.MosaicAlt
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.WindowNavigation
import XMonad.Layout.LayoutHints (layoutHints)

import System.Exit

-- import XMonad.Util.Run (spawnPipe)

main :: IO ()
main = xmonad gnomeConfig
  { manageHook = myManageHook
  , modMask    = mod4Mask
  , keys       = \c -> myKeys c `M.union` keys gnomeConfig c
  , layoutHook = myLayout
  , workspaces = myWorkspaces
  , startupHook = startup
  }
  where
    startup = do
        spawn "gnome-keyring-daemon"
        spawn "gnome-settings-daemon"
        spawn "synapse"
        spawn "xset r rate 250 80"

    myLayout = avoidStruts $ layoutHints $ showWName $ smartBorders $
        (navigable $ MosaicAlt M.empty) |||
        simpleTabbed

    navigable = configurableNavigation noNavigateBorders

myManageHook :: ManageHook
myManageHook = composeAll (
    [ manageHook gnomeConfig
    -- , className =? "Unity-2d-panel" --> doIgnore
    -- , className =? "Unity-2d-launcher" --> doFloat
    ])

myWorkspaces :: [String]
myWorkspaces = map (\i -> ">>  "++show i++"  <<") [1..9]

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, xK_p), spawn "krunner")
    -- , ((modm .|. shiftMask, xK_q), spawn "")
    , ((modm, xK_a), withFocused (sendMessage . expandWindowAlt))
    , ((modm, xK_z), withFocused (sendMessage . shrinkWindowAlt))
    , ((modm, xK_s), withFocused (sendMessage . tallWindowAlt))
    , ((modm, xK_d), withFocused (sendMessage . wideWindowAlt))

      -- close focused window
    , ((modm, xK_c), kill)

      -- Quit xmonad
    , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))

     -- reset the current layout
    , ((modm .|. controlMask,   xK_space ), sendMessage resetAlt)

      -- move focus to the next window
    , ((mod1Mask,               xK_Tab   ), windows W.focusDown)
      -- move focus to the previous window
    , ((mod1Mask .|. shiftMask, xK_Tab   ), windows W.focusUp)

    -- Navigate windows
    , ((modm,                 xK_Right), sendMessage $ Go R)
    , ((modm,                 xK_Left ), sendMessage $ Go L)
    , ((modm,                 xK_Up   ), sendMessage $ Go U)
    , ((modm,                 xK_Down ), sendMessage $ Go D)

      -- switching between workspaces
    , ((modm .|. controlMask, xK_Right), nextWS)
    , ((modm .|. controlMask, xK_Left ), prevWS)

      -- moving windows between adjacent workspaces
    , ((modm .|. shiftMask,   xK_Right), shiftToNext >> nextWS)
    , ((modm .|. shiftMask,   xK_Left ), shiftToPrev >> prevWS)

      -- swapping screens
    , ((modm,                 xK_x    ), swapNextScreen)

      -- shortcuts for often used software
    , ((modm .|. shiftMask,   xK_Return), spawn "gnome-terminal")
    , ((modm .|. shiftMask,   xK_h),      spawn "gvim")
    , ((modm .|. shiftMask,   xK_j),      spawn "firefox")
    , ((modm .|. shiftMask,   xK_n),      spawn "nautilus")
    , ((modm .|. shiftMask,   xK_i),      spawn "chromium-browser")
    , ((modm .|. shiftMask,   xK_o),      spawn "gksu synaptic")
    ]
