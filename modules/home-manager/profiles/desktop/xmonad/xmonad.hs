{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (IOException, handle)
import Control.Monad (forM, when)
import qualified DBus as D
import qualified DBus.Client as D
import Data.Bifunctor (first)
import Data.List (elemIndex, find, isPrefixOf, nub, sort)
import Data.Maybe (catMaybes, isJust)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Graphics.X11.ExtraTypes.XF86
import System.Directory (doesDirectoryExist, doesPathExist, getHomeDirectory, listDirectory)
import System.FilePath ((</>))
import XMonad
import XMonad.Actions.CycleWS (shiftNextScreen, swapNextScreen)
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace, addWorkspace, removeEmptyWorkspaceAfter, renameWorkspaceByName)
import XMonad.Actions.EasyMotion (selectWindow)
import qualified XMonad.Actions.EasyMotion as EM
import XMonad.Actions.FocusNth (swapNth)
import XMonad.Actions.Minimize (maximizeWindowAndFocus, minimizeWindow, withLastMinimized, withMinimized)
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.FloatConfigureReq (fixSteamFlicker)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.OnPropertyChange (onXPropertyChange)
import XMonad.Hooks.StatusBar.PP (filterOutWsPP)
import XMonad.Hooks.UrgencyHook (UrgencyHook (..), readUrgents, withUrgencyHook)
import XMonad.Hooks.WorkspaceHistory (workspaceHistory, workspaceHistoryHook)
import XMonad.Layout.BoringWindows (boringWindows, focusDown, focusMaster, focusUp)
import XMonad.Layout.Decoration (DecorationStyle (..), decoration, fi, findWindowByDecoration)
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.MultiToggle (Toggle (Toggle), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR))
import XMonad.Layout.Renamed (Rename (CutWordsLeft, Replace), renamed)
import XMonad.Layout.ResizableThreeColumns (MirrorResize (MirrorExpand, MirrorShrink), ResizableThreeCol (ResizableThreeCol, ResizableThreeColMid))
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.Spacing (spacingWithEdge)
import XMonad.Layout.Tabbed (Theme (..), shrinkText)
import XMonad.Layout.TwoPanePersistent (TwoPanePersistent (TwoPanePersistent))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, removeKeysP)
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadAction, namedScratchpadManageHook, scratchpadWorkspaceTag)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

-- Theme colors (substituted by Nix at build time)
colorBase00 = "@colorBase00@"

colorBase01 = "@colorBase01@"

colorBase03 = "@colorBase03@"

colorBase04 = "@colorBase04@"

colorBase05 = "@colorBase05@"

colorBase06 = "@colorBase06@"

colorBase08 = "@colorBase08@"

fontMono = "@fontMono@"

fontSize = "@fontSize@"

notifySend = "@notifySend@"

main = do
  logOutput <- setupLogOutput
  nScreens <- countScreens
  xmonad
    . docks
    . ewmhFullscreen
    . ewmh
    . withUrgencyHook NotifyUrgencyHook
    $ def
      { borderWidth = 6,
        normalBorderColor = colorBase03,
        focusedBorderColor = colorBase06,
        layoutHook = myLayoutHook,
        logHook = workspaceHistoryHook <+> mkPolybarLogHook logOutput,
        manageHook = myManageHook,
        handleEventHook = myHandleEventHook,
        modMask = mod4Mask,
        terminal = "ghostty",
        workspaces = map show [1 .. max 1 nScreens]
      }
      `additionalKeysP` [ -- focus
                          ("M-j", focusDown),
                          ("M-k", focusUp),
                          ("M-m", focusMaster),
                          ("M-<Tab>", removeEmptyWorkspaceAfter historyToggle),
                          ("M-S-u", focusAttention),
                          ("M-o", easyFocus),
                          ("M-S-o", easySwap),
                          -- resizable 3col
                          ("M-a", sendMessage MirrorShrink),
                          ("M-z", sendMessage MirrorExpand),
                          -- minimize/maximize
                          ("M-\\", withFocused minimizeWindow),
                          ("M-S-\\", withLastMinimized maximizeWindowAndFocus),
                          ("M-x", sendMessage $ Toggle MIRROR),
                          -- workspaces (base = go, shift = send window there,
                          -- ctrl = name the current workspace)
                          ("M-n", projectPrompt),
                          ("M-S-n", shiftPrompt),
                          ("M-i", openPrompt),
                          ("M-S-i", shiftToOpenPrompt),
                          ("M-C-n", renamePrompt),
                          -- screens (M-w/M-e focus left/right by default; r swaps
                          -- the two screens, S-r sends the focused window across)
                          ("M-r", swapNextScreen),
                          ("M-S-r", shiftNextScreen),
                          -- launchers
                          ( "M-S-<Return>",
                            spawnInProject
                              (\p -> "ghostty --working-directory=" ++ shellQuote p)
                              "ghostty"
                          ),
                          ("M-p", spawn "rofi -show-icons -show drun"),
                          ("M-S-p", spawn "rofi -show-icons -show run"),
                          ("M-0", spawn "rofi-rbw --keybindings Ctrl-1:type:username:password,Ctrl-2:type:password,Ctrl-3:copy:password,Ctrl-4:type:totp"),
                          ("M-y", spawn "emacsclient -c -n -e '(switch-to-buffer nil)'"),
                          ("M-u", spawn "chromium-browser"),
                          ("M-s", spawn "dm-tool switch-to-greeter"),
                          ("<Print>", spawn "flameshot gui"),
                          ("S-<Print>", spawn "flameshot screen -c"),
                          -- scratchpads (M-; submap)
                          ("M-; b", namedScratchpadAction scratchpads "btop"),
                          ("M-; v", namedScratchpadAction scratchpads "pavucontrol"),
                          ("M-; c", namedScratchpadAction scratchpads "emacs-org-capture"),
                          ("M-; a", namedScratchpadAction scratchpads "emacs-org-agenda"),
                          ("M-; =", namedScratchpadAction scratchpads "emacs-calc"),
                          ("M-; m", namedScratchpadAction scratchpads "spotify"),
                          -- rofi menus (M-' submap)
                          ("M-' s", spawn "rofi -show ssh"),
                          ("M-' e", spawn "rofi -show emoji"),
                          ("M-' c", spawn "rofi -show calc -no-show-match -no-sort"),
                          ("M-' w", spawn "rofi -show-icons -show window")
                        ]
      `additionalKeys` ( first (noModMask,)
                           <$> [ (xF86XK_MonBrightnessUp, spawn "xbacklight -inc 5"),
                                 (xF86XK_MonBrightnessDown, spawn "xbacklight -dec 5"),
                                 (xF86XK_AudioMute, spawn "@wpctl@ set-mute @DEFAULT_AUDIO_SINK@ toggle"),
                                 (xF86XK_AudioLowerVolume, spawn "@wpctl@ set-volume @DEFAULT_AUDIO_SINK@ 5%-"),
                                 (xF86XK_AudioRaiseVolume, spawn "@wpctl@ set-volume @DEFAULT_AUDIO_SINK@ 5%+")
                               ]
                       )
      `removeKeysP` ( ["M-" ++ [k] | k <- "123456789"]
                        ++ ["M-S-" ++ [k] | k <- "123456789"]
                    )

myLayoutHook =
  diminish (spacingWithEdge 6)
    . diminish minimize
    . boringWindows
    . avoidStruts
    . mkToggle (single MIRROR)
    $ layout
  where
    layout =
      renamed [Replace "ThreeCol"] (ResizableThreeCol 1 (3 % 100) (1 % 2) [])
        ||| renamed [Replace "Tabbed"] (decoration shrinkText tabbedTheme (GapTab tabGap) Simplest)
        ||| renamed [Replace "ThreeColMid"] (ResizableThreeColMid 1 (3 % 100) (1 % 2) [])
        ||| renamed [Replace "TwoPane"] (TwoPanePersistent Nothing (3 % 100) (1 % 2))

    tabbedTheme =
      def
        { activeColor = colorBase06,
          inactiveColor = colorBase03,
          urgentColor = colorBase08,
          activeBorderColor = colorBase06,
          inactiveBorderColor = colorBase03,
          urgentBorderColor = colorBase08,
          activeTextColor = colorBase01,
          inactiveTextColor = colorBase05,
          urgentTextColor = colorBase01,
          fontName = "xft:" ++ fontMono ++ ":size=" ++ fontSize ++ ":antialias=true",
          decoHeight = 44
        }

    tabGap = 6

    diminish = (renamed [CutWordsLeft 1] .)

-- A tabbed decoration that insets each tab horizontally by tabGap, leaving real
-- gaps between tab windows (the desktop shows through) instead of edge-to-edge
-- tabs. Mirrors XMonad.Layout.Tabbed's top-tab placement plus its click-to-focus
-- / middle-click-close handler.
newtype GapTab a = GapTab Dimension deriving (Read, Show)

instance (Eq a) => DecorationStyle GapTab a where
  describeDeco _ = "Tabbed"

  decorationEventHook _ ds ButtonEvent {ev_window = ew, ev_event_type = et, ev_button = eb}
    | et == buttonPress,
      Just ((w, _), _) <- findWindowByDecoration ew ds =
        if eb == button2 then killWindow w else focus w
  decorationEventHook _ _ _ = return ()

  shrink _ (Rectangle _ _ _ dh) (Rectangle x y w h) =
    Rectangle x (y + fi dh) w (h - dh)

  pureDecoration (GapTab gap) _ ht _ s wrs (win, r@(Rectangle x y wh _))
    | numWindows > 1 = Just (Rectangle (nx + half) y tabW (fi ht))
    | otherwise = Nothing
    where
      ws = filter (`elem` map fst (filter ((== r) . snd) wrs)) (W.integrate s)
      numWindows = length ws
      loc k h i = k + fi ((h * fi i) `div` max 1 (fi (length ws)))
      esize k h = fi $ maybe k (\i -> loc k h (i + 1) - loc k h i) (win `elemIndex` ws)
      slotW = esize x wh
      nx = maybe x (loc x wh) (win `elemIndex` ws)
      half = fi (gap `div` 2)
      tabW = if slotW > gap then slotW - gap else 1

-- Fires a dunst popup when a window signals urgency; the framework dedupes per
-- window, so it is safe to leave unconditional.
data NotifyUrgencyHook = NotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook NotifyUrgencyHook where
  urgencyHook NotifyUrgencyHook w = do
    name <- getName w
    wsTag <- W.findTag w <$> gets windowset
    let body = maybe (show name) (\t -> "[" ++ t ++ "] " ++ show name) wsTag
    safeSpawn notifySend ["-a", "xmonad", "Urgent window", body]

mkPolybarLogHook :: (String -> IO ()) -> X ()
mkPolybarLogHook output = do
  windowCount <- withWindowSet (pure . length . W.index)
  minimizedCount <- withMinimized (pure . length)
  currentTag <- gets (W.currentTag . windowset)
  dynamicLogWithPP $
    filterOutWsPP [scratchpadWorkspaceTag] $
      def
        { ppSep = color colorBase03 " | ",
          ppTitle = shorten 80,
          ppCurrent = id,
          ppVisible = const "",
          ppHidden = const "",
          ppHiddenNoWindows = const "",
          ppUrgent = \t -> if t == currentTag then t else "",
          ppLayout =
            let windowCountLabel =
                  unwords $
                    catMaybes
                      [ show <$> positive windowCount,
                        color colorBase04 . wrap "(" ")" . show <$> positive minimizedCount
                      ]
             in (++ " " ++ windowCountLabel),
          ppOutput = output
        }
  where
    positive n = if n > 0 then Just n else Nothing
    color c = wrap ("%{F" ++ c ++ "}") "%{F-}"

setupLogOutput :: IO (String -> IO ())
setupLogOutput = do
  dbus <- D.connectSession

  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  let logOutput str = do
        let signal = (D.signal objectPath interfaceName memberName) {D.signalBody = [D.toVariant str]}
        D.emit dbus signal
        where
          objectPath = D.objectPath_ "/org/xmonad/Log"
          interfaceName = D.interfaceName_ "org.xmonad.Log"
          memberName = D.memberName_ "Update"

  pure logOutput

myManageHook =
  composeAll
    [ manageDocks,
      namedScratchpadManageHook scratchpads,
      manageZoomHook
    ]

myHandleEventHook =
  fixSteamFlicker
    <+> onXPropertyChange "WM_NAME" manageZoomHook
    <+> handleEventHook def

-- https://www.peterstuart.org/posts/2021-09-06-xmonad-zoom/
manageZoomHook =
  composeAll
    [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat,
      (className =? zoomClassName) <&&> shouldSink <$> title --> doSink
    ]
  where
    zoomClassName = "zoom"
    tileTitles =
      [ "Zoom - Free Account", -- main window
        "Zoom - Licensed Account", -- main window
        "Zoom", -- meeting window on creation
        "Zoom Meeting" -- meeting window shortly after creation
      ]
    shouldFloat title = title `notElem` tileTitles
    shouldSink title = title `elem` tileTitles
    doSink = (ask >>= doF . W.sink) <+> doF W.swapDown

emConfig =
  def
    { EM.emFont = "xft:" ++ fontMono ++ ":bold:size=18:antialias=true",
      EM.bgCol = colorBase06,
      EM.borderCol = colorBase06,
      EM.borderPx = 1,
      EM.cancelKey = xK_Escape,
      EM.overlayF = EM.textSize,
      EM.txtCol = colorBase00
    }

easyFocus :: X ()
easyFocus = do
  win <- selectWindow emConfig
  whenJust win (windows . W.focusWindow)

-- https://www.reddit.com/r/xmonad/comments/rnpron/comment/hptqkh6
easySwap :: X ()
easySwap = do
  win <- selectWindow emConfig
  stack <- gets $ W.index . windowset
  let match = find ((win ==) . Just . fst) $ zip stack [0 ..]
  whenJust match $ swapNth . snd

-- Spawn a command parameterised by the current workspace's project path.
-- Falls back to `fallback` when the workspace name does not resolve to a
-- path (no namespace match, or the path does not exist on disk).
spawnInProject ::
  (FilePath -> String) -> -- command builder given project path
  String -> -- fallback command
  X ()
spawnInProject withCwd fallback = do
  ws <- gets (W.currentTag . windowset)
  mp <- io (resolveWorkspace ws)
  spawn (maybe fallback withCwd mp)

-- Quote a path for safe inclusion in a shell command line.
shellQuote :: String -> String
shellQuote s = "'" ++ concatMap escape s ++ "'"
  where
    escape '\'' = "'\\''"
    escape c = [c]

-- Run rofi -dmenu with the given candidates on stdin. Returns the user's
-- selection (matched candidate OR free-form input), or Nothing on cancel.
--
-- Uses XMonad.Util.Run.runProcessWithInput rather than
-- System.Process.readProcessWithExitCode: the latter calls waitForProcess,
-- which loses races against the Haskell RTS's SIGCHLD handler inside the
-- xmonad event loop and silently returns the wrong exit/output state.
runRofi :: [String] -> X (Maybe String)
runRofi candidates = do
  out <-
    runProcessWithInput
      "rofi"
      ["-dmenu", "-i", "-p", "workspace", "-format", "s"]
      (unlines candidates)
  pure $ case trim out of
    "" -> Nothing
    s -> Just s
  where
    trim = reverse . dropWhile (== '\n') . reverse

data Namespace = Namespace
  { nsPrefix :: String,
    -- | Absolute root directory for this namespace (e.g. ~/projects). Used by
    -- attribution to map an existing tag back to its path by string only.
    nsRoot :: FilePath,
    -- | Resolve a key (no prefix) to an absolute path. Nothing if the path
    -- does not exist on disk.
    nsResolve :: String -> IO (Maybe FilePath),
    -- | All picker candidates for this namespace (keys only, no prefix).
    nsCandidates :: IO [String]
  }

defaultMarkers :: [FilePath]
defaultMarkers = [".git", ".jj", "flake.nix"]

defaultSkip :: Set.Set FilePath
defaultSkip = Set.fromList ["node_modules", "result", "target", "dist", ".cache"]

-- A filesystem-rooted namespace.
-- depth: how many levels deep to walk when enumerating candidates.
-- markers: filenames that, when present in a directory, stop further descent
--          into that directory (the directory itself is still listed).
mkPathNamespace ::
  String -> -- prefix (without colon)
  FilePath -> -- root
  Int -> -- depth
  [FilePath] -> -- markers
  Namespace
mkPathNamespace prefix root depth markers =
  Namespace
    { nsPrefix = prefix,
      nsRoot = root,
      nsResolve = resolve,
      nsCandidates = enumerate
    }
  where
    resolve "" = pure Nothing
    resolve key =
      let path = root </> key
       in do
            ok <- safeIO False (doesDirectoryExist path)
            pure $ if ok then Just path else Nothing

    enumerate = do
      ok <- safeIO False (doesDirectoryExist root)
      if ok then sort <$> walk "" depth root else pure []

    -- Walk returns keys (relative paths) under `dir`. `relKey` is the path
    -- accumulated so far (empty at the root).
    walk relKey 0 _ = pure [relKey | not (null relKey)]
    walk relKey remaining dir = do
      entries <- safeIO [] (listDirectory dir)
      let kept =
            [ e
            | e <- entries,
              not ("." `isPrefixOf` e),
              not (e `Set.member` defaultSkip)
            ]
      hasMarker <- anyM (\m -> safeIO False (doesPathExist (dir </> m))) markers
      let self = [relKey | not (null relKey)]
      if hasMarker
        then pure self
        else do
          children <-
            fmap concat . forM kept $ \e -> do
              let sub = dir </> e
                  relKey' = if null relKey then e else relKey </> e
              isDir <- safeIO False (doesDirectoryExist sub)
              if isDir then walk relKey' (remaining - 1) sub else pure []
          pure (self ++ children)

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM p (x : xs) = do
  b <- p x
  if b then pure True else anyM p xs

-- Run an IO action, returning `fallback` if it raises an IOException. Used to
-- keep the filesystem walk robust against per-directory failures like
-- permission-denied: one unreadable subdirectory should not break the whole
-- picker.
safeIO :: a -> IO a -> IO a
safeIO fallback = handle (\(_ :: IOException) -> pure fallback)

-- Constructed on each picker invocation. The home-directory lookup and record
-- allocation are cheap; the filesystem walk only happens lazily inside
-- `nsCandidates`.
namespaces :: IO [Namespace]
namespaces = do
  home <- getHomeDirectory
  pure
    [ mkPathNamespace "p" (home </> "projects") 3 defaultMarkers
    ]

-- Look up a namespace by its prefix.
findNamespace :: String -> [Namespace] -> Maybe Namespace
findNamespace pfx = find ((== pfx) . nsPrefix)

-- Split a workspace ID into (prefix, key). "p:foo/bar" -> Just ("p", "foo/bar").
-- A name with no colon, or with an unknown prefix, is treated as context-only.
splitWorkspace :: WorkspaceId -> Maybe (String, String)
splitWorkspace name = case break (== ':') name of
  (pfx, ':' : rest) -> Just (pfx, rest)
  _ -> Nothing

resolveWorkspace :: WorkspaceId -> IO (Maybe FilePath)
resolveWorkspace name = do
  nss <- namespaces
  case splitWorkspace name of
    Just (pfx, key) -> case findNamespace pfx nss of
      Just ns -> nsResolve ns key
      Nothing -> pure Nothing
    Nothing -> pure Nothing

enumerateAllCandidates :: IO [String]
enumerateAllCandidates = do
  nss <- namespaces
  concat <$> traverse (\ns -> map ((nsPrefix ns ++ ":") ++) <$> nsCandidates ns) nss

-- All workspace tags currently in the set, minus the named-scratchpad workspace.
existingTags :: X [String]
existingTags = gets $ filter (/= scratchpadWorkspaceTag) . map W.tag . W.workspaces . windowset

-- Workspaces that are currently focused, visible on another screen, or have
-- windows. Excludes the named-scratchpad workspace. Use this for "switch
-- among open" pickers where the project pool would be noise.
openTags :: X [String]
openTags = gets (compute . windowset)
  where
    compute ws =
      let displayed = W.currentTag ws : map (W.tag . W.workspace) (W.visible ws)
       in [ W.tag w
          | w <- W.workspaces ws,
            W.tag w /= scratchpadWorkspaceTag,
            W.tag w `elem` displayed || isJust (W.stack w)
          ]

-- All picker candidates: existing workspaces plus enumerated namespace entries.
allCandidates :: X [String]
allCandidates = do
  existing <- existingTags
  enumerated <- io enumerateAllCandidates
  pure $ Set.toList $ Set.fromList (existing ++ enumerated)

-- Toggle to the most recently focused workspace that isn't the current one
-- or NSP. Built on the WorkspaceHistory hook (an actual focus log) +
-- greedyView, so it tracks focus correctly across screens. The library
-- toggles instead derive recency from the windowset, which misses cross-screen
-- focus: CycleWS.toggleWS' only inspects W.hidden (never the workspace visible
-- on the other screen, and empty when N workspaces fill N screens), and
-- CycleRecentWS.toggleRecentWS reconstructs order heuristically via unView.
-- history's head is the current workspace, hence the t /= current filter.
historyToggle :: X ()
historyToggle = do
  hist <- workspaceHistory
  current <- gets (W.currentTag . windowset)
  let prev = find (\t -> t /= current && t /= scratchpadWorkspaceTag) hist
  whenJust prev (windows . W.greedyView)

-- Jump to the next off-screen urgent window and focus it. Acting makes the
-- target visible, so repeated presses cycle through everything urgent.
focusAttention :: X ()
focusAttention = do
  ws <- gets windowset
  let visibleTags = W.currentTag ws : map (W.tag . W.workspace) (W.visible ws)
  urgents <- readUrgents
  let urgentByTag = [(t, w) | w <- urgents, Just t <- [W.findTag w ws]]
      targets = filter (`notElem` visibleTags) . nub $ map fst urgentByTag
  case targets of
    [] -> pure ()
    (t : _) -> whenJust (lookup t urgentByTag) (windows . W.focusWindow)

-- View or create-then-view a workspace, picked from the full candidate set
-- (existing + all namespace entries). Bound to M-n.
projectPrompt :: X ()
projectPrompt = do
  candidates <- allCandidates
  result <- runRofi candidates
  whenJust result $ \name -> removeEmptyWorkspaceAfter (addWorkspace name)

-- Switch to an already-open workspace, picked from open ones only. Free-form
-- input that doesn't match an open workspace silently no-ops (greedyView on a
-- non-existent tag is a no-op). Bound to M-i.
openPrompt :: X ()
openPrompt = do
  candidates <- openTags
  result <- runRofi candidates
  whenJust result $ \name -> removeEmptyWorkspaceAfter (windows (W.greedyView name))

-- Send the focused window to an already-open workspace, picked from open ones
-- only. The destination already exists, so (unlike shiftPrompt) there is no
-- workspace to create. Bound to M-S-i, the send-pair of M-i (openPrompt).
shiftToOpenPrompt :: X ()
shiftToOpenPrompt = do
  candidates <- openTags
  result <- runRofi candidates
  whenJust result $ \name -> windows (W.shift name)

-- Rename the current workspace to a name picked from the namespace candidates.
-- Used to associate a workspace (e.g. one of the numerically-named startup
-- workspaces) with a project so workspace-aware actions resolve the right
-- cwd. Free-form input is allowed; no-op if the target name already names a
-- different workspace. Bound to M-C-n, grouping it with the other
-- project-name pickers (M-n go, M-S-n send) since it shares their candidate
-- set.
renamePrompt :: X ()
renamePrompt = do
  existing <- existingTags
  enumerated <- io enumerateAllCandidates
  current <- gets (W.currentTag . windowset)
  result <- runRofi (filter (`notElem` existing) enumerated)
  whenJust result $ \name ->
    when (name == current || name `notElem` existing) $
      renameWorkspaceByName name

-- Shift the focused window to a workspace, picked from the full candidate set.
-- Creates the destination workspace if it doesn't exist (added hidden so focus
-- doesn't follow the window). Bound to M-S-n.
shiftPrompt :: X ()
shiftPrompt = do
  candidates <- allCandidates
  result <- runRofi candidates
  whenJust result $ \name -> do
    addHiddenWorkspace name
    windows (W.shift name)

scratchpads =
  [ NS
      "pavucontrol"
      "pavucontrol"
      (className =? "pavucontrol")
      scratchpadFloat,
    NS
      "spotify"
      "spotify"
      (className =? "Spotify")
      scratchpadFloat,
    NS
      "emacs-org-capture"
      "emacsclient -c -n -F '((name . \"org-capture-dedicated\"))' -e '(org-capture nil \"t\")'"
      (title =? "org-capture-dedicated")
      scratchpadFloat,
    NS
      "emacs-org-agenda"
      "emacsclient -c -n -F '((name . \"org-agenda-dedicated\"))' -e '(let ((org-agenda-window-setup (quote current-frame))) (org-agenda nil \"n\"))'"
      (title =? "org-agenda-dedicated")
      scratchpadFloat,
    NS
      "emacs-calc"
      "emacsclient -c -n -F '((name . \"full-calc-dedicated\"))' -e '(full-calc)'"
      (title =? "full-calc-dedicated")
      scratchpadFloat,
    -- ghostty's `class` must be a valid GTK app-id, so tag the scratchpad with
    -- the free-form X11 instance name instead (matched via appName).
    NS
      "btop"
      "ghostty --x11-instance-name=btop-scratchpad -e btop"
      (appName =? "btop-scratchpad")
      scratchpadFloat
  ]
  where
    scratchpadFloat = customFloating $ W.RationalRect (3 / 10) (1 / 5) (2 / 5) (3 / 5)
