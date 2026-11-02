{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.harnesses.voice;
  prompt = lib.concatStringsSep ", " cfg.vocabulary;
  luaList = xs: "{ ${lib.concatMapStringsSep ", " builtins.toJSON xs} }";

  # First call records; second transcribes and inserts the text without
  # submitting it. The second call's mode decides where the text goes:
  #   dictate tmux PANE CLIENT  bracketed paste into a tmux pane
  #   dictate x11               typed into the focused X11 window
  dictate = pkgs.writeShellApplication {
    name = "dictate";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.curl
      pkgs.gnused
      pkgs.libnotify
      pkgs.pipewire
      pkgs.xdotool
      config.programs.tmux.package
    ];
    text = ''
      mode=$1
      dir="''${XDG_RUNTIME_DIR:-/tmp}/dictate"
      mkdir -p "$dir"
      # The stack tag makes each message replace the previous one.
      notify() {
        notify-send -a dictate -h string:x-dunst-stack-tag:dictate \
          -t "''${2:-3000}" dictation "$1" || true
      }
      case $mode in
        tmux)
          pane=$2
          client=$3
          say() { tmux display-message -c "$client" "dictation: $1"; }
          stop_hint="prefix v"
          ;;
        x11)
          say() { notify "$@"; }
          stop_hint="super-v"
          ;;
        *)
          echo "usage: dictate tmux PANE CLIENT | dictate x11" >&2
          exit 2
          ;;
      esac

      pid=$(cat "$dir/pid" 2>/dev/null || true)
      if [[ -z "$pid" ]] || ! kill -0 "$pid" 2>/dev/null; then
        pw-record --rate 16000 --channels 1 --format s16 \
          ${lib.optionalString (cfg.source != null) "--target ${lib.escapeShellArg cfg.source}"} \
          "$dir/clip.wav" &
        echo $! >"$dir/pid"
        echo "$mode" >"$dir/mode"
        say "recording, $stop_hint to stop" 0
        exit 0
      fi

      rm -f "$dir/pid"
      # A recording started with super-v left a notification that never expires.
      if [[ $(cat "$dir/mode" 2>/dev/null || true) == x11 && $mode != x11 ]]; then
        notify "stopped" 1000
      fi
      kill -INT "$pid"
      while kill -0 "$pid" 2>/dev/null; do sleep 0.05; done

      if ! raw=$(curl -sf --max-time 30 \
        -F file=@"$dir/clip.wav" \
        -F model=${lib.escapeShellArg cfg.model} \
        -F response_format=text \
        --form-string prompt=${lib.escapeShellArg prompt} \
        ${lib.escapeShellArg cfg.endpoint}); then
        say "transcription failed"
        exit 1
      fi
      # whisper-server's text format breaks lines between segments.
      text=$(printf '%s' "$raw" | tr '\n' ' ' | sed -e 's/  */ /g' -e 's/^ //' -e 's/ $//')

      ${lib.optionalString cfg.keepClips ''
        archive="''${XDG_STATE_HOME:-$HOME/.local/state}/voice/clips"
        mkdir -p "$archive"
        stamp=$(date +%Y%m%dT%H%M%S)
        cp "$dir/clip.wav" "$archive/$stamp.wav"
        printf '%s\n' "$text" >"$archive/$stamp.txt"
      ''}

      if [[ -z "$text" ]]; then
        say "nothing transcribed"
        exit 0
      fi
      case $mode in
        tmux)
          printf '%s' "$text" | tmux load-buffer -b dictate -
          tmux paste-buffer -p -d -b dictate -t "$pane"
          ;;
        x11)
          say "$text" 2000
          # Typing rather than pasting: Ctrl-V is not paste in every app.
          xdotool type --clearmodifiers --delay 5 -- "$text"
          ;;
      esac
    '';
  };

  # Hold to record, release to transcribe and paste with Cmd-V, which Ghostty
  # sends as a bracketed paste through mosh and tmux.
  hammerspoonInit = ''
    local rec = "${pkgs.sox}/bin/rec"
    local curl = "${pkgs.curl}/bin/curl"
    local clip = hs.fs.temporaryDirectory() .. "dictate.wav"
    local archive = os.getenv("HOME") .. "/Library/Application Support/voice/clips"
    local keepClips = ${lib.boolToString cfg.keepClips}
    local recording

    local function paste(text)
      local saved = hs.pasteboard.getContents()
      hs.pasteboard.setContents(text)
      hs.eventtap.keyStroke({ "cmd" }, "v")
      hs.timer.doAfter(0.5, function()
        if saved then hs.pasteboard.setContents(saved) end
      end)
    end

    local function keep(text)
      os.execute("/bin/mkdir -p '" .. archive .. "'")
      local stamp = archive .. "/" .. os.date("%Y%m%dT%H%M%S")
      os.execute("/bin/cp '" .. clip .. "' '" .. stamp .. ".wav'")
      local f = io.open(stamp .. ".txt", "w")
      if f then
        f:write(text, "\n")
        f:close()
      end
    end

    local function transcribed(code, out)
      hs.alert.closeAll()
      if code ~= 0 then
        hs.alert.show("dictation failed")
        return
      end
      local text = out:gsub("%s+", " "):gsub("^ ", ""):gsub(" $", "")
      if keepClips then keep(text) end
      if text ~= "" then paste(text) end
    end

    hs.hotkey.bind(${luaList cfg.macHotkey.mods}, ${builtins.toJSON cfg.macHotkey.key},
      function()
        recording = hs.task.new(rec, nil, { "-q", "-r", "16000", "-c", "1", "-b", "16", clip })
        recording:start()
        hs.alert.show("● dictating", true)
      end,
      function()
        if not recording then return end
        recording:interrupt()
        recording:waitUntilExit()
        recording = nil
        hs.task.new(curl, transcribed, {
          "-sf", "--max-time", "30",
          "-F", "file=@" .. clip,
          "-F", ${builtins.toJSON "model=${cfg.model}"},
          "-F", "response_format=text",
          "--form-string", ${builtins.toJSON "prompt=${prompt}"},
          ${builtins.toJSON cfg.endpoint},
        }):start()
      end)
  '';
in
{
  options.harnesses.voice = {
    enable = lib.mkEnableOption "push-to-talk dictation into agent prompts";

    endpoint = lib.mkOption {
      type = lib.types.str;
      default = "http://127.0.0.1:8080/v1/audio/transcriptions";
      example = "http://satori:8080/v1/audio/transcriptions";
      description = "OpenAI-compatible transcription endpoint, such as llama-swap fronting whisper-server.";
    };

    model = lib.mkOption {
      type = lib.types.str;
      default = "whisper";
      description = "Model name sent with each request; llama-swap routes on it.";
    };

    vocabulary = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "Claude Code"
        "Codex"
        "OpenCode"
        "pi"
        "tmux"
        "NixOS"
        "nixpkgs"
        "nixos-rebuild"
        "home-manager"
        "nix-darwin"
        "flake.nix"
        "--flake"
        "worktree"
        "llama-swap"
      ];
      description = "Terms sent as Whisper's prompt to bias spelling.";
    };

    keepClips = lib.mkEnableOption "keeping each recording and its transcript, for comparing models later";

    source = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "alsa_input.usb-Remo_Tech_Co.__Ltd._OBSBOT_Meet_2-02.analog-stereo";
      description = "PipeWire node to record from on Linux; null uses the default source.";
    };

    package = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      default = dictate;
      description = "The `dictate` script, for binding in a window manager.";
    };

    macHotkey = {
      mods = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [
          "ctrl"
          "alt"
        ];
      };
      key = lib.mkOption {
        type = lib.types.str;
        default = "d";
      };
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      (lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
        home.packages = [ dictate ];
        programs.tmux.extraConfig = ''
          bind v run-shell -b '${lib.getExe dictate} tmux #{q:pane_id} #{q:client_name}'
        '';
      })

      # Hammerspoon itself is a Homebrew cask, for stable TCC grants.
      (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
        home.file.".hammerspoon/init.lua".text = hammerspoonInit;
      })
    ]
  );
}
