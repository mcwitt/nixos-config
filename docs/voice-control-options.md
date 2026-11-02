# Voice control for coding agents: options

Status: deployed on satori, not yet on the Mac (2026-10-23). The client module is
[`modules/home-manager/harnesses/voice.nix`](../modules/home-manager/harnesses/voice.nix).
The server is a llama-swap entry in the private repository (see
[Setup](#setup)). satori's full system builds with both, and the satori half
passed an end-to-end test (see [Tests](#tests)). The Mac half evaluates but
has not run.

## The setup and the question

The agents (Claude Code, Codex, pi, OpenCode) run in tmux on satori. satori is
NixOS with xmonad on X11, PipeWire and an RTX 3090 (24 GB), and its
`profiles.cuda` sets `nixpkgs.config.cudaSupport = true`. Usually I reach satori
from an Apple Silicon MacBook (nix-darwin, Ghostty, mosh or ssh), so in the
usual case the microphone is on a different machine from the agent. There are
two cases:

- **Remote**: I speak on the Mac and the agent is on satori.
- **Local**: I sit at satori.

The key fact is that **any tool that types into the focused Mac app works with
every agent through Ghostty → mosh → tmux, with no change on satori.** The
satori side needs nothing for the remote case.

## Baseline: Claude Code's built-in voice mode

`modules/home-manager/harnesses/claude-code/default.nix` sets
`voiceEnabled = true` and installs `sox`. This is what that does, from the
[docs](https://code.claude.com/docs/en/voice-dictation) and a string search of
the installed 2.1.287 binary:

- **Activation**: hold Space to talk (`voice:pushToTalk`, which can be rebound
  in `keybindings.json`), or tap mode. The current schema is
  `voice = { enabled; mode = "hold" | "tap"; autoSubmit; }`. The legacy
  `voiceEnabled` key still works.
- **Capture**: on the host where `claude` runs. Linux tries the bundled
  `audio-capture.node`, then `arecord`, then SoX `rec`, at 16 kHz mono.
- **Transcription**: hosted. Audio streams over a websocket to Anthropic's
  `/api/ws/speech_to_text/voice_stream`. The protocol looks like Deepgram's,
  but I could not verify the backend. It needs a claude.ai login (Pro, Max,
  Team or Enterprise). It does not work with an API key, Bedrock or Vertex, and
  it does not count against usage limits.
- **Vocabulary**: up to 50 automatic "keyterms": a fixed list (MCP, grep,
  worktree and so on), the project name, and the git branch split into words.
  There is no user vocabulary; the requests for one were closed as duplicates
  ([#60774](https://github.com/anthropics/claude-code/issues/60774)).
- **Remote**: the docs say it "does not work in SSH sessions" and needs a local
  microphone ([#34907](https://github.com/anthropics/claude-code/issues/34907)).
  In practice, `claude` on satori over mosh records **satori's** default
  capture device (satori has an OBSBOT webcam mic). It cannot use the Mac's
  microphone. The binary has unused audio-forwarding scaffolding
  (`~/.cache/coder-audio`), but it is not documented and not wired up.

So the baseline covers **only the local case and only Claude Code**, and it
sends audio to Anthropic. Everything below is measured against that.

## Comparison

"R" means remote (Mac mic, agent on satori). "L" means local (at satori).
English WER is the average from the
[Open ASR Leaderboard](https://huggingface.co/spaces/hf-audio/open_asr_leaderboard)
(lower is better). Use it only to rank models against each other.

| Option | Nix install | Capture → transcription | Cases | Engine, speed | Compute | Offline, cost | Terminal integration | Code vocabulary | Maintenance |
|---|---|---|---|---|---|---|---|---|---|
| Claude Code voice (baseline) | already on (`claude-code` + `sox`) | agent host → Anthropic cloud | L only, Claude only | hosted streaming, live | none | hosted; in plan | built into the TUI | automatic keyterms only | none |
| macOS Dictation / Voice Control | built in | Mac → on-device (new model on M3+, macOS 27) | R | Apple; good prose | ANE | offline; free | system text input; no hold-to-talk | Dictation none; Voice Control 1000 terms | none, but no control |
| **whisper-server behind llama-swap + thin clients** (recommended) | `pkgs.whisper-cpp` 1.8.4, built from source with CUDA; llama-swap already on satori; clients in `voice.nix` | Mac mic (Hammerspoon + sox) or satori mic (pw-record) → satori GPU | **R and L**, all agents | large-v3-turbo (WER 8.4); **measured 0.13–0.18 s** for an 11 s clip, cold start 1.5 s via llama-swap | 1.6 GB weights, about 2 GB VRAM (measured) | offline (tailnet only); free | Mac: hold a chord, Cmd-V paste; satori: `prefix v` toggle, `paste-buffer -p` | `prompt` from a Nix list | about 100 lines I own; every piece declarative |
| Handy | cask `handy` 0.9.8; `pkgs.llm-agents.handy` (repackaged upstream `.deb`, not built from source) | where it runs → local | R and L | Parakeet TDT v3 (WER 6.5) or Whisper; under 1 s | CPU/ANE (Parakeet), Vulkan (Whisper) | offline; free, MIT | hold or toggle hotkey; pastes Cmd-V/Ctrl-V or types; `auto_submit` off by default | `custom_words` (fuzzy correction) | settings live in a JSON store the app owns, so not declarative |
| VoiceInk | cask `voiceink` | Mac → local, or a custom OpenAI-style endpoint | R | whisper.cpp or Parakeet | ANE/Metal | offline; $25–49 one-time, GPL-3.0 | push-to-talk; per-app Modes (no formatting, no auto-send) | personal dictionary + replacements | low; active |
| Superwhisper | cask `superwhisper` | Mac → local or cloud | R | Whisper, Parakeet, cloud | ANE/Metal | free local tier; Pro $8.49/mo | push-to-talk; per-app modes; paste with clipboard restore | vocabulary + exact replacements | low; closed source |
| Hex (anomalyco) / FluidVoice | tap `anomalyco/tap/hex` / cask `fluidvoice` | Mac → local | R | Parakeet, Whisper | ANE | offline; free, MIT / GPL-3.0 | hold Option / accessibility typing; per-app modes | corrections / UNVERIFIED | new and fast-moving |
| Wispr Flow / Aqua Voice | casks `wispr-flow`, `aqua-voice` | Mac → vendor cloud | R | proprietary; strong | none | hosted; about $8–15/mo | hold-to-talk; paste | IDE-only context features | low; cloud dependency |
| faster-whisper / Wyoming | `python3Packages.faster-whisper`; NixOS `services.wyoming.faster-whisper` (`device = "cuda"`, `sttLibrary = "onnx-asr"` for Parakeet) | satori → local GPU | L | large-v3 (7.8), distil-v3.5 (7.0) | GPU | offline; free | Wyoming TCP, not HTTP; built for Home Assistant | `initial_prompt`, `hotwords` | module exists; pin lags (3.1.0 vs 3.8.1) |
| Parakeet servers (later) | audio.cpp (not packaged; llama-swap supports it), `python3Packages.onnx-asr` (in the pin, library only), whisper.cpp ≥ 1.9.0 `parakeet-cli` (no server) | satori → local GPU | R and L via the same clients | Parakeet TDT v3 (WER 6.5) | GPU | offline; free | same endpoint, a different `model` | no prompt | needs packaging or a wrapper |
| pi-voice-stt | npm / `pi install`; not packaged | Mac mic via a reverse-SSH bridge, or the pi host → any of 9 providers | R and L, **pi only** | provider-dependent | — | provider-dependent | Ctrl+R inside pi | replacements dictionary | single maintainer, 11 stars |
| Hosted APIs (for a DIY tool or as BYOK) | — | anywhere → vendor | R and L | streaming or batch | none | see below | through the tool | `prompt` or keyterms | API churn |

Hosted prices: OpenAI `gpt-4o-mini-transcribe` $0.003/min and
`gpt-4o-transcribe` $0.006/min, both ZDR-eligible
([pricing](https://developers.openai.com/api/docs/pricing),
[data](https://developers.openai.com/api/docs/guides/your-data)). Deepgram
Nova-3 streaming is $0.0048/min, plus $0.0013/min for keyterms
([pricing](https://deepgram.com/pricing)). AssemblyAI Universal-Streaming is
$0.15/hr and has keyterms
([pricing](https://www.assemblyai.com/pricing)). Groq `whisper-large-v3-turbo`
is $0.04/hr, batch only, with ZDR open to everyone
([docs](https://console.groq.com/docs/speech-to-text)). ElevenLabs Scribe v2
Realtime is $0.39/hr, with zero retention only on Enterprise. The Gemini Live
API is about $0.005/min, and the free tier is used for training
([terms](https://ai.google.dev/gemini-api/terms)). At 15 hours of dictation a
month, any of these costs between $0.60 and $6. Cost does not matter. What
matters is that code, paths and project names leave the machine, and local
Whisper turbo or Parakeet on the 3090 is fast and accurate enough for short
prompts.

## The serious options

**whisper-server behind llama-swap, with thin clients.** This uses the
same pattern as replacing ollama with llama.cpp and llama-swap: configure the
open components separately instead of using an app that bundles them. satori
already runs llama-swap on the tailnet (`satori-ts:8080`). llama-swap routes
`/v1/audio/transcriptions` on the multipart `model` field, so one more model
entry serves `whisper-server` from nixpkgs `whisper-cpp`. That package builds
from source with CUDA because of satori's `cudaSupport`. I measured it on
satori: large-v3-turbo with Silero VAD and flash attention used 1.6 GB of
weights and about 2 GB of VRAM. It transcribed an 11 s clip in 0.13–0.18 s and
started cold in 0.43 s on its own (1.5 s through llama-swap, including the
first request). Because the server starts so fast, it does not matter
when llama-swap evicts it. What matters is that a dictation request does not
evict Qwen, and a llama-swap `matrix` set (`q & w`) prevents that. The clients
are about 100 lines in `voice.nix`:
- **Mac**: Hammerspoon (a cask) gives true hold-to-talk on a chord. It records
  with nixpkgs `sox`, posts to satori, and pastes with Cmd-V, which Ghostty
  sends through mosh and tmux as a bracketed paste.
- **satori**: `prefix v` toggles `pw-record` and pastes into the pane that
  pressed it with `tmux paste-buffer -p`. It needs no X11 or xmonad. I tested
  the paste path on a scratch tmux server: the text arrived as one line inside
  bracketed-paste markers, with no Enter.

One Nix list feeds Whisper's `prompt` on both sides. Every piece is in
nixpkgs and declarative. What I give up compared with Handy: Parakeet (see
below), a polished recording indicator (the clients use `hs.alert` and
`tmux display-message`), and hold-to-talk on satori, where the tmux key only
toggles.

**Handy** ([cjpais/Handy](https://github.com/cjpais/Handy)) is the strongest
bundled app. It is free and MIT-licensed, it runs Parakeet TDT v3 or Whisper
locally, and it can hold-to-talk or toggle. It inserts text by paste or
typing, and `auto_submit` is off by default. It runs on both machines, but it
fits NixOS and declarative configuration badly:
- The llm-agents Linux package unpacks the upstream `.deb` and patches the
  binary. It is not built from source, and it uses Vulkan, not CUDA.
- It is not in the pinned nixpkgs at all.
- All settings, including the hotkey, the model and `custom_words`, live in
  `settings_store.json`, a file the app rewrites itself
  ([`settings.rs`](https://github.com/cjpais/Handy/blob/main/src-tauri/src/settings.rs)).
  Upstream also renames keys between releases.
- On X11 it depends on xdotool, and its README warns that its overlay can
  interfere with pasting.

Its cask is still a five-minute way to compare Parakeet's accuracy on the Mac.

**VoiceInk** ([Beingpax/VoiceInk](https://github.com/Beingpax/VoiceInk))
is the best Mac-only runner-up. Its per-app Modes let Ghostty get "no
formatting, no AI cleanup, no auto-send", it has a dictionary with
replacements, and it can use a
[custom OpenAI-compatible endpoint](https://tryvoiceink.com/docs/custom-models).
That endpoint could be a `whisper-server` on satori over Tailscale, if the Mac's
Parakeet ever falls short. The cost is $25–49 for signed builds; the source is
GPL-3.0.

**Superwhisper** is the most polished commercial option. It has real
push-to-talk, per-app auto-activation, exact replacements, and paste with
clipboard restore. Its agent plugins assume that the agent runs on the Mac, so
here they add nothing. It is worth it only if Handy's lack of per-app modes
turns out to hurt.

**macOS Dictation and Voice Control** are free and on-device. The macOS 27
model on M3+ is good for prose
([MacStories](https://www.macstories.net/stories/macos-27-the-macstories-review/8/)),
but Dictation has no hold-to-talk and no vocabulary. I found no test of it on
code. Voice Control adds 1000 custom terms and commands, but it is always
listening and was built for accessibility, not for prompting.

**Parakeet, later.** The weights are open (CC-BY-4.0). The obstacle is
the server. The pinned `whisper-cpp` 1.8.4 predates Parakeet. whisper.cpp
added it in v1.9.0 ([#3735](https://github.com/ggml-org/whisper.cpp/pull/3735)),
but only as a separate `parakeet-cli`; v1.9.5's `whisper-server` cannot serve
it. Three routes fit the same clients:
- **audio.cpp** ([0xShug0/audio.cpp](https://github.com/0xShug0/audio.cpp)):
  Apache-2.0, built on ggml, CUDA and Metal. It serves Parakeet TDT v3 GGUF
  on OpenAI's `/v1/audio/transcriptions`, and llama-swap lists it as
  supported. This is the true llama.cpp analogue, but it is not packaged, and
  it is young (first release June 2026).
- **onnx-asr**: in the pin, with CUDA and TensorRT, but it is a library only,
  so it needs a wrapper of about 30 lines.
- **sherpa-onnx**: in the pin, but its server speaks WebSocket, not OpenAI
  HTTP.

Parakeet averages better (WER 6.5 against 8.4) and is faster, but it takes no
prompt. For identifiers, the Nix vocabulary prompt may matter more. Decide
by running the same saved clips through both.
`services.wyoming.faster-whisper` (Wyoming protocol, made for Home Assistant)
and WhisperX (batch alignment) are the wrong shape for this.

**pi-voice-stt** ([cgarrot/pi-voice-stt](https://github.com/cgarrot/pi-voice-stt))
is the only agent plugin I found that handles a remote microphone: a Mac
daemon, a reverse-SSH tunnel and a bearer token. It works only in pi, has one
maintainer, and does nothing that a Mac dictation app does not already do for
all four agents.

## Dismissed

- **Talon**: its author announced in May 2026 that Linux support will be
  removed from public releases
  ([OSnews](https://www.osnews.com/story/145162/)), and `talon-nix` is stuck at
  0.4.0. It is also a command grammar with a steep learning curve, and I want
  dictation.
- **nerd-dictation, Numen** (VOSK): far less accurate than Parakeet or Whisper,
  and not packaged. Only the `vosk` binding is in nixpkgs.
- **Dragonfly / Caster**: Kaldi or Natlink grammars; not packaged; the same
  "commands, not prompts" problem as Talon.
- **whisper-overlay, blurt, hyprwhspr, voxtype**: Wayland, GNOME or Hyprland
  first. voxtype treats X11 as a fallback, and its Nix builds are CPU-only.
  satori is X11 with xmonad.
- **OpenWhispr, Speech Note**: Electron or Flatpak, and not packaged.
- **Codex `/voice`**: since 0.118.0 it is a realtime spoken conversation, not
  dictation. Its push-to-talk dictation was
  [removed](https://github.com/openai/codex/releases/tag/rust-v0.118.0). It
  captures on the agent host.
- **VoiceMode MCP** ([mbailey/voicemode](https://github.com/mbailey/voicemode)):
  the agent calls a tool to talk with me turn by turn, so this is not prompt
  dictation. It could run on the Mac through `voicemode serve` and an ssh `-R`
  tunnel, which is interesting for hands-free sessions later, but not first.
- **OpenCode voice plugins** (`opencode-voice` on npm, TheAmanM/opencode-voice):
  immature and OpenCode only.
- **Wispr Flow, Aqua Voice, Willow**: cloud subscriptions whose code features
  work only in IDEs; through remote tmux they are ordinary dictation.
- **MacWhisper**: built for file transcription.
- **speaches** (faster-whisper-server): not packaged, Docker-first, and
  development has slowed (last tag 2025-12).
- **Porcupine** wake word: the free tier
  [ended on 2026-06-30](https://community.home-assistant.io/t/fyi-picovoice-confirmed-free-tier-accesskeys-will-stop-working-after-june-30-2026/1012744).

## Recommendation

**Start with whisper-server behind satori's llama-swap and the two thin
clients in `voice.nix`, for both cases.** It is the simplest setup that is
fully declarative. Everything it needs is already in the pinned nixpkgs or
already running, and one server covers every agent, both microphones and the
same vocabulary. The Mac reaches the server over the tailnet, so the Mac does
not dictate when satori is down or unreachable.

- **Mac (remote case)**: Hammerspoon, with ctrl-alt-D held to talk. Install
  the `hammerspoon` cask, not a Nix build, for the same reason as the other
  GUI apps in `marin-macbook`: a stable `/Applications` path keeps the
  microphone and accessibility grants across updates (my inference; not
  tested).
- **satori (local case)**: `prefix v` to start, `prefix v` again to
  transcribe and paste.
- **Keep Claude Code's built-in voice.** It costs nothing and its Space key
  stays inside Claude.
- **Parakeet later**: after a few days of use, run the saved clips through
  Parakeet. If it wins, add audio.cpp as a second llama-swap model and change
  `harnesses.voice.model`.
- **Fallbacks**: the Handy cask for a quick bundled comparison on the Mac;
  VoiceInk if per-app modes turn out to matter.

**Push-to-talk, not a wake word.** The agents run with
`permissions.defaultMode = "auto"`, so a false wake from conversation, a call
or a video would inject a prompt that an agent might act on. Porcupine's free
tier is gone. openWakeWord's models are CC-BY-NC-SA, its last release was in
2024, and nothing connects it to a tmux pane. A wake word on the Mac would
also mean a microphone that is always listening. Hold a chord, not a bare
key, so that it does not clash with Claude's Space binding or with typing.
Nothing in the clients sends Enter. Read the text, then submit it yourself.

## Setup

The server is in the private repository's
`hosts/satori/configuration/llama-swap.nix`. It uses weights from
`modules/whisper-weights.nix`, which pins Hugging Face revisions:

```nix
whisper = {
  cmd = builtins.concatStringsSep " " [
    (lib.getExe' pkgs.whisper-cpp "whisper-server")
    "--port \${PORT}"
    "-m ${whisper.large-v3-turbo}"
    "--vad -vm ${whisper.silero-vad}"
    "-fa"
    "--inference-path /v1/audio/transcriptions"
  ];
  # whisper-server binds IPv4 only; the default localhost tries ::1 first.
  proxy = "http://127.0.0.1:\${PORT}";
  checkEndpoint = "/"; # whisper-server has no /health
  ttl = 600;
};

# Dictation must not evict the LLM. Models in no set run alone, so an image
# model still evicts both.
settings.matrix = {
  vars = {
    q = "Qwen3.8-27B";
    w = "whisper";
  };
  sets.chat = "q & w";
};
```

The clients:
- **satori**: `home-manager.users.marin.harnesses.voice` sets
  `enable = true` and `keepClips = true`. The default endpoint,
  `127.0.0.1:8080`, is llama-swap. Clips go to
  `~/.local/state/voice/clips`.
- **marin-macbook**: `harnesses.voice` sets the endpoint to
  `http://satori-ts:8080/v1/audio/transcriptions` and turns on `keepClips`,
  which saves clips to `~/Library/Application Support/voice/clips`. The host
  also adds `"hammerspoon"` to `homebrew.casks`.

The Hammerspoon client owns `~/.hammerspoon/init.lua`. If you later add more
Hammerspoon configuration, move the client into a separate file that
`init.lua` loads.

## Tests

All tests ran on satori, without switching the system:
- **Build**: satori's `system.build.toplevel` builds from the private
  repository, with this branch as its `nixos-config` input. Compared with
  the running system, the new closure adds whisper-cpp, the two model files
  and `tmux-dictate`, and changes `tmux.conf`. The darwin system evaluates,
  and the generated `init.lua` passes a Lua syntax check.
- **llama-swap config**: the generated config differs from the live one only
  by the `whisper` model and the `matrix`.
- **End to end on satori**: a throwaway llama-swap ran the generated config
  on a spare port. A scratch tmux server loaded the real binding line.
  Sending `C-b v` to an attached client started `pw-record` on a temporary
  `pw-loopback` virtual mic. I played an 11 s speech sample into the mic,
  then sent `C-b v` again. The transcript arrived in the pane as one
  bracketed paste with no Enter. The clip and the transcript were archived,
  no recorder was left running, and the status message went to the right
  client.
- **Speed**: 162 ms per request warm, 1.5 s cold, through llama-swap. After
  the deploy, the live service gave the same: 1.5 s cold on localhost, and
  167 ms warm over the tailnet through `satori-ts:8080`, the Mac's path.
- **Matrix**: I ran it with whisper-server stand-ins for Qwen and z-image.
  Requesting whisper while Qwen ran kept both loaded. Requesting z-image
  evicted both.

## Not verified

- That Claude Code's speech backend is Deepgram (the protocol suggests it).
- How Claude Code, Codex, OpenCode and pi each handle the bracketed paste.
  tmux's side is tested.
- Recording from a real microphone. satori records from the default
  PipeWire source, currently the OBSBOT webcam mic; set
  `harnesses.voice.source` to use another one.
- The Mac client at runtime. In particular, whether `hs.task:interrupt()`
  lets `rec` finish the WAV header, whether Hammerspoon (not sox) receives
  the microphone grant, and whether ctrl-alt-D collides with anything.
- How accurate macOS 27 Dictation is on code terms.
- That a Nix-store `.app` loses TCC (privacy permission) grants on update.
