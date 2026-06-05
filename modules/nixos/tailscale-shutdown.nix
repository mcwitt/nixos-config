{
  config,
  lib,
  ...
}:

lib.mkIf config.services.tailscale.enable {
  # Bound how long systemd waits for tailscaled to stop on shutdown/reboot.
  #
  # During a full shutdown the network is torn down concurrently with
  # tailscaled, so its graceful Close (wgengine.Close -> closing DERP
  # connections, flushing logs to log.tailscale.com) blocks on network I/O that
  # can no longer succeed ("connect: network is unreachable"). tailscaled's own
  # wgengine watchdog only force-aborts this after ~45s, which surfaces as
  # "A stop job is running for Tailscale node agent..." on every shutdown.
  #
  # The interrupted work is already failing and is not needed for a clean
  # teardown: ExecStopPost (`tailscaled --cleanup`) tears down the tun
  # interface, routes and DNS regardless of how the main process exits. So we
  # cap the stop timeout; the short grace window still lets a normal
  # `systemctl stop/restart tailscaled` (network up) exit cleanly.
  #
  # Upstream bug: https://github.com/tailscale/tailscale/issues/3671 (and #3932).
  systemd.services.tailscaled.serviceConfig.TimeoutStopSec = 5;
}
