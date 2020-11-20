# SPDX-FileCopyrightText: 2020 Justin Humm <mail@erictapen.name>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{ config, pkgs, lib, ... }: {
  systemd = {
    timers.vbb-crawler = {
      description = "VBB-Crawler";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        Unit = "vbb-crawler.service";
        OnCalendar = "*:*:0/10";
        # Start with random delay, but exact on that spot.
        RandomizedDelaySec = "10s";
        AccuracySec = "1us";
      };
    };

    services.vbb-crawler = {
      description = "VBB-Crawler";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.vbb-crawler}/bin/vbb-crawler.sh";
        WorkingDirectory = "/var/lib/vbb-crawler";
      };
    };
  };

  systemd.tmpfiles.rules = [
    "d '/var/lib/vbb-crawler' - root root - -"
  ];


}