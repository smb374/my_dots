# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  nixpkgs.config.allowUnfree = true;
  # Use the systemd-boot EFI boot loader.
  # boot.loader.systemd-boot.enable = true;
  boot.loader.grub.enable = true;
  # boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "/dev/disk/by-id/ata-Hitachi_HTS545016B9A300_090602PB5B00QCGHTY7H";
  # boot.loader.efi.canTouchEfiVariables = true;


  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
  #   consoleFont = "Lat2-Terminus16";
      consoleKeyMap = "us";
      defaultLocale = "en_US.UTF-8";
      supportedLocales = [ "zh_TW.UTF-8/UTF-8" "en_US.UTF-8/UTF-8" "zh_CN.UTF-8/UTF-8" "ja_JP.UTF-8/UTF-8" ];
  };

  # Set your time zone.
  time.timeZone = "Asia/Taipei";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    (vim_configurable.override {
      python = python3;
      features = "huge";
      luaSupport = true;
      perlSupport = true;
      pythonSupport = true;
      rubySupport = true;
      nlsSupport = true;
      tclSupport = true;
      multibyteSupport = true;
      cscopeSupport = true;
      ximSupport = true;     
      ftNixSupport = true;
    })
    (neovim.override{
      withPython = true;
      withPython3 = true;
      withNodeJs = true;
      withRuby = true;
    }) 
    neovim-remote
    (python37.withPackages(ps: with ps; [ pip pynvim numpy toolz ]))
    (python27.withPackages(ps: with ps; [ pip pynvim numpy toolz ]))
    git
    p7zip
    subversion
    dos2unix
    gptfdisk
    neofetch
    firefox
    adapta-gtk-theme
    papirus-icon-theme
    conky
    feh
    mpd
    mpc_cli
    ncmpcpp
    neomutt
    weechat
    curl
    zsh
    ghc
    cabal-install
    stack
    python27
    python37
    ruby
    perl
    antibody
    dd_rescue
    ddrescue
    lz4
    lzo
    lzop
    pulseaudioFull
    pavucontrol
    mpd_clientlib
    wirelesstools
    libnl
    xlibs.libXft
    xlibs.libXau
    xlibs.libXdmcp
    xorg.xcbproto
    xorg.xcbutil
    xorg.xcbutilwm
    xorg.xcbutilimage
    xorg.libpthreadstubs
    xorg.libxcb
    (polybar.override{
      alsaSupport = true;
      mpdSupport = true;
      pulseSupport = true;
    })
    google-chrome
    tdesktop
    telegram-cli
    slack-dark
    slack-cli
    rustup
    htop
    alacritty
    lxappearance
    unrar
    xcompmgr
    compton
    nixos-icons
    tmux
    aria2
    fio
    emacs
    socat
    gnome3.zenity
    steam
    scrot
    polkit
    fcitx
    fcitx-configtool
    fcitx-engines.chewing
    fcitx-engines.mozc
    fcitx-engines.libpinyin
    fcitx-engines.m17n
    fbterm
    thefuck
    automake
    autoconf
    autogen
    autoreconfHook
    cmake
    gnumake
    pkg-config
    fontconfig
    fontconfig-penultimate
    fontconfig-ultimate
    freetype
    gcc
    (pkgs.st.overrideAttrs ( attrs:{
      conf = builtins.readFile /home/thomas/st_conf/config.h;
      patches = attrs.patches ++ [
        /home/thomas/st/st-alpha-0.8.2.diff
        /home/thomas/st/st-anysize-0.8.1.diff
        /home/thomas/st/st-scrollback-0.8.2.diff
        /home/thomas/st/st-vertcenter-20180320-6ac8c8a.diff
      ];
    }))
    i3lock-color
    imagemagick
    xorg.xdpyinfo
    xorg.xrandr
    xorg.libXrandr
    bc
    dunst
    rofi
  ];
  fonts.fonts = with pkgs; [
    nerdfonts
    google-fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    mplus-outline-fonts
    dina-font
    proggyfonts
    wqy_microhei
    wqy_zenhei
    dejavu_fonts
    liberation_ttf_v2
  ];
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    desktopManager = {
      xfce.enable = true;
      xterm.enable = true;
    };
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
        ];
      };
      default = "xmonad";
    };
    displayManager.lightdm.greeters.gtk = {
      cursorTheme.name = "Adwaita";
      iconTheme.name = "Papirus-Dark";
      theme.name = "Adaptai-Nokto";
    };
  };
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  services.mpd.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  programs.zsh.enable = true;
  users.users.thomas = {
    isNormalUser = true;
    extraGroups = [ "wheel" "video" "audio" "uucp" "dbus" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };
  

  # Configure Nix
  nix = {
    maxJobs = 4;
    allowedUsers = [ "@wheel" ];
  };
  # Configure Vim
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}
