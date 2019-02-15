# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
  export ZSH="/home/thomas/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="ys"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    zsh-autosuggestions
    zsh-syntax-highlighting
    history-substring-search
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
eval `dircolors`
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
export WINEPREFIX="/home/thomas/.local/share/wineprefixes/osu/"
#source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
if [ $TILIX_ID  ] || [ $VTE_VERSION  ]; then
    source /etc/profile.d/vte.sh
fi
### Powerlevel9k settings
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir dir_writable vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status background_jobs history load time)
POWERLEVEL9K_MODE='powerline'
###
### Alien
export AM_INITIAL_LINE_FEED=2
###
alias sudo='sudo '
alias pacman='pacman --color auto'
alias ls='ls --color=always'
alias ll='ls -hal'
alias grep='grep --color=auto'
alias pfetch="gallery-dl -u foo@bar -p barz https://www.pixiv.net/bookmark.php"
alias rs='screen -r'
alias tnew='tmux new -s'
alias tat='tmux attach -t'
alias emacs='emacs -nw'
alias newmenu='mmaker -vf -t Xterm OpenBox3'
alias aria2rpc='aria2c --conf-path=/home/thomas/.aria2/config/aria2.conf -D'
alias diskspeed='fio --name TEST --eta-newline=5s --filename=fio-tempfile.dat --rw=rw --size=500m --io_size=10g --blocksize=2048k --ioengine=libaio --fsync=10000 --iodepth=32 --numjobs=1 --runtime=60 --group_reporting'
alias ptt='ssh bbsu@ptt.cc'
alias rmbs='find -L . -name . -o -type d -prune -o -type l -exec rm {} +'
alias fl2ogg='find . -name "*flac" -exec oggenc -q 10 -b 485 {} \; && find . -iname "*.flac" -exec rm {} +'
alias anboxbr='sudo nmcli con add type bridge ifname anbox0 -- connection.id anbox-net ipv4.method shared ipv4.addresses 192.168.250.1/24'
alias zsnap='sudo zfs snapshot zroot/ROOT/default@$(date +%Y%m%d%H%M) ; sudo zfs snapshot zroot/data/usr@$(date +%Y%m%d%H%M) ; sudo zfs snapshot zroot/data/home@$(date +%Y%m%d%H%M) ; sudo zfs snapshot zroot/data/opt@$(date +%Y%m%d%H%M)'
alias zlsnap='zfs list -t snapshot'
alias zroll='sudo zfs rollback'
alias tnvim='nvim -u ~/.config/nvim/tty.vim'
alias fbterm='FBTERM=1 exec fcitx-fbterm-helper -l'
alias freeup='sudo zsh -c "echo 3 > /proc/sys/vm/drop_caches"'
alias updategrub='sudo sh -c "ZPOOL_VDEV_NAME_PATH=1 grub-mkconfig > /boot/grub/grub.cfg"'
alias termite='termite --class termite --name termite'
alias jwine='LC_ALL=ja_JP.UTF-8 wine '

export VISUAL="emacs -nw"
export theme_nerd_fonts=yes
export theme_color_scheme="zenburn"
export VBOX_USB="usbfs"
source ~/.profile
export ANDROID_SDK_PATH=/home/thomas/android-sdk
export ANDROID_NDK_PATH=/home/thomas/android-ndk-r16
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS='-R '
export VST_PATH="/usr/lib/vst:/usr/local/lib/vst:~/.vst"
export LXVST_PATH="/usr/lib/lxvst:/usr/local/lib/lxvst:~/.lxvst"
export LADSPA_PATH="/usr/lib/ladspa"
export LV2_PATH="/usr/lib/lv2:/usr/local/lib/lv2:~/.lv2"
export DSSI_PATH="/usr/lib/dssi:/usr/local/lib/dssi:~/.dssi:"
#pacmd set-default-sink alsa_output.pci-0000_00_1b.0.analog-stereo
export JAVA_HOME="/home/thomas/jdk1.8.0_181"
export PATH="~/.cabal/bin:$HOME/bin/:/usr/bin/core_perl/:/usr/local/bin:/home/thomas//.local/bin/:/home/thomas/.gem/ruby/2.5.0/bin:/home/thomas/.gem/ruby/2.6.0/bin:/home/thomas/eclipse_bin/photon/eclipse:$JAVA_HOME/bin:$PATH"
export PROMPT_COMMAND='echo -ne "\033]2;$(PWD/#$(HOME)/\~)\007"'
export MPD_HOST=/home/thomas/.config/mpd/socket
export LD_LIBRARY_PATH="/usr/lib64/nvidia/xorg/:/usr/lib32/nvidia/xorg/:/usr/lib64/nvidia/:/usr/lib32/nvidia:/usr/lib:"
eval $(thefuck --alias)
alias dmesg='dmesg --color=always | less'
# (cat ~/.cache/wal/sequences &)
case "$TERM" in
    xterm*)
        if [ -e /usr/share/terminfo/x/xterm-256color ]; then
            export TERM=xterm-256color
        elif [ -e /usr/share/terminfo/x/xterm-color ]; then
            export TERM=xterm-color;
        else
            export TERM=xterm
        fi
        ;;
    linux)
        [ -n "$FBTERM" ] && export TERM=fbterm
        ;;
esac

export MPD_HOST=$HOME/.config/mpd/socket
bindkey -e
echo '' > /tmp/album
