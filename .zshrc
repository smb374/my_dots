# ~/.zshrc
# source <(antibody init)
# antibody bundle < ~/.zsh_plugins.txt
# source ~/.profile
source ~/.zsh_plugins.sh
# source /etc/profile.d/plan9.sh
# source /home/thomas/.ghcup/env

alias ..='cd ..'
alias anboxbr='sudo nmcli con add type bridge ifname anbox0 -- connection.id anbox-net ipv4.method shared ipv4.addresses 192.168.250.1/24'
alias aria2rpc='aria2c --conf-path=/home/thomas/.aria2/config/aria2.conf -D'
alias ccvpn='sudo openfortivpn 140.113.235.174:443 -u poyehchen --trusted-cert 2980c8c04d4a8648f8ffec2edb220af6dead56ae5918be9ff9806811fccf9171'
alias diskspeed='fio --name TEST --eta-newline=5s --filename=fio-tempfile.dat --rw=rw --size=500m --io_size=10g --blocksize=2048k --ioengine=libaio --fsync=10000 --iodepth=32 --numjobs=1 --runtime=60 --group_reporting'
alias dmesg='dmesg --color=always | less'
alias dre='cd $HOME/dwm/ && make clean && make && cd -'
alias emacs='emacs -nw '
alias fbterm='FBTERM=1 exec fcitx-fbterm-helper -l'
alias fl2ogg='find . -name "*flac" -exec oggenc -q 10 -b 485 {} \; && find . -iname "*.flac" -delete'
alias freeup='sudo zsh -c "echo 3 > /proc/sys/vm/drop_caches"'
alias g='wget -O cover.jpg '
alias ghc='stack exec -- ghc '
alias ghci='stack exec -- ghci '
alias grep='grep --color=auto'
alias jwine='LC_ALL=ja_JP.UTF-8 wine '
alias l='ls -lh'
alias ll='ls -hal'
alias ls='ls --color=always'
alias miku='LC_ALL=ja_JP.UTF-8 WINEPREFIX=/home/thomas/.mikumiku WINEARCH=win32 wine /home/thomas/Downloads/MikuMikuDanceE_v931/MikuMikuDance.exe'
alias newmenu='mmaker -vf -t Xterm OpenBox3'
alias pacman='pacman --color auto'
alias pfetch="gallery-dl -u supermariobros374@gmail.com -p thomas374 https://www.pixiv.net/bookmark.php"
alias ptt='ssh bbsu@ptt.cc'
alias rmbs='find -L . -name . -o -type d -prune -o -type l -exec rm {} +'
alias rs='screen -r'
alias sudo='sudo '
alias Syu='yay -Syu'
alias S='yay -S '
alias Ss='yay -Ss '
alias tat='tmux a -t '
alias termite='termite --class termite --name termite'
alias tnew='tmux new -s'
alias tnvim='nvim -u ~/.config/nvim/tty.vim'
alias updategrub='sudo sh -c "ZPOOL_VDEV_NAME_PATH=1 grub-mkconfig -o /boot/grub/grub.cfg"'
alias zlsnap='zfs list -t snapshot'
alias zroll='sudo zfs rollback'
alias zsnap='sudo zfs snapshot zroot/ROOT/default@$(date +%Y%m%d%H%M) ; sudo zfs snapshot zroot/data/usr@$(date +%Y%m%d%H%M) ; sudo zfs snapshot zroot/data/home@$(date +%Y%m%d%H%M) ; sudo zfs snapshot zroot/data/opt@$(date +%Y%m%d%H%M)'

export ANDROID_NDK_PATH=/home/thomas/android-ndk-r16
export ANDROID_SDK_PATH=/home/thomas/android-sdk
export WINEPREFIX="/home/thomas/.local/share/wineprefixes/osu/"
export DSSI_PATH="/usr/lib/dssi:/usr/local/lib/dssi:~/.dssi:"
export EDITOR="vim"
export INPUTRC=/home/thomas/.inputrc
export JAVA_HOME="/home/thomas/jdk1.8.0_181"
export LADSPA_PATH="/usr/lib/ladspa"
export LC_CTYPE="zh_TW.UTF-8"
export LD_LIBRARY_PATH="/usr/lib64/nvidia/xorg/:/usr/lib32/nvidia/xorg/:/usr/lib64/nvidia/:/usr/lib32/nvidia:/usr/lib:"
export LESS='-R '
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LV2_PATH="/usr/lib/lv2:/usr/local/lib/lv2:~/.lv2"
export LXVST_PATH="/usr/lib/lxvst:/usr/local/lib/lxvst:~/.lxvst"
# export MPD_HOST=/home/thomas/.config/mpd/socket
export MPD_HOST=127.0.0.1
export MPD_PORT=6600
export PROMPT_COMMAND='echo -ne "\033]2;$(PWD/#$(HOME)/\~)\007"'
export PATH="$HOME/.cargo/bin:/home/thomas/.local/bin:$PATH:$HOME/bin/:/usr/bin/core_perl/:/usr/local/bin:/home/thomas/.vim/bin/:/home/thomas/.gem/ruby/2.5.0/bin:/home/thomas/.gem/ruby/2.6.0/bin:/home/thomas/eclipse_bin/photon/eclipse:$JAVA_HOME/bin"
export RUST_SRC_PATH="$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
export VBOX_USB="usbfs"
export VISUAL="vim"
export VST_PATH="/usr/lib/vst:/usr/local/lib/vst:~/.vst"
eval $(thefuck --alias)
eval $(dircolors -p | perl -pe 's/^((CAP|S[ET]|O[TR]|M|E)\w+).*/$1 00/' | dircolors -)
export LS_COLORS

# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -g -A key
autoload zkbd
key[Home]="$terminfo[khome]"
key[End]="$terminfo[kend]"
key[Insert]="$terminfo[kich1]"
key[Backspace]="$terminfo[kbs]"
key[Delete]="$terminfo[kdch1]"
key[Up]="$terminfo[kcuu1]"
key[Down]="$terminfo[kcud1]"
key[Left]="$terminfo[kcub1]"
key[Right]="$terminfo[kcuf1]"
key[PageUp]="$terminfo[kpp]"
key[PageDown]="$terminfo[knp]"
# setup key accordingly
# [[ -n "$key[Home]" ]]       && bindkey "$key[Home]"         beginning-of-line
# [[ -n "$key[End]" ]]        && bindkey "$key[End]"          end-of-line
# [[ -n "$key[Insert]" ]]     && bindkey "$key[Insert]"       overwrite-mode
# [[ -n "$key[Backspace]" ]]  && bindkey "$key[Backspace]"    backward-delete-char
# [[ -n "$key[Delete]" ]]     && bindkey "$key[Delete]"       delete-char
# [[ -n "$key[Up]" ]]         && bindkey "$key[Up]"           history-substring-search-up
# [[ -n "$key[Down]" ]]       && bindkey "$key[Down]"         history-substring-search-down
# [[ -n "$key[Left]" ]]       && bindkey "$key[Left]"         backward-char
# [[ -n "$key[Right]" ]]      && bindkey "$key[Right]"        forward-char
bindkey "$key[Home]"         beginning-of-line
bindkey "$key[End]"          end-of-line
bindkey "$key[Insert]"       overwrite-mode
bindkey "$key[Backspace]"    backward-delete-char
bindkey "$key[Delete]"       delete-char
bindkey "$key[Up]"           history-substring-search-up
bindkey "$key[Down]"         history-substring-search-down
bindkey "$key[Left]"         backward-char
bindkey "$key[Right]"        forward-char
# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init () {
        echoti smkx
    }
    function zle-line-finish () {
        echoti rmkx
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi
bindkey -v
echo '' > /tmp/album
HISTFILE=/home/thomas/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

setopt APPEND_HISTORY
setopt BANG_HIST
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt HIST_VERIFY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt SHARE_HISTORY

zstyle ':completion:*' completer _oldlist _expand _complete _match _ignored _approximate
zstyle ':completion:*' completions 0
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' glob 0
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list '+m:{a-z}={A-Z} r:|[._-]=** r:|=**' '' '' '+m:{a-z}={A-Z} r:|[._-]=** r:|=**'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' max-errors 1 numeric
zstyle ':completion:*' menu select=2
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' substitute 0
zstyle ":completion:*:commands" rehash 1
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS'
zstyle ':completion:*:(rm|kill|diff):*' ignore-line yes
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=36=31"
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:cd:*' ignored-patterns '(*/)#lost+found' '(*/)#CVS'
zstyle ':completion:*:descriptions' format '%U%F{yellow}%d%f%u'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:processes' command 'ps -au$USER'

autoload -Uz compinit
autoload -U complist
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^E' edit-command-line                   # Opens Vim to edit current command line
bindkey '^R' history-incremental-search-backward # Perform backward search in command line history
bindkey '^S' history-incremental-search-forward  # Perform forward search in command line history
bindkey '^P' history-search-backward             # Go back/search in history (autocomplete)
bindkey '^N' history-search-forward              # Go forward/search in history (autocomplete)
[[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]] && compinit || compinit -C

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
