if &shell =~# 'fish$'
    set shell=sh
endif
augroup init
if empty(glob('~/.config/nvim/autoload/plug.vim'))
    silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
          \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    autocmd FileType nerdtree setlocal nolist
endif
augroup end
set encoding=UTF-8 " Set utf-8 encoding
scriptencoding UTF-8
let g:color='brogrammer'
let g:fancy_font = 1 " disable this if your terminal or font doesn't support fancy utf8 fonts.
let g:tty=0 " set if you're in a tty . useful when using tmux under tty.
if ($TERM ==# 'linux')
    let g:fancy_font=0
    let g:color='jellybeans'
elseif g:tty
    let g:fancy_font=0
    let g:color='jellybeans'
endif
" set nocompatible
let mapleader=';' " Change the mapleader
let maplocalleader='\' " Change the maplocalleader
set timeoutlen=500 " Time to wait for a command
filetype on " Enable filetype
filetype plugin indent on " Enable filetype
syntax on
set wrap
set ruler
set incsearch
set showcmd
set number
set relativenumber
set smarttab
set autoindent
set cindent
set modifiable
set fileencoding=UTF-8
set ambiwidth=single
set cmdheight=1
set updatetime=300
set shortmess+=c
set signcolumn=yes
set guifont=MesloLGSDZ\ Nerd\ Font\ Mono:h15
" tmux will only forward escape sequences to the terminal if surrounded by a DCS sequence
" http://sourceforge.net/mailarchive/forum.php?thread_name=AANLkTinkbdoZ8eNR1X2UobLTeww1jFrvfJxTMfKSq-L%2B%40mail.gmail.com&forum_name=tmux-users
if exists('$TMUX')
  let &t_SI = "\ePtmux;\e\e[5 q\e\\"
  let &t_EI = "\ePtmux;\e\e[2 q\e\\"
else
  let &t_SI = "\e[5 q"
  let &t_EI = "\e[2 q"
endif
" Source the vimrc file after saving it
augroup vimrc
    autocmd!
    autocmd BufWritePost $MYVIMRC source %
augroup end
" Fast edit the .vimrc file using ;x
nmap <Leader>x :e $MYVIMRC<cr>
nmap <Leader>tx :tabedit $MYVIMRC<cr>

let g:zipPlugin_ext = '*.zip,*.jar,*.xpi,*.ja,*.war,*.ear,*.celzip,*.oxt,*.kmz,*.wsz,*.xap,*.docx,*.docm,*.dotx,*.dotm,*.potx,*.potm,*.ppsx,*.ppsm,*.pptx,*.pptm,*.ppam,*.sldx,*.thmx,*.crtx,*.vdw,*.glox,*.gcsx,*.gqsx'

set autoread " Set autoread when a file is changed outside
set autowrite " Write on make/shell commands
set hidden " Turn on hidden"

set history=1000 " Increase the lines of history
set modeline " Turn on modeline
set completeopt+=longest " Optimize auto complete
set completeopt-=preview " Optimize auto complete

set undofile " Set undo
set showmatch
set matchtime=2 " Decrease the time to blink
set viewoptions+=slash,unix " Better Unix/Windows compatibility
set viewoptions-=options " in case of mapping change
set hlsearch
set shortmess=at
set wildmenu
set wildmode=longest,list,full
set splitbelow
set splitright
set expandtab
set tabstop=4
set shiftwidth=4
augroup broadcast
    autocmd VimEnter * silent ! set broadcast=none
    autocmd VimLeave * silent ! set broadcast=all
augroup end
if g:fancy_font
    set list " Show these tabs and spaces and so on
    set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮ " Change listchars
    set linebreak " Wrap long lines at a blank
    set showbreak=↪  " Change wrap line break
    set fillchars=diff:⣿,vert:│ " Change fillchars
    augroup trailing " Only show trailing whitespace when not in insert mode
        autocmd!
        autocmd InsertEnter * :set listchars-=trail:⌴
        autocmd InsertLeave * :set listchars+=trail:⌴
    augroup end
endif
" Folding
set foldlevelstart=0 " Start with all folds closed
set foldcolumn=1 " Set fold column

" Space to toggle and create folds.
nmap <silent> <Space> @=(foldlevel('.') ? 'za' : '\<Space>')<CR>
vmap <Space> zf

" Set foldtext
function! MyFoldText()
    let line=getline(v:foldstart)
    let nucolwidth=&foldcolumn+&number*&numberwidth
    let windowwidth=winwidth(0)-nucolwidth-3
    let foldedlinecount=v:foldend-v:foldstart+1
    let onetab=strpart('          ', 0, &tabstop)
    let line=substitute(line, '\t', onetab, 'g')
    let line=strpart(line, 0, windowwidth-2-len(foldedlinecount))
    let fillcharcount=windowwidth-len(line)-len(foldedlinecount)
    return line.'…'.repeat(' ',fillcharcount).foldedlinecount.'L'.' '
endfunction
set foldtext=MyFoldText()
" Set directories
function! InitializeDirectories()
    let parent=$HOME
    let prefix='.vim'
    let dir_list={
        \ 'backup': 'backupdir',
        \ 'view': 'viewdir',
        \ 'swap': 'directory',
        \ 'undo': 'undodir',
        \ 'cache': '',
        \ 'session': ''}
    for [dirname, settingname] in items(dir_list)
        let directory=parent.'/'.prefix.'/'.dirname.'/'
        if !isdirectory(directory)
            if exists('*mkdir')
                let dir = substitute(directory, '/$', '', '')
                call mkdir(dir, 'p')
            else
                echo 'Warning: Unable to create directory: '.directory
            endif
        endif
        if settingname!=#''
            exe 'set '.settingname.'='.directory
        endif
    endfor
endfunction
call InitializeDirectories()
augroup leave
    autocmd!
    autocmd BufWinLeave *.* silent! mkview " Make Vim save view (state) (folds, cursor, etc)
    autocmd BufWinEnter *.* silent! loadview " Make Vim load view (state) (folds, cursor, etc)
augroup end

augroup cursor_line
    " Only have cursorline in current window and in normal window
    autocmd WinLeave * set nocursorline
    autocmd WinEnter * set cursorline
    autocmd InsertEnter * set nocursorline
    autocmd InsertLeave * set cursorline
augroup end
" autocmd BufEnter * silent! lcd %:p:h
set backspace=indent,eol,start " Make backspaces delete sensibly
set whichwrap+=h,l,<,>,[,] " Backspace and cursor keys wrap to
set virtualedit=block,onemore " Allow for cursor beyond last character
" set scrolljump=5 " Lines to scroll when cursor leaves screen
set scrolloff=3 " Minimum lines to keep above and below cursor
set sidescroll=1 " Minimal number of columns to scroll horizontally
set sidescrolloff=10 " Minimal number of screen columns to keep away from cursor
"" Searching
set ignorecase " Case insensitive search
set smartcase " Case sensitive when uc present
set hlsearch " Highlight search terms
set incsearch " Find as you type search
set gdefault " turn on g flag
" Key Mappings
nmap <Leader>rt :retab<cr>
" Make j and k work the way you expect
nmap j gj
nmap k gk
vmap j gj
vmap k gk
" Navigation between windows
nmap <Leader>j <C-W>j
nmap <Leader>k <C-W>k
nmap <Leader>h <C-W>h
nmap <Leader>l <C-W>l
nmap <C-J> <C-W>j
nmap <C-K> <C-W>k
nmap <C-H> <C-W>h
nmap <C-L> <C-W>l
" Split controls (Emacs style:))
nmap <Leader>x1 :only<cr>
nmap <Leader>x2 :split<cr>
nmap <Leader>x3 :vsplit<cr>
nmap <Leader>xr <C-W>R
nmap <Leader>xt <C-W>T
nmap <Leader>xo <C-W>o
" Same when jumping around
nmap g; g;zz
nmap g, g,zz
" Reselect visual block after indent/outdent
vmap < <gv
vmap > >gv
" Repeat last substitution, including flags, with &.
nmap & :&&<CR>
xmap & :&&<CR>
" Keep the cursor in place while joining lines
nmap J mzJ`z
" Select entire buffer
nmap vaa ggvGg_
" Strip all trailing whitespace in the current file
nmap <Leader>q :%s/\s\+$//<cr>:let @/=''<cr>
" Tab stuffs
nmap <C-p> :tabprevious<cr>
nmap <C-n> :tabnext<cr>
nmap <silent> <A-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<cr>
nmap <silent> <A-Right> :execute 'silent! tabmove ' . (tabpagenr()+1)<cr>
let notabs = 0
nmap <silent> <F8> :let notabs=!notabs<Bar>:if notabs<Bar>:tabo<Bar>:else<Bar>:tab ball<Bar>:tabn<Bar>:endif<cr>
"" buffer keys
nmap <C-S-n> :bn<cr>
nmap <C-S-p> :bp<cr>
nmap <Leader>cb :CloseHiddenBuffers<cr>
" nmap mc :nohlsearch<cr>
nmap <Leader>nh :noh<CR>
"" location window
nmap <Leader>ol :lopen<cr>
nmap <Leader>xl :lclose<cr>
"" terminal
nmap <C-x>t :terminal<cr>
tmap <Esc> <C-\><C-n>
" vim-plug
call plug#begin('~/.config/nvim/plugged')
    Plug 'flazz/vim-colorschemes'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'mbbill/undotree'
    Plug 'wincent/command-t'
    Plug 'thaerkh/vim-workspace'
    Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
    Plug 'xuyuanp/nerdtree-git-plugin', { 'on': 'NERDTreeToggle' }
    Plug 'scrooloose/nerdcommenter'
    Plug 'mhinz/vim-startify'
    Plug 'Raimondi/delimitMate'
    Plug 'sirver/ultisnips'
    Plug 'honza/vim-snippets' " Snippets
    if g:fancy_font
        Plug 'ryanoasis/vim-devicons'
    endif
    Plug 'w0rp/ale'
    " Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}
    Plug 'Shougo/deoplete.nvim'
    Plug 'Shougo/neco-syntax'
    Plug 'PotatoesMaster/i3-vim-syntax', { 'for': 'i3' }
    Plug 'dag/vim-fish'
    Plug 'vim-ruby/vim-ruby'
    Plug 'majutsushi/tagbar'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-repeat'
    Plug 'Shougo/vimproc'
    Plug 'wolfgangmehner/perl-support'
    Plug 'vim-perl/vim-perl', { 'for': 'perl', 'do': 'make clean carp dancer highlight-all-pragmas moose test-more try-tiny' }
    Plug 'c9s/perlomni.vim'
    Plug 'kovisoft/slimv'
    Plug 'roxma/vim-tmux-clipboard'
    Plug 'mfulz/cscope.nvim'
    Plug 'wesleyche/Trinity'
    Plug 'lervag/vimtex', { 'for': 'tex' }
    Plug 'neovimhaskell/haskell-vim'
    Plug 'alx741/vim-hindent'
    Plug 'autozimu/LanguageClient-neovim', {
        \ 'branch': 'next',
        \ 'do': './install.sh'
        \ }
    Plug 'LnL7/vim-nix'
    Plug 'neomake/neomake'
    Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
    Plug 'ekalinin/Dockerfile.vim'
    Plug 'rust-lang/rust.vim'
    Plug 'racer-rust/vim-racer'
call plug#end()
nmap <Leader>pi :PlugInstall<CR>
" Plugin Settings
"" vim-airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline#extensions#tabline#buffer_nr_show = 1
set laststatus=2
if g:fancy_font
    let g:airline_powerline_fonts = 1
else
    let g:airline_left_sep=''
    let g:airline_right_sep=''
endif
let g:airline_mode_map = {
    \ 'n' :    'N',
    \ 'i' :    'I',
    \ 'R' :    'R',
    \ 'v' :    'V',
    \ 'V' :    'VL',
    \ '':    'VB',
    \ }
let g:airline_detect_modified = 1
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#hunks#enabled = 0
"" vim-airline-themes
if (has('termguicolors'))
    " fix bug for vim
    set termguicolors
    let g:true_color='on'
else
    let g:true_color='off'
endif
if g:color==#'onedark'
    let g:one_allow_italics = 1
    colorscheme one
    let g:airline_theme='onedark'
    " let g:airline_theme='jellybeans'
    set background=dark
elseif g:color==#'gruvbox'
    let g:gruvbox_italic=1
    colorscheme gruvbox
    let g:airline_theme='gruvbox'
    set background=dark
elseif g:color==#'jellybeans'
    colorscheme jellybeans
    let g:airline_theme='jellybeans'
    set background=dark
elseif g:color==#'brogrammer'
    colorscheme brogrammer
    let g:airline_theme='jellybeans'
    set background=dark
elseif g:color==#'hybrid-material'
    let g:enable_bold_font = 1
    let g:enable_italic_font = 1
    let g:airline_theme='hybrid'
    set background=dark
    colorscheme hybrid_material
elseif g:color==#'badwolf'
    let g:enable_bold_font = 1
    let g:enable_italic_font = 1
    let g:airline_theme='badwolf'
    set background=dark
    colorscheme badwolf
elseif g:color==#'goodwolf'
    let g:enable_bold_font = 1
    let g:enable_italic_font = 1
    let g:airline_theme='badwolf'
    set background=dark
    colorscheme goodwolf
endif
"" undotree
nmap <Leader>u :UndotreeToggle<cr>
"" command-t
nmap <Leader>t :CommandT<cr>
"" vim-workspace
nmap <Leader>s :ToggleWorkspace<cr>
let g:workspace_session_name = 'Session.vim'
"" nerdtree
let g:webdevicons_enable_nerdtree = 1
let g:WebDevIconsUnicodeDecorateFileNodesDefaultSymbol='x'
nmap <Leader>f :NERDTreeToggle<CR>
let NERDTreeChDirMode=2
let NERDTreeShowBookmarks=1
let NERDTreeShowHidden=1
let NERDTreeShowLineNumbers=1
if g:fancy_font
    let g:NERDTreeDirArrowExpandable = '▸'
    let g:NERDTreeDirArrowCollapsible = '▾'
else
    let g:NERDTreeDirArrowExpandable = '>'
    let g:NERDTreeDirArrowCollapsible = '\'
endif
" augroup nerd_loader
"     autocmd!
"     autocmd VimEnter * silent! autocmd! FileExplorer
"     autocmd BufEnter,BufNew *
"         \  if isdirectory(expand('<amatch>'))
"         \|   call plug#load('nerdtree')
"         \|   execute 'autocmd! nerd_loader'
"         \| endif
" augroup end
" NERDTrees File highlighting
function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
 exec 'autocmd FileType nerdtree highlight ' . a:extension .' ctermbg='. a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
 exec 'autocmd FileType nerdtree syn match ' . a:extension .' #^\s\+.*'. a:extension .'$#'
endfunction

call NERDTreeHighlightFile('jade', 'green', 'none', 'green', '#151515')
call NERDTreeHighlightFile('ini', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('md', 'blue', 'none', '#3366FF', '#151515')
call NERDTreeHighlightFile('yml', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('config', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('conf', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('json', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('html', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('styl', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('css', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('coffee', 'Red', 'none', 'red', '#151515')
call NERDTreeHighlightFile('js', 'Red', 'none', '#ffa500', '#151515')
call NERDTreeHighlightFile('php', 'Magenta', 'none', '#ff00ff', '#151515')
call NERDTreeHighlightFile('ds_store', 'Gray', 'none', '#686868', '#151515')
call NERDTreeHighlightFile('gitconfig', 'Gray', 'none', '#686868', '#151515')
call NERDTreeHighlightFile('gitignore', 'Gray', 'none', '#686868', '#151515')
call NERDTreeHighlightFile('bashrc', 'Gray', 'none', '#686868', '#151515')
call NERDTreeHighlightFile('bashprofile', 'Gray', 'none', '#686868', '#151515')
"" nerdtree-git-plugin
if g:fancy_font
    let g:NERDTreeIndicatorMapCustom = {
        \ 'Modified'  : '✹',
        \ 'Staged'    : '✚',
        \ 'Untracked' : '✭',
        \ 'Renamed'   : '➜',
        \ 'Unmerged'  : '═',
        \ 'Deleted'   : '✖',
        \ 'Dirty'     : '✗',
        \ 'Clean'     : '✔︎',
        \ 'Unknown'   : '?'
        \ }
else
    let g:NERDTreeIndicatorMapCustom = {
        \ 'Modified'  : 'o',
        \ 'Staged'    : '+',
        \ 'Untracked' : '*',
        \ 'Renamed'   : '->',
        \ 'Unmerged'  : '=',
        \ 'Deleted'   : 'x',
        \ 'Dirty'     : 'X',
        \ 'Clean'     : 'v',
        \ 'Unknown'   : '?'
        \ }
endif
"" vim-nerdtree-tabs
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1
" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" Set a language to use its alternate delimiters by default
let g:NERDAltDelims_java = 1
" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1
" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1
" Enable NERDCommenterToggle to check all selected lines is commented or not
let g:NERDToggleCheckAllLines = 1
"" YouCompleteMe
let g:ycm_global_ycm_extra_conf = '/home/thomas/.vim/plugged/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
let g:ycm_confirm_extra_conf = 0
let g:ycm_show_diagnostics_ui = 0
"" syntastic
let g:airline#extensions#syntastic#enabled = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_aggregate_errors=1
let g:syntastic_auto_jump=1
if g:fancy_font
    let g:syntastic_error_symbol = '✗'
    let g:syntastic_style_error_symbol = '✠'
    let g:syntastic_warning_symbol = '∆'
    let g:syntastic_style_warning_symbol = '≈'
else
    let g:syntastic_error_symbol = '>>'
    let g:syntastic_style_error_symbol = '>'
    let g:syntastic_warning_symbol = '--'
    let g:syntastic_style_warning_symbol = '-'
endif
let g:syntastic_python_checkers = ['pylint']
let g:syntastic_enable_perl_checker=1
let g:syntastic_perl_checkers = ['perl']
let g:syntastic_cpp_checkers = ['cppcheck']
let g:syntastic_c_checkers = ['cppcheck']
let g:syntastic_vim_checkers = ['vint']
"" startify
let g:startify_session_dir=$HOME . '/.vim/session'
let g:startify_custom_footer=['', '    This configuration is created by Po-Yeh Chen (aka me!) <supermariobros374@gmail.com>']
let g:startify_custom_header=[
    \'                   __   ____________ __ _                     _         ',
    \'   _________ ___  / /_ |__  /__  / // /( )_____   ____ _   __(_)___ ___ ',
    \'  / ___/ __ `__ \/ __ \ /_ <  / / // /_|// ___/  / __ \ | / / / __ `__ \',
    \' (__  ) / / / / / /_/ /__/ / / /__  __/ (__  )  / / / / |/ / / / / / / /',
    \'/____/_/ /_/ /_/_.___/____/ /_/  /_/   /____/  /_/ /_/|___/_/_/ /_/ /_/ ',
    \'']
if g:fancy_font
    function! StartifyEntryFormat()
        return 'WebDevIconsGetFileTypeSymbol(absolute_path) ." ". entry_path'
    endfunction
endif
if has('gui_running') || g:true_color==#'on'
    hi StartifyHeader  guifg=#87afff
    hi StartifyFooter  guifg=#87afff
    hi StartifyBracket guifg=#585858
    hi StartifyNumber  guifg=#ffaf5f
    hi StartifyPath    guifg=#8a8a8a
    hi StartifySlash   guifg=#585858
else
    hi StartifyHeader  ctermfg=111
    hi StartifyFooter  ctermfg=111
    hi StartifyBracket ctermfg=240
    hi StartifyNumber  ctermfg=215
    hi StartifyPath    ctermfg=245
    hi StartifySlash   ctermfg=240
endif
"" ultisnips
let g:UltiSnipsExpandTrigger='<C-K>'
let g:UltiSnipsJumpForwardTrigger='<Tab>'
let g:UltiSnipsJumpBackwardTrigger='<S-Tab>'
"" SingleCompile
nmap <Leader>r :SingleCompileRun<CR>
let g:SingleCompile_showquickfixiferror=1
"" vim-devicons
let g:webdevicons_enable = 1
let g:webdevicons_enable_nerdtree = 1
let g:webdevicons_enable_unite = 1
let g:webdevicons_enable_airline_tabline = 1
let g:webdevicons_enable_airline_statusline = 1
let g:WebDevIconsNerdTreeAfterGlyphPadding = ' '
let g:WebDevIconsNerdTreeGitPlugForceVAlign = ' '
let g:webdevicons_conceal_nerdtree_brackets = 1
"" ale
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'vim': ['vint'],
\   'python': ['autopep8','flake8'],
\   'r': ['lintr'],
\   'perl': ['perl'],
\   'c': ['cppcheck'],
\   'cpp': ['cppcheck'],
\   'fish': ['fish'],
\   'haskell': ['ghc','hlint']
\}
let g:ale_lint_on_insert_leave = 1
let g:ale_lint_on_enter = 1
let g:ale_set_highlights = 1
if g:fancy_font
    let g:ale_sign_error = '✗'
    let g:ale_sign_syntax_error = '✠'
    let g:ale_sign_warning = '∆'
    let g:ale_sign_syntax_warning = '≈'
else
    let g:ale_sign_error = '>>'
    let g:ale_sign_syntax_error = '>'
    let g:ale_sign_warning = '--'
    let g:ale_sign_syntax_warning = '-'
endif
highlight ALEErrorSign ctermbg=NONE ctermfg=red
highlight ALEWarningSign ctermbg=NONE ctermfg=yellow
let g:ale_python_flake8_executable = 'python3'
let g:ale_python_flake8_options = '-m flake8'
let g:ale_haskell_ghc_options = '-fno-code -v0 -isrc'
"" deoplete
let g:deoplete#enable_at_startup = 1
" call deoplete#custom#option('sources', {
"     \ 'haskell': ['ghc'],
"     \})
"" Tagbar
let g:tagbar_width=35
let g:tagbar_autofocus=1
nmap <F6> :TagbarToggle<CR>
"" ctrlp
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.png,*.jpg,*.jpeg,*.gif " MacOSX/Linux
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
if executable('ag')
  " Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor
  " Use ag in CtrlP for listing files.
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  " Ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif
"" tmuxline
let g:tmuxline_preset = {
\   'a'    : '#S',
\   'b'    : '#W',
\   'c'    : '#H',
\   'win'  : '#I #W',
\   'cwin' : '#I #W',
\   'x'    : '%a',
\   'y'    : '#W %R',
\   'z'    : '#H'
\}
"" slimv
let g:slimv_swank_cmd = "!ros -e '(ql:quickload :swank) (swank:create-server)' wait &"
let g:slimv_lisp = 'ros run'
let g:slimv_impl = 'sbcl'
" cscope
set cscopetag
set cscopetagorder=0

if filereadable('cscope.out')
    cs add cscope.out
elseif $CSCOPE_DB !=#''
    cs add $CSCOPE_DB
endif
set cscopeverbose

nmap zs :cs find s <C-R>=expand("<cword>")<CR><CR>
nmap zg :cs find g <C-R>=expand("<cword>")<CR><CR>
nmap zc :cs find c <C-R>=expand("<cword>")<CR><CR>
nmap zt :cs find t <C-R>=expand("<cword>")<CR><CR>
nmap ze :cs find e <C-R>=expand("<cword>")<CR><CR>
nmap zf :cs find f <C-R>=expand("<cfile>")<CR><CR>
nmap zi :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
nmap zd :cs find d <C-R>=expand("<cword>")<CR><CR>
"" SrcExpl
nmap <F8> :SrcExplToggle<CR>
" // Set the height of Source Explorer window
let g:SrcExpl_winHeight = 8
" // Set 100 ms for refreshing the Source Explorer
let g:SrcExpl_refreshTime = 100
" // Set "Enter" key to jump into the exact definition context
let g:SrcExpl_jumpKey = '<ENTER>'
" // Set "Space" key for back from the definition context
let g:SrcExpl_gobackKey = '<SPACE>'
" // In order to avoid conflicts, the Source Explorer should know what plugins "
" // except itself are using buffers. And you need add their buffer names into "
" // below listaccording to the command ':buffers!'
"
let g:SrcExpl_pluginList = [
        \ '__Tag_List__',
        \ '_NERD_tree_',
        \ 'Source_Explorer'
        \ ]
" // The color schemes used by Source Explorer. There are five color schemes
" // supported for now - Red, Cyan, Green, Yellow and Magenta. Source Explorer"
" // will pick up one of them randomly when initialization.
"
let g:SrcExpl_colorSchemeList = [
        \ 'Red',
        \ 'Cyan',
        \ 'Green',
        \ 'Yellow',
        \ 'Magenta'
        \ ]
" // Enable/Disable the local definition searching, and note that this is not  "
" // guaranteed to work, the Source Explorer doesn't check the syntax for now. "
" // It only searches for a match with the keyword according to command 'gd'
" let g:SrcExpl_searchLocalDef = 1
" // Workaround for Vim bug @https://goo.gl/TLPK4K as any plugins using autocmd for"
" // BufReadPre might have conflicts with Source Explorer. e.g. YCM, Syntastic etc."
" let g:SrcExpl_nestedAutoCmd = 1
" // Do not let the Source Explorer update the tags file when opening
" let g:SrcExpl_isUpdateTags = 0
" // Use 'Exuberant Ctags' with '--sort=foldcase -R .' or '-L cscope.files' to "
" //  create/update a tags file
" let g:SrcExpl_updateTagsCmd = 'ctags --sort=foldcase -R .'
" // Set '<F12>' key for updating the tags file artificially
" let g:SrcExpl_updateTagsKey = '<F12>'
" // Set '<F3>' key for displaying the previous definition in the jump list
let g:SrcExpl_prevDefKey = '<F3>'
" // Set '<F4>' key for displaying the next definition in the jump list
let g:SrcExpl_nextDefKey = '<F4>' "'
"" latex-live-preview
let g:livepreview_previewer = 'zathura'
"" haskell-vim
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords"
"" vim-hindent
let g:hindent_on_save = 0
let g:hindent_indent_size = 8

" Trigger configuration. Do not use <tab> if you use
" https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger='<S-tab>'
let g:UltiSnipsJumpForwardTrigger='<c-b>'
let g:UltiSnipsJumpBackwardTrigger='<c-z>'
" hie
let g:LanguageClient_serverCommands = { 'haskell': ['/home/thomas/.local/bin/hie-wrapper'] }
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
map <Leader>lb :call LanguageClient#textDocument_references()<CR>
map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>
"" racer
let g:racer_cmd = "/home/thomas/.cargo/bin/racer"
let g:racer_experimental_completer = 1
