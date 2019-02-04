if &shell =~# 'fish$'
    set shell=sh
endif
set encoding=UTF-8 " Set utf-8 encoding
scriptencoding UTF-8
let g:color='badwolf'
let g:fancy_font = 1 " disable this if your terminal or font doesn't support fancy utf8 fonts.
let g:tty = 0 " set if you're in a tty . useful when using tmux under tty.
if ($TERM ==# 'linux')
    let g:fancy_font=0
    let g:color='jellybeans'
elseif g:tty
    let g:fancy_font=0
    let g:color='jellybeans'
endif
" set nocompatible
filetype plugin indent on " Enable filetype
let mapleader=';' " Change the mapleader
let maplocalleader='\' " Change the maplocalleader
set timeoutlen=500 " Time to wait for a command
syntax on
filetype plugin indent on
set wrap
set ruler
set incsearch
set showcmd
set number
set relativenumber
set smarttab
set autoindent
set cindent
set fileencoding=utf-8
set ambiwidth=single
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
augroup END
" Fast edit the .vimrc file using ;x
nnoremap <leader>x :e $MYVIMRC<cr>
nnoremap <leader>tx :tabedit $MYVIMRC<cr>

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
    augroup END
endif
" Folding
set foldlevelstart=0 " Start with all folds closed
set foldcolumn=1 " Set fold column

" Space to toggle and create folds.
nnoremap <silent> <Space> @=(foldlevel('.') ? 'za' : '\<Space>')<CR>
vnoremap <Space> zf

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
    let prefix='.config/nvim'
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
augroup END
augroup cursor_line
    " Only have cursorline in current window and in normal window
    autocmd WinLeave * set nocursorline
    autocmd WinEnter * set cursorline
    autocmd InsertEnter * set nocursorline
    autocmd InsertLeave * set cursorline
augroup END
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
nnoremap <leader>rt :retab<cr>
" Make j and k work the way you expect
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
" Navigation between windows
nnoremap <leader>j <C-W>j
nnoremap <leader>k <C-W>k
nnoremap <leader>h <C-W>h
nnoremap <leader>l <C-W>l
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-H> <C-W>h
nnoremap <C-L> <C-W>l
" Split controls (Emacs style:))
nnoremap <leader>x1 :only<cr>
nnoremap <leader>x2 :split<cr>
nnoremap <leader>x3 :vsplit<cr>
nnoremap <leader>xr <C-W>R
nnoremap <leader>xt <C-W>T
nnoremap <leader>xo <C-W>o
" Same when jumping around
nnoremap g; g;zz
nnoremap g, g,zz
" Reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv
" Repeat last substitution, including flags, with &.
nnoremap & :&&<CR>
xnoremap & :&&<CR>
" Keep the cursor in place while joining lines
nnoremap J mzJ`z
" Select entire buffer
nnoremap vaa ggvGg_
" Strip all trailing whitespace in the current file
nnoremap <leader>q :%s/\s\+$//<cr>:let @/=''<cr>
" Tab stuffs
nnoremap <C-Left> :tabprevious<cr>
nnoremap <C-Right> :tabnext<cr>
nnoremap <silent> <A-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<cr>
nnoremap <silent> <A-Right> :execute 'silent! tabmove ' . (tabpagenr()+1)<cr>
let notabs = 0
nnoremap <silent> <F8> :let notabs=!notabs<Bar>:if notabs<Bar>:tabo<Bar>:else<Bar>:tab ball<Bar>:tabn<Bar>:endif<cr>
"" buffer keys
nnoremap <C-M-Right> :bn<cr>
nnoremap <C-M-Left> :bp<cr>
nnoremap <leader>cb :CloseHiddenBuffers<cr>
" nnoremap mc :nohlsearch<cr>
nnoremap <Leader>hl :set hlsearch!<CR>
"" location window
nnoremap <leader>ol :lopen<cr>
nnoremap <leader>xl :lclose<cr>
"" search related
nnoremap / /\v
vnoremap / /\v
cnoremap s/ s/\v
nnoremap ? ?\v
vnoremap ? ?\v
cnoremap s? s?\v
"" terminal
nnoremap <c-x>t :tabedit<cr>:terminal<cr>
tnoremap <Esc> <C-\><C-n>
" vim-plug
call plug#begin('~/.config/nvim/plugged')
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'kristijanhusak/vim-hybrid-material'
    Plug 'w0ng/vim-hybrid'
    Plug 'mbbill/undotree'
    Plug 'wincent/command-t'
    Plug 'thaerkh/vim-workspace'
    Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
    Plug 'xuyuanp/nerdtree-git-plugin', { 'on': 'NERDTreeToggle' }
    Plug 'scrooloose/nerdcommenter'
    " Plug 'Valloric/YouCompleteMe'
    " Plug 'scrooloose/syntastic' uncomment this line to use syntastic.
    Plug 'mhinz/vim-startify'
    Plug 'shougo/neocomplete.vim'
    Plug 'Raimondi/delimitMate'
    Plug 'honza/vim-snippets' " Snippets
    Plug 'sirver/ultisnips' " Snippet engine
    Plug 'xuhdev/SingleCompile'
    if g:fancy_font
        Plug 'ryanoasis/vim-devicons'
    endif
    Plug 'SpaceVim/SpaceVim-theme'
    Plug 'w0rp/ale'
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'Shougo/neco-syntax'
    Plug 'edkolev/tmuxline.vim'
    Plug 'rakr/vim-one'
    Plug 'morhetz/gruvbox'
    Plug 'PotatoesMaster/i3-vim-syntax'
    Plug 'nanotech/jellybeans.vim'
    Plug 'dag/vim-fish'
    " Plug 'vim-ruby/vim-ruby'
    Plug 'majutsushi/tagbar'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-repeat'
    Plug 'Shougo/vimproc'
    Plug 'sjl/badwolf'
call plug#end()
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
    \ 'c' :    'CMD',
    \ 'v' :    'V',
    \ 'V' :    'VL',
    \ '':    'VB',
    \ }
let g:airline_detect_modified = 1
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#hunks#enabled = 0
"" vim-airline-themes
let g:airline_theme='onedark'
"" vim-one
" if (has('nvim'))
"   "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
"   let $NVIM_TUI_ENABLE_TRUE_COLOR=1
" endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if (has('termguicolors'))
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
elseif g:color==#'gruvbox-dark'
    let g:gruvbox_italic=1
    colorscheme gruvbox
    let g:airline_theme='gruvbox'
    set background=dark
elseif g:color==#'jellybeans'
    colorscheme jellybeans
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
nnoremap <leader>u :UndotreeToggle<cr>
"" command-t
nnoremap <leader>t :CommandT<cr>
"" vim-workspace
nnoremap <leader>s :ToggleWorkspace<cr>
let g:workspace_session_name = 'Session.vim'
"" nerdtree
nnoremap <Leader>f :NERDTreeToggle<CR>
let NERDTreeChDirMode=2
let NERDTreeShowBookmarks=1
let NERDTreeShowHidden=1
let NERDTreeShowLineNumbers=1
augroup nerd_loader
    autocmd!
    autocmd VimEnter * silent! autocmd! FileExplorer
    autocmd BufEnter,BufNew *
        \  if isdirectory(expand('<amatch>'))
        \|   call plug#load('nerdtree')
        \|   execute 'autocmd! nerd_loader'
        \| endif
augroup END
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
"" delimitMate
let delimitMate_expand_cr = 1
"" ultisnips
let g:UltiSnipsExpandTrigger='<C-K>'
let g:UltiSnipsJumpForwardTrigger='<Tab>'
let g:UltiSnipsJumpBackwardTrigger='<S-Tab>'
"" SingleCompile
nnoremap <Leader>r :SingleCompileRun<CR>
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
\   'haskell': ['hlint']
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
"" deocomplete
let g:deoplete#enable_at_startup = 1
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

