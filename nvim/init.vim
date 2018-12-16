:autocmd!
autocmd bufwritepost ~/.config/nvim/init.vim source ~/.config/nvim/init.vim
let mapleader=','

nnoremap j gj
nnoremap k gk
nnoremap <leader>bd :Bclose<cr>
nnoremap <leader>r :Ranger<cr>
nnoremap <space> :<c-u>call SkyBison("")<cr>

nnoremap <leader>f :Files<cr>
nnoremap <leader>g :FlyGrep<cr>
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>ev :e ~/.config/nvim/init.vim<cr>

nnoremap <leader>n :bn<cr>
nnoremap <leader>N :bp<cr>

nnoremap <leader>l :ALENext<cr>
nnoremap <leader>L :ALEPrevious<cr>
" Use <tab> to cycle through deocomplete completions
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

nnoremap <leader>t :e ~/.todo<cr>
nnoremap <leader>0 :set hlsearch!<cr>

" Diff-ing
nnoremap <leader>d :windo diffthis<cr>
nnoremap <leader>D :windo diffoff<cr>

" Format JSON
nnoremap <leader>j :%!python -m json.tool<cr>

" Open VSCode
nnoremap <leader>V :!code .<cr><cr>

"nnoremap <leader>g :let mycurf=expand("<cfile>")<cr><c-w> w :execute("e ".mycurf)<cr><c-w>p

" Center cursor
set scrolloff=999

set splitright
set splitbelow

" Searching
set ignorecase
set smartcase
set incsearch

" Indentation
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab

call plug#begin('~/.vim/plugged')

Plug 'Raimondi/delimitMate'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Dependency of ranger.vim
" Allows deletion of buffer while keeping windowopen
Plug 'rbgrouleff/bclose.vim'
Plug 'francoiscabrol/ranger.vim'

" General purpose fuzzy finder
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } 
Plug 'junegunn/fzf.vim'

" Grep / Search
Plug 'wsdjeg/FlyGrep.vim'
Plug 'google/vim-searchindex'

" Completion helper (like helm)
Plug 'paradigm/SkyBison'

" Autocomplete + Tern completion engine
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'ternjs/tern_for_vim', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }

Plug 'w0rp/ale'

" Language specific plugins
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'Valloric/MatchTagAlways'
Plug 'dag/vim-fish'
Plug 'keith/swift.vim'

" Flow
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

" TypeScript
Plug 'HerringtonDarkholme/yats.vim'
Plug 'mhartington/nvim-typescript', {'do': './install.sh'}

" Themes
Plug 'kaicataldo/material.vim'

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'


" post install (yarn install | npm install) then load plugin only for editing supported files
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }

call plug#end()

" Theme
set background=dark
colorscheme material

let g:material_theme_style = 'default'
let g:material_theme_italics = 1

if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if (has("termguicolors"))
  set termguicolors
endif

" Unmap ranger keys
let g:ranger_map_keys = 0

" Use ranger instead of netrw when opening a directory
let g:ranger_replace_netrw = 1

" Start deocomplete
let g:deoplete#enable_at_startup = 1

" File path autocomplete is relative to current file
let g:deoplete#file#enable_buffer_path = 1

" Set up omnifunc sources
let g:deoplete#omni#functions = {}
let g:deoplete#omni#functions.javascript = [
  \ 'tern#Complete',
\]
set completeopt=longest,menuone
let g:deoplete#sources = {}
let g:deoplete#sources['javascript'] = ['file', 'ternjs']
let g:tern#command = ['tern']
let g:tern#arguments = ['--persistent'] 

" Turn on fuzzy finding
let g:skybison_fuzz = 1

" MatchTagAlways filetypes
let g:mta_filetypes = {
    \ 'html' : 1,
    \ 'xml' : 1,
    \ 'javascript' : 1
\}

let g:ale_linters = {
\ 'javascript': ['eslint'],
\}

let g:ale_fixers = {
\   'javascript': ['eslint'],
\}

let g:ale_fix_on_save = 0

let g:airline#extensions#tabline#enabled = 1
let g:airline_theme = 'material'

"if has("nvim")
"  " Make escape work in the Neovim terminal.
""  tnoremap <C-[> <C-\><C-n>
"
"  " Make navigation into and out of Neovim terminal splits nicer.
""  tnoremap <C-h> <C-\><C-N><C-w>h
""  tnoremap <C-j> <C-\><C-N><C-w>j
""  tnoremap <C-k> <C-\><C-N><C-w>k
""  tnoremap <C-l> <C-\><C-N><C-w>l
"
"  " Prefer Neovim terminal insert mode to normal mode.
"  "autocmd BufEnter term://* startinsert
"endif

" Introduced for flow package
let g:LanguageClient_serverCommands = {
    \ 'javascript': ['flow-language-server', '--stdio'],
    \ }

" Persistent undo
set undofile
set undodir=$HOME/.vim/undo

set undolevels=1000
set undoreload=10000
