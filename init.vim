:autocmd!
autocmd bufwritepost ~/.config/nvim/init.vim source ~/.config/nvim/init.vim
let mapleader=','

hi Normal guibg=NONE ctermbg=NONE

nnoremap j gj
nnoremap k gk
nnoremap <leader>r :Ranger<cr>

nnoremap <leader>ev :e ~/.config/nvim/init.vim<cr>
nnoremap <leader>fb :Buffers<cr>
nnoremap <leader>ff :GFiles<cr>
nnoremap <leader>fF :Files<cr>
nnoremap <leader>g :FlyGrep<cr>

nnoremap <leader>B :Bclose<cr>
nnoremap <leader>n :bn<cr>
nnoremap <leader>N :bp<cr>

nnoremap <leader>ln :ALENextWrap<cr>
nnoremap <leader>lN :ALEPreviousWrap<cr>
nnoremap <leader>ll :ALEDetail<cr>

" Use <tab> to cycle through deocomplete completions
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

nnoremap <leader>t :e ~/notes/todo.md<cr>

" Diff-ing
nnoremap <leader>d :windo diffthis<cr>
nnoremap <leader>D :windo diffoff<cr>

" Format JSON
nnoremap <leader>jp :%!jq .<cr>
nnoremap <leader>jc :%!jq -c .<cr>

" Open VSCode
nnoremap <leader>V :!code .<cr><cr>

" MarkdownPreview
nnoremap <leader>m :MarkdownPreview<cr>

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

" Persistent undo
set undofile
set undodir=$HOME/.vim/undo

set undolevels=1000
set undoreload=10000

set shell=/usr/local/bin/zsh

call plug#begin('~/.vim/plugged')

Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'justinmk/vim-sneak'

" Themes
Plug 'kaicataldo/material.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'TroyFletcher/vim-colors-synthwave'
Plug 'jlesquembre/base16-neovim'

" Dependency of ranger.vim
" Allows deletion of buffer while keeping windowopen
Plug 'rbgrouleff/bclose.vim'
Plug 'francoiscabrol/ranger.vim'

" Grep / Search
Plug 'wsdjeg/FlyGrep.vim'
Plug 'google/vim-searchindex'

" Completion helper (like helm)
"Plug 'paradigm/SkyBison'
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }

" Autocomplete + Tern completion engine
Plug 'ternjs/tern_for_vim', { 'for': ['javascript', 'javascript.jsx'] }

" General purpose fuzzy finder
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } 
Plug 'junegunn/fzf.vim'

"Plug 'w0rp/ale'

" Coc
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Language specific plugins
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'Valloric/MatchTagAlways'
Plug 'keith/swift.vim'
Plug 'natebosch/vim-lsc'

" Flow
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

" TypeScript
Plug 'HerringtonDarkholme/yats.vim'
Plug 'mhartington/nvim-typescript', {'do': './install.sh'}

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" post install (yarn install | npm install) then load plugin only for editing supported files
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }

" Markdown
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }
call plug#end()

" Theme
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
let g:deoplete#sources#flow#flow_bin = 'flow'
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
\ 'javascript': ['eslint', 'flow'],
\}

let g:ale_fixers = {
\   'javascript': ['eslint'],
\}

let g:ale_completion_enabled = 1
let g:ale_fix_on_save = 0

let g:airline#extensions#tabline#enabled = 1
let g:airline_theme = 'base16_nord'
colorscheme base16-nord


" Introduced for flow package
let g:LanguageClient_serverCommands = {
    \ 'javascript': ['flow-language-server', '--stdio'],
    \ }

" Use local node_modules flow binary
function! StrTrim(txt)
  return substitute(a:txt, '^\n*\s*\(.\{-}\)\n*\s*$', '\1', '')
endfunction

let g:flow_path = StrTrim(system('PATH=$(npm bin):$PATH && which flow'))

if g:flow_path != 'flow not found'
  let g:deoplete#sources#flow#flow_bin = g:flow_path
endif

" let g:python_host_prog = '/usr/bin/python'
" let g:python3_host_prog = '/usr/local/bin/python3'

" Think this is for vim-javascript - to enable flow highlighting
let g:javascript_plugin_flow = 1

" print spaces between brackets
let g:prettier#config#bracket_spacing = 'true'
" put > on the last line instead of new line
let g:prettier#config#jsx_bracket_same_line = 'false'

" if hidden is not set, TextEdit might fail.
set hidden

" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Create mappings for function text object, requires document symbols feature of languageserver.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <TAB> for select selections ranges, needs server support, like: coc-tsserver, coc-python
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

