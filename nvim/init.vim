call plug#begin('~/.local/share/nvim/plugged')

Plug 'Raimondi/delimitMate' " Auto-close quotes, parenthesis, brackets, etc.
Plug 'duff/vim-bufonly' " A script to close all buffers but the one that is open
Plug 'itchyny/lightline.vim' " Adds a statusline
Plug 'jpalardy/vim-slime' " Adds REPL support
Plug 'luochen1990/rainbow' " Color codes parenthesis or brackets
Plug 'mattn/emmet-vim', { 'for': [ 'html', 'css', 'javascriptreact' ] } " html snippets
Plug 'neoclide/coc.nvim', {'branch': 'release'} " VSCode like LSP client
Plug 'romainl/vim-cool' " Disables search highlighting when you are done searching and re-enables it when you search again 
Plug 'sbdchd/neoformat' " Add a :Neoformat command to format code
Plug 'scrooloose/nerdcommenter' " An auto comment or un-commenting command
Plug 'scrooloose/nerdtree' " A file browser
Plug 'sheerun/vim-polyglot' " Adds a bunch of syntax highlight suport for many file types
Plug 'tpope/vim-fugitive' " Git support
Plug 'vim-scripts/Tabmerge' " A script to merge tabs
Plug 'arcticicestudio/nord-vim', {'branch': 'main'} " nord color scheme
Plug 'editorconfig/editorconfig-vim' " editorconfig support
Plug 'vmchale/dhall-vim' " dhall support
Plug 'tpope/vim-surround' " Keybindings for surrounding things with quotes, parenthesis, etc.
Plug 'vimwiki/vimwiki' " Personal wiki managed inside vim, useful notes.
Plug 'michal-h21/vimwiki-sync' " Synchronize vimwiki with git
Plug 'christoomey/vim-tmux-navigator'
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
Plug 'tpope/vim-dadbod', { 'for': ['sql'] }

call plug#end()

" Turn syntax highlighting on
syntax on

" Theme
if (has("termguicolors"))
  set termguicolors
endif
let g:nord_cursor_line_number_background = 1
let g:nord_italic = 1
let g:nord_bold = 1
let g:nord_underline = 1
set cursorline
colorscheme nord

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("nvim-0.5.0") || has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
endif

" Indent settings
set autoindent
set expandtab
set softtabstop=2
set tabstop=2
set shiftwidth=2

" Fold settings
set nofoldenable

" Search settings
set incsearch
set smartcase
set path+=**

" Use relative line numbers
set number relativenumber

" Tab completion for : commands
set wildmenu

" Auto reload the file when it changes on disk
set autoread

" Show current command in the bottom left
set showcmd

" Avoid resizing panes when another closes
set noequalalways

set nohidden

" Recommended from https://github.com/neoclide/coc.nvim#example-vim-configuration
set updatetime=300
set shortmess+=c

" Fixes HMR for Parcel. https://parceljs.org/hmr.html#safe-write
set backupcopy=yes

" Use backup, undo, and swap folders
set backup
set undofile
set undodir=~/.cache/nvim/undo//
set backupdir=~/.cache/nvim/backup//
set directory=~/.cache/nvim/swap//

" Move split focus with Ctrl + hjkl
noremap <silent> <c-h> <c-w>h
noremap <silent> <c-j> <c-w>j
noremap <silent> <c-k> <c-w>k
noremap <silent> <c-l> <c-w>l

set nowrap

" Correct filetype for odd extensions
autocmd BufRead,BufNewFile *.vbproj set filetype=xml
autocmd BufRead,BufNewFile *.csproj set filetype=xml
autocmd BufRead,BufNewFile *.fsproj set filetype=xml
autocmd BufRead,BufNewFile *.cshtml set filetype=html
autocmd BufRead,BufNewFile *.vbhtml set filetype=html

" Enable ftplugins
filetype plugin on

" SLIME
let g:slime_target = "neovim"
let g:slime_paste_file = "$XDG_RUNTIME_DIR/slime"
command JobID :echo b:terminal_job_id
map <leader>i <Plug>SlimeRegionSend

" Show number of matches in command line
let g:CoolTotalMatches = 1

" NERDTree Toggle
noremap <leader>; :NERDTreeToggle<CR>
let NERDTreeBookmarksFile = stdpath('data') . '/NERDTreeBookmarks'

" Terminal
nnoremap <leader>ntt :tabe<CR><ESC>:terminal<CR>
nnoremap <leader>nts :split<CR><ESC>:terminal<CR>
nnoremap <leader>ntv :vsplit<CR><ESC>:terminal<CR>

" Remove color from sign column
highlight SignColumn ctermbg=none

" CoC Key Bindings
inoremap <silent><expr> <c-space> coc#refresh()
nmap <silent> <leader>I <Plug>(coc-implementation)
nmap <silent> <leader>h :call CocActionAsync('doHover')<CR>
nmap <silent> <leader>d <Plug>(coc-definition)
nmap <silent> <leader>t <Plug>(coc-type-definition)
nmap <silent> <leader>r <Plug>(coc-references)
nmap <silent> <leader>R <Plug>(coc-rename)
vmap <silent> <leader>f <Plug>(coc-format-selected)
nmap <silent> <leader>f <Plug>(coc-format)
nmap <silent> <leader>F <Plug>(coc-fix-current)
nmap <silent> <leader>m <Plug>(coc-codeaction)
nmap <silent> <leader>e <Plug>(coc-diagnostic-info)
nmap <silent> <leader>a <Plug>(coc-codeaction-cursor)
vmap <silent> <leader>a <Plug>(coc-codeaction-selected)
nmap <silent> <leader>c <Plug>(coc-codelens-action)

" CoC extensions
let g:coc_global_extensions=[
    \ '@yaegassy/coc-ansible',
    \ 'coc-css',
    \ 'coc-db',
    \ 'coc-docker',
    \ 'coc-eslint',
    \ 'coc-html',
    \ 'coc-json',
    \ 'coc-rust-analyzer',
    \ 'coc-tsserver',
    \ 'coc-yaml',
\ ]

" Rainbow parens
let g:rainbow_active = 1

" Since lightline shows the mode, we no longer need it shown on the last line
set noshowmode

" Enables autocompletion in sql files
let g:db = $DATABASE_URL

" Lightline settings
" Taken from :help lightline-nice-examples
let g:lightline = {
    \ 'colorscheme': 'nord',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ] ]
    \ },
    \ 'component_function': {
    \   'fugitive': 'LightlineFugitive',
    \   'filename': 'LightlineFilename'
    \ }
    \ }
function! LightlineModified()
    return &ft =~# 'help\|vimfiler' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction
function! LightlineReadonly()
    return &ft !~? 'help\|vimfiler' && &readonly ? 'RO' : ''
endfunction
function! LightlineFilename()
    return (LightlineReadonly() !=# '' ? LightlineReadonly() . ' ' : '') .
    \ (&ft ==# 'vimfiler' ? vimfiler#get_status_string() :
    \  &ft ==# 'unite' ? unite#get_status_string() :
    \  &ft ==# 'vimshell' ? vimshell#get_status_string() :
    \ expand('%:t') !=# '' ? expand('%:t') : '[No Name]') .
    \ (LightlineModified() !=# '' ? ' ' . LightlineModified() : '')
endfunction
function! LightlineFugitive()
    if &ft !~? 'vimfiler' && exists('*FugitiveHead')
        return FugitiveHead()
    endif
    return ''
endfunction


" Vimwiki settings
let g:vimwiki_list = [{
  \ 'path': '~/.local/share/personal-wiki', 
  \ 'auto_toc': 1, 
  \ 'auto_diary_index': 1, 
  \ 'diary_caption_level': -1,
  \ 'nested_syntaxes': {
    \ 'bash': 'bash',
    \ 'cabal': 'cabal',
    \ 'csharp': 'csharp',
    \ 'css': 'css',
    \ 'dhall': 'dhall',
    \ 'elm': 'elm',
    \ 'fsharp': 'fsharp',
    \ 'go': 'go',
    \ 'haskell': 'haskell',
    \ 'html': 'html',
    \ 'javascript': 'javascript',
    \ 'json': 'json',
    \ 'purescript': 'purescript',
    \ 'python': 'python',
    \ 'ruby': 'ruby',
    \ 'rust': 'rust',
    \ 'sh': 'sh',
    \ 'sql': 'sql',
    \ 'typescript': 'typescript',
    \ 'vim': 'vim',
    \ 'yaml': 'yaml',
    \ 'zsh': 'zsh'
    \ }
  \ }]

function! VimwikiLinkHandler(link)
  " Use Vim to open external files with the 'vfile:' scheme.  E.g.:
  "   1) [[vfile:~/Code/PythonProject/abc123.py]]
  "   2) [[vfile:./|Wiki Home]]
  let link = a:link
  if link =~# '^vfile:'
    let link = link[1:]
  else
    return 0
  endif
  let link_infos = vimwiki#base#resolve_link(link)
  if link_infos.filename == ''
    echomsg 'Vimwiki Error: Unable to resolve link!'
    return 0
  else
    exe 'tabnew ' . fnameescape(link_infos.filename)
    return 1
  endif
endfunction
