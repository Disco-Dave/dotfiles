call plug#begin('~/.local/share/nvim/plugged')

Plug 'Raimondi/delimitMate' " Auto-close quotes, parenthesis, brackets, etc.
Plug 'duff/vim-bufonly' " A script to close all buffers but the one that is open
Plug 'itchyny/lightline.vim' " Adds a statusline
Plug 'jpalardy/vim-slime' " Adds REPL support
Plug 'junegunn/rainbow_parentheses.vim' " Color codes parenthesis or brackets
Plug 'mattn/emmet-vim', { 'for': [ 'html', 'css', 'javascriptreact' ] } " html snippets
Plug 'neoclide/coc.nvim', {'branch': 'release'} " VSCode like LSP client
Plug 'romainl/vim-cool' " Disables search highlighting when you are done searching and re-enables it when you search again 
Plug 'sbdchd/neoformat' " Add a :Neoformat command to format code
Plug 'scrooloose/nerdcommenter' " An auto comment or un-commenting command
Plug 'scrooloose/nerdtree' " A file browser
Plug 'sheerun/vim-polyglot' " Adds a bunch of syntax highlight suport for many file types
Plug 'tpope/vim-fugitive' " Git support
Plug 'vim-scripts/Tabmerge' " A script to merge tabs
Plug 'arcticicestudio/nord-vim' " nord color scheme
Plug 'editorconfig/editorconfig-vim' " editorconfig support
Plug 'vmchale/dhall-vim' " dhall support

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

" Indent settings
set autoindent
set expandtab
set softtabstop=4
set tabstop=4
set shiftwidth=4

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
set undodir=~/.local/share/nvim/undo//
set backupdir=~/.local/share/nvim/backup//
set directory=~/.local/share/nvim/swap//

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
nmap <silent> <leader>h :call CocAction('doHover')<CR>
nmap <silent> <leader>d <Plug>(coc-definition)
nmap <silent> <leader>t <Plug>(coc-type-definition)
nmap <silent> <leader>r <Plug>(coc-references)
nmap <silent> <leader>R <Plug>(coc-rename)
vmap <silent> <leader>f <Plug>(coc-format-selected)
nmap <silent> <leader>f <Plug>(coc-format)
nmap <silent> <leader>F <Plug>(coc-fix-current)
nmap <silent> <leader>m <Plug>(coc-codeaction)
nmap <silent> <leader>e <Plug>(coc-diagnostic-info)
nmap <silent> <leader>a <Plug>(coc-codeaction-line)
vmap <silent> <leader>a <Plug>(coc-codeaction-selected)
nmap <silent> <leader>c <Plug>(coc-codelens-action)

" CoC extensions
let g:coc_global_extensions=[
    \ 'coc-css',
    \ 'coc-eslint',
    \ 'coc-fsharp',
    \ 'coc-html',
    \ 'coc-json',
    \ 'coc-omnisharp',
    \ 'coc-prettier',
    \ 'coc-rust-analyzer',
    \ 'coc-sh',
    \ 'coc-metals',
    \ 'coc-tsserver'
\ ]

" Rainbow parens
let g:rainbow#pairs = [['(', ')'], ['[', ']']]
autocmd Syntax * RainbowParentheses

" Since lightline shows the mode, we no longer need it shown on the last line
set noshowmode


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
