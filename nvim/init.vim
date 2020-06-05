call plug#begin('~/.local/share/nvim/plugged')

Plug 'Raimondi/delimitMate' " Auto-close quotes, parenthesis, brackets, etc.
Plug 'duff/vim-bufonly' " A script to close all buffers but the one that is open
Plug 'itchyny/lightline.vim' " Adds a statusline
Plug 'jpalardy/vim-slime' " Adds REPL support
Plug 'kien/rainbow_parentheses.vim' " Color codes parenthesis or brackets
Plug 'neoclide/coc.nvim', {'branch': 'release'} " VSCode like LSP client
Plug 'romainl/vim-cool' " Disables search highlighting when you are done searching and re-enables it when you search again 
Plug 'sbdchd/neoformat' " Add a :Neoformat command to format code
Plug 'scrooloose/nerdcommenter' " An auto comment or un-commenting command
Plug 'scrooloose/nerdtree' " A file browser
Plug 'sheerun/vim-polyglot' " Adds a bunch of syntax highlight suport for many file types
Plug 'tpope/vim-fugitive' " Git support
Plug 'vim-scripts/Tabmerge' " A script to merge tabs

call plug#end()


" Indent settings
set autoindent
set expandtab
set softtabstop=4
set tabstop=4
set shiftwidth=4

" Fold settings
set foldlevel=5
set foldmethod=syntax
set foldnestmax=10
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

" Turn syntax highlighting on
syntax on

" Recommended from https://github.com/neoclide/coc.nvim#example-vim-configuration
set hidden
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

" Figutive
if &diff
    noremap <leader>1 :diffget LOCAL<CR>
    noremap <leader>2 :diffget BASE<CR>
    noremap <leader>3 :diffget REMOTE<CR>
endif

" Terminal
nnoremap <leader>ntt :tabe<CR><ESC>:terminal<CR>
nnoremap <leader>nts :split<CR><ESC>:terminal<CR>
nnoremap <leader>ntv :vsplit<CR><ESC>:terminal<CR>

" CoC Key Bindings
inoremap <silent><expr> <c-space> coc#refresh()
nmap <silent> <leader>I <Plug>(coc-implementation)
nmap <silent> <leader>h :call CocAction('doHover')<CR>
nmap <silent> <leader>d <Plug>(coc-definition)
nmap <silent> <leader>t <Plug>(coc-type-definition)
nmap <silent> <leader>r <Plug>(coc-references)
nmap <silent> <leader>R <Plug>(coc-rename)
vmap <silent> <leader>f <Plug>(coc-format-selected)
nmap <silent> <leader>f :call CocAction('format')<CR>
nmap <silent> <leader>m <Plug>(coc-codeaction)
nmap <silent> <leader>e <Plug>(coc-diagnostic-info)

" Rainbow parens
autocmd Syntax * RainbowParenthesesActivate
autocmd Syntax * RainbowParenthesesLoadRound
autocmd Syntax * RainbowParenthesesLoadSquare
autocmd Syntax * RainbowParenthesesLoadBraces

" Lightline settings
" Taken from :help lightline-nice-examples
let g:lightline = {
    \ 'colorscheme': 'wombat',
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
