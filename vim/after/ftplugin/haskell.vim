" Set formatter for haskell
let g:neoformat_enabled_haskell = ['brittany']

" Run neoformat on save
"augroup haskell-fmt
  "autocmd!
  "autocmd BufWritePre *.hs undojoin | Neoformat
"augroup END

" Set tab to 2 spaces
setlocal softtabstop=2 shiftwidth=2 expandtab

setlocal signcolumn=no
