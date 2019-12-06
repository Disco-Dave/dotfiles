" Place results in seperate buffers
let g:dbext_default_use_sep_result_buffer = 1

" Set the binaries for various SQL Vendors
let g:dbext_default_SQLSRV_bin = 'sqlcmd'
let g:dbext_default_SQLITE_bin = 'sqlite3'

" I - Enable Quoted Identifiers
" w - Screen width
" r - msgs to stdeer
" b - On error batch abort
let g:dbext_default_SQLSRV_cmd_options = '-I -w 10000 -r -b'

" Enable async queries
let g:dbext_default_use_jobs = 1

" Uppercase keywords
let g:dbext_uppercase_keywords = 1

" Replace window title with connection info
let g:dbext_default_replace_title = 1

" Store SQL history in a file located in ~/.local/share/nvim
let g:dbext_default_history_file = '~/.local/share/nvim/sql_history.txt'

" SQL Completion
let g:sql_type_default = 'sqlanywhere'

" Add DBext connection strings if they exist
source ~/.config/nvim/.dbext_profiles.vim
