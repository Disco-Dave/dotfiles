vim.cmd([[

let g:vimwiki_list = [
  \ {
    \ 'path': '~/Documents/wiki/personal',
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
  \ },
  \ {
    \ 'path': '~/Documents/wiki/work',
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
  \ },
\]


function! VimwikiLinkHandler(link)
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

]])
