local status_ok, treesitter = pcall(require, "nvim-treesitter.configs")
if not status_ok then
  return
end

treesitter.setup({
  auto_install = true,

  sync_install = false,
  ensure_installed = "all",

  ignore_install = { "" }, -- List of parsers to ignore installing

  highlight = {
    enable = true, -- false will disable the whole extension
    disable = { "haskell", "elm", "purescript" }, -- list of language that will be disabled
    additional_vim_regex_highlighting = true,
  },

  rainbow = {
    enable = true,
    disable = {  }, -- list of languages you want to disable the plugin for
    extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
    max_file_lines = nil, -- Do not enable for files with more than n lines, int
    -- colors = {}, -- table of hex strings
    -- termcolors = {} -- table of colour name strings
  }
})
