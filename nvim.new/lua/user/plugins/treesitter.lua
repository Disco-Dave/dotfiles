-- https://github.com/nvim-treesitter/nvim-treesitter
-- :help nvim-treesitter

local status_ok, treesitter = pcall(require, "nvim-treesitter.configs")
if not status_ok then
  return
end

local disabled_parsers = {
  "git_rebase",
  "gitattributes",
  "gitcommit",
  "gitignore",
  "markdown",
  "markdown_inline",
}

treesitter.setup({
  auto_install = false,

  sync_install = false,
  ensure_installed = "all",
  ignore_install = disabled_parsers,

  highlight = {
    enable = true,
    disabled = disabled_parsers,
    additional_vim_regex_highlighting = false,
  },
})
