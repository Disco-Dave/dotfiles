-- https://github.com/wbthomason/packer.nvim
-- :help packer.txt


-- Clone packer if it is not already present.
local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()


local status_ok, packer = pcall(require, "packer")
if not status_ok then
  return
end


return packer.startup(function(use)
  use "wbthomason/packer.nvim"


  -- dependencies
  use "nvim-tree/nvim-web-devicons"
  use "nvim-lua/plenary.nvim"
  use "nvim-lua/popup.nvim"


  -- snippet
  use({
    "L3MON4D3/LuaSnip",
    run = "make install_jsregexp"
  })


  -- autocompletion
  use "hrsh7th/cmp-buffer"
  use "hrsh7th/cmp-path"
  use "hrsh7th/cmp-nvim-lsp"
  use "hrsh7th/cmp-nvim-lua"
  use "saadparwaiz1/cmp_luasnip"
  use "hrsh7th/nvim-cmp"


  -- lsp
  use "neovim/nvim-lspconfig"
  use "williamboman/mason.nvim"
  use "williamboman/mason-lspconfig.nvim"
  use "jose-elias-alvarez/null-ls.nvim"


  -- sql
  use "tpope/vim-dadbod"
  use "kristijanhusak/vim-dadbod-completion"


  -- vimwiki
  use "michal-h21/vimwiki-sync" -- synchronize vimwiki with a git repo
  use "vimwiki/vimwiki" -- note taking with in a wiki syntax


  -- others
  use "arcticicestudio/nord-vim" -- official nord colorscheme
  use "christoomey/vim-tmux-navigator" -- share C-j, C-h, C-k, C-l with tmux to navigate between splits
  use "duff/vim-bufonly" -- close all buffers with :BufOnly
  use "jpalardy/vim-slime" -- send snippets to a terminal window
  use "luochen1990/rainbow" -- rainbow parens
  use "numToStr/Comment.nvim" -- comment things
  use "NvChad/nvim-colorizer.lua" -- highlight color
  use "nvim-lualine/lualine.nvim" -- fancy statusline
  use "nvim-tree/nvim-tree.lua" -- file tree
  use "romainl/vim-cool" -- disables search highlighting when you are done searching and re-enables it when you search again
  use "sheerun/vim-polyglot" -- big collection of syntax highlighting for various file types
  use "tpope/vim-fugitive" -- git stuff
  use "vim-scripts/Tabmerge" -- merge tabs with :TabMerge <target index>
  use "windwp/nvim-autopairs" -- auto close things
  use "editorconfig/editorconfig-vim" -- editorconfig support. NOTE: this will be built into neovim soon


  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require("packer").sync()
  end
end)
