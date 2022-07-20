local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system {
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  }
  print "Installing packer close and reopen Neovim..."
  vim.cmd [[packadd packer.nvim]]
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]

-- Use a protected call so we don"t error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
  return
end

-- Have packer use a popup window
packer.init {
  display = {
    open_fn = function()
      return require("packer.util").float { border = "rounded" }
    end,
  },
}

-- Install your plugins here
return packer.startup(function(use)
  use "wbthomason/packer.nvim" -- Have packer manage itself


  -- neovim-from-scratch says these two plugins are often required by other plugins
  use "nvim-lua/plenary.nvim" -- Useful lua functions used ny lots of plugins
  use "nvim-lua/popup.nvim" -- An implementation of the Popup API from vim in Neovim
  use "kyazdani42/nvim-web-devicons"


  -- vikwiki stuff
  use "michal-h21/vimwiki-sync" -- synchronize vimwiki with a git repo
  use "vimwiki/vimwiki" -- note taking with in a wiki syntax


  -- file explorer
  use {
    "kyazdani42/nvim-tree.lua",
    requires = {
      "kyazdani42/nvim-web-devicons", -- optional, for file icons
    },
  }


  -- commands
  use "christoomey/vim-tmux-navigator" -- share C-j, C-h, C-k, C-l with tmux to navigate between splits
  use "duff/vim-bufonly" -- close all buffers with :BufOnly
  use "jpalardy/vim-slime" -- send snippets to a terminal window
  use "vim-scripts/Tabmerge" -- merge tabs with :TabMerge <target index>
  use "windwp/nvim-autopairs" -- auto close things
  use "numToStr/Comment.nvim" -- comment things
  use "tpope/vim-fugitive" -- git stuff


  -- syntax, colors, and themes
  use "arcticicestudio/nord-vim" -- download the official nord colorscheme for (neo)vim
  use "romainl/vim-cool" -- disables search highlighting when you are done searching and re-enables it when you search again 
  use "sheerun/vim-polyglot" -- big collection of syntax highlighting for various file types
  use { -- advanced syntax highlighting
    "nvim-treesitter/nvim-treesitter",
    run = function() require("nvim-treesitter.install").update({ with_sync = true }) end,
  }
  use "p00f/nvim-ts-rainbow" -- rainbow parens for treesitter
  use { -- status line
    "nvim-lualine/lualine.nvim",
    requires = { "kyazdani42/nvim-web-devicons", opt = true }
  }
  use "alvarosevilla95/luatab.nvim"


  -- sql
  use "tpope/vim-dadbod"
  use "kristijanhusak/vim-dadbod-completion"


  -- lsp
  use "neovim/nvim-lspconfig" -- enable LSP
  use "williamboman/nvim-lsp-installer" -- simple to use language server installer
  use "jose-elias-alvarez/null-ls.nvim"
  use "simrat39/rust-tools.nvim"


  -- snippets
  use "L3MON4D3/LuaSnip"
  use "rafamadriz/friendly-snippets"


  -- completion
  use "hrsh7th/cmp-buffer"
  use "hrsh7th/cmp-cmdline"
  use "hrsh7th/cmp-nvim-lsp"
  use "hrsh7th/cmp-nvim-lua"
  use "hrsh7th/cmp-path"
  use "hrsh7th/nvim-cmp"
  use "saadparwaiz1/cmp_luasnip"


  -- misc
  use "lewis6991/impatient.nvim" -- startup caching
  use 'mfussenegger/nvim-dap' -- add debugging


  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if PACKER_BOOTSTRAP then
    require("packer").sync()
  end
end)
