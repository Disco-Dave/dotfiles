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

  use "sheerun/vim-polyglot" -- big collection of syntax highlighting for various file types


  -- colorschemes
  use "arcticicestudio/nord-vim" -- official nord colorscheme


  -- commands
  use "duff/vim-bufonly" -- close all buffers with :BufOnly
  use "jpalardy/vim-slime" -- select text in a buffer, send it to a neovim terminal
  use "numToStr/Comment.nvim" -- comment things
  use "vim-scripts/Tabmerge" -- merge tabs with :TabMerge <target index>


  -- behavior
  use "christoomey/vim-tmux-navigator" -- share C-j, C-h, C-k, C-l with tmux to navigate between splits
  use "romainl/vim-cool" -- disables search highlighting when you are done searching and re-enables it when you search again
  use "windwp/nvim-autopairs" -- auto close things


  -- treesitter
  use {
    "nvim-treesitter/nvim-treesitter",
    run = function()
      local ts_update = require("nvim-treesitter.install").update({ with_sync = true })
      ts_update()
    end,
  }


  -- cmp
  use "hrsh7th/cmp-buffer"
  use "hrsh7th/cmp-path"
  use "hrsh7th/cmp-omni"
  use "hrsh7th/cmp-nvim-lsp"
  use "hrsh7th/cmp-nvim-lua"
  use "hrsh7th/nvim-cmp"


  -- git
  use "tpope/vim-fugitive" -- git stuff


  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require("packer").sync()
  end
end)
