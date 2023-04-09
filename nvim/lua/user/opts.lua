-- Syntax Highlighting --
vim.opt.syntax = "on"        -- enable syntax highlighting
vim.opt.termguicolors = true -- enables 24-bit rgb color in the |tui


-- Tabs --
vim.b.tab_size = 2

vim.opt.expandtab = true            -- replace tab with spaces
vim.opt.shiftwidth = vim.b.tab_size -- number of spaces for << and >>
vim.opt.tabstop = vim.b.tab_size    -- number of spaces for tab


-- Line Numbers --
vim.opt.number = true         -- adds actual line number for current line
vim.opt.numberwidth = 1       -- width of the line number column
vim.opt.relativenumber = true -- enables relative line numbering
vim.opt.signcolumn = "number" -- adds signs to the number column

vim.api.nvim_create_autocmd('TermOpen', {
  callback = function()
    -- Remove line numbers for terminal
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
  end,
})

-- Windows --
vim.opt.equalalways = false -- avoid resizing windows after splitting or closing
vim.opt.splitbelow = true   -- force all horizontal splits to go below current window
vim.opt.splitright = true   -- force all vertical splits to go to the right of current window


-- Completion --
vim.opt.completeopt = { "menu", "menuone", "noselect" } -- recommended from https://github.com/knubie/nvim-cmp#recommended-configuration
vim.opt.shortmess:append "c"                            -- don't give ins-completion-menu messages
vim.opt.updatetime = 300                                -- faster completion (4000ms default)


-- Backups and Undo Files --
vim.opt.backup = true                                          -- enables backup files
vim.opt.backupcopy = "yes"                                     -- Fixes hot reloading for things like parcel
vim.opt.undofile = true                                        -- enables undo files, keeps undo tree in between sessions
vim.opt.undodir = vim.env.XDG_CACHE_HOME .. "/nvim/undo//"     -- store the undo files in $XDG_CONFIG_HOME
vim.opt.backupdir = vim.env.XDG_CACHE_HOME .. "/nvim/backup//" -- store the backup files in $XDG_CONFIG_HOME
vim.opt.directory = vim.env.XDG_CACHE_HOME .. "/nvim/swap//"   -- store the swap files in $XDG_CONFIG_HOME


-- Folding --
vim.opt.foldenable = false -- disable all syntax folding


-- Peripherals --
vim.opt.mouse = "nv" -- enable the mouse for normal and visual mode


-- Misc --
vim.opt.showmode = false -- we don't need to see things like -- INSERT -- anymore
vim.opt.wrap = false     -- display lines as one long line
