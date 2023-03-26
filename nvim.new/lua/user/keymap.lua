local opts = {
  noremap = true,
  silent = true,
}


-- Terminals --
vim.keymap.set("n", "<leader>nt", ":tabe<CR><ESC>:terminal<CR>", opts) -- open a terminal in a new tab
vim.keymap.set("n", "<leader>nts", ":split<CR><ESC>:terminal<CR>", opts) -- open a terminal in a horizontal split
vim.keymap.set("n", "<leader>ntv", ":vsplit<CR><ESC>:terminal<CR>", opts) -- open a terminal in a vertical split


-- Windows --
vim.keymap.set("n", "<C-h>", "<C-w>h", opts) -- move left from a window
vim.keymap.set("n", "<C-j>", "<C-w>j", opts) -- move down from a window
vim.keymap.set("n", "<C-k>", "<C-w>k", opts) -- move up from a window
vim.keymap.set("n", "<C-l>", "<C-w>l", opts) -- move right from a window

vim.keymap.set("n", "<C-Up>", ":resize +2<CR>", opts) -- increase vertical space of a window
vim.keymap.set("n", "<C-Down>", ":resize -2<CR>", opts) -- decrease vertical space of a window
vim.keymap.set("n", "<C-Left>", ":vertical resize -2<CR>", opts) -- decrease horizontal space of a window
vim.keymap.set("n", "<C-Right>", ":vertical resize +2<CR>", opts) -- increase horizontal space of a window
