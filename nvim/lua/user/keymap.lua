local opts = { noremap = true, silent = true } -- options for all key bindings
local keymap = vim.api.nvim_set_keymap -- shorten vim.api.nvim_set_keymap to keymap


-- Terminals --
local term_opts = { silent = true } -- options for all terminal keybindings
keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts) -- move left from a terminal window
keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts) -- move down from a terminal window 
keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts) -- move up from a terminal window
keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts) -- move right from a terminal window

keymap("n", "<leader>nt", ":tabe<CR><ESC>:terminal<CR>", opts) -- open a terminal in a new tab
keymap("n", "<leader>nts", ":split<CR><ESC>:terminal<CR>", opts) -- open a terminal in a horizontal split
keymap("n", "<leader>ntv", ":vsplit<CR><ESC>:terminal<CR>", opts) -- open a terminal in a vertical split


-- Windows --
keymap("n", "<C-h>", "<C-w>h", opts) -- move left from a window
keymap("n", "<C-j>", "<C-w>j", opts) -- move down from a window
keymap("n", "<C-k>", "<C-w>k", opts) -- move up from a window
keymap("n", "<C-l>", "<C-w>l", opts) -- move right from a window

keymap("n", "<C-Up>", ":resize +2<CR>", opts) -- increase vertical space of a window
keymap("n", "<C-Down>", ":resize -2<CR>", opts) -- decrease vertical space of a window
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts) -- decrease horizontal space of a window
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts) -- increase horizontal space of a window


-- Identation --
keymap("v", "<", "<gv", opts) -- keep visual-block selected when running <<
keymap("v", ">", ">gv", opts) -- keep visual-block selected when running >>
