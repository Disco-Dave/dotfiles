local function keymap(mode, bind, action)
  local opts = { noremap = true, silent = true }
  vim.keymap.set(mode, bind, action, opts)
end

local function term_keymap(bind, action)
  local term_opts = { silent = true }
  vim.keymap.set("t", bind, action, term_opts)
end

-- Terminals --
term_keymap("<C-h>", "<C-\\><C-N><C-w>h") -- move left from a terminal window
term_keymap("<C-j>", "<C-\\><C-N><C-w>j") -- move down from a terminal window 
term_keymap("<C-k>", "<C-\\><C-N><C-w>k") -- move up from a terminal window
term_keymap("<C-l>", "<C-\\><C-N><C-w>l") -- move right from a terminal window

keymap("n", "<leader>nt", ":tabe<CR><ESC>:terminal<CR>") -- open a terminal in a new tab
keymap("n", "<leader>nts", ":split<CR><ESC>:terminal<CR>") -- open a terminal in a horizontal split
keymap("n", "<leader>ntv", ":vsplit<CR><ESC>:terminal<CR>") -- open a terminal in a vertical split


-- Windows --
keymap("n", "<C-h>", "<C-w>h") -- move left from a window
keymap("n", "<C-j>", "<C-w>j") -- move down from a window
keymap("n", "<C-k>", "<C-w>k") -- move up from a window
keymap("n", "<C-l>", "<C-w>l") -- move right from a window

keymap("n", "<C-Up>", ":resize +2<CR>") -- increase vertical space of a window
keymap("n", "<C-Down>", ":resize -2<CR>") -- decrease vertical space of a window
keymap("n", "<C-Left>", ":vertical resize -2<CR>") -- decrease horizontal space of a window
keymap("n", "<C-Right>", ":vertical resize +2<CR>") -- increase horizontal space of a window
keymap("v", "<leader>i", "<Plug>SlimeRegionSend") -- send region to neovim terminal
