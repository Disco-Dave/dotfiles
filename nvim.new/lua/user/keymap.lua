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


-- Diagnostics
vim.keymap.set("n", "[g", vim.diagnostic.goto_prev, opts)
vim.keymap.set("n", "]g", vim.diagnostic.goto_next, opts)
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, opts)
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, opts)


-- LSP
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    local buffer_opts = vim.tbl_deep_extend("force", opts, { buffer = ev.buf })

    vim.keymap.set("n", "<leader>D", vim.lsp.buf.declaration, buffer_opts)
    vim.keymap.set("n", "<leader>d", vim.lsp.buf.definition, buffer_opts)
    vim.keymap.set("n", "<leader>h", vim.lsp.buf.hover, buffer_opts)
    vim.keymap.set("n", "<leader>I", vim.lsp.buf.implementation, buffer_opts)
    vim.keymap.set("n", "<leader>R", vim.lsp.buf.rename, buffer_opts)
    vim.keymap.set("n", "<leader>r", vim.lsp.buf.references, buffer_opts)
    vim.keymap.set("n", "<leader>a", vim.lsp.buf.code_action, buffer_opts)
    vim.keymap.set("n", "<leader>f", vim.lsp.buf.format, buffer_opts)
    vim.keymap.set("v", "<leader>f", vim.lsp.buf.range_formatting, buffer_opts)
  end,
})
