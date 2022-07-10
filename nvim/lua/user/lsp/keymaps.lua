return function(_, bufnr)
  local opts = { noremap = true, silent = true } -- options for all key bindings
  local keymap = vim.api.nvim_buf_set_keymap -- shorten vim.api.nvim_set_keymap to keymap

  keymap(bufnr, "n", "<leader>D", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  keymap(bufnr, "n", "<leader>d", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  keymap(bufnr, "n", "<leader>h", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  keymap(bufnr, "n", "<leader>I", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)

  keymap(bufnr, "n", "<leader>R", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  keymap(bufnr, "n", "<leader>r", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  keymap(bufnr, "n", "<leader>a", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  keymap(bufnr, "n", "<leader>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  keymap(bufnr, "v", "<leader>f", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)

  keymap(bufnr, "n", "[g", '<cmd>lua vim.diagnostic.goto_prev({ border = "rounded" })<CR>', opts)
  keymap(bufnr, "n", "]g", '<cmd>lua vim.diagnostic.goto_next({ border = "rounded" })<CR>', opts)

  keymap(
    bufnr,
    "n",
    "<leader>e",
    '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics({ border = "rounded" })<CR>',
    opts
  )


  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)

  vim.cmd [[ command! Format execute 'lua vim.lsp.buf.formatting()' ]]
end
