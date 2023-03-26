return {
  setup = function(_, bufnr)
    local function keymap(mode, bind, action)
      local opts = { noremap = true, silent = true, buffer = bufnr }
      vim.keymap.set(mode, bind, action, opts)
    end

    keymap("n", "[g", vim.diagnostic.goto_prev)
    keymap("n", "]g", vim.diagnostic.goto_next)
    keymap("n", "<leader>e", vim.diagnostic.open_float)
    keymap("n", "<leader>q", vim.diagnostic.setloclist)

    keymap("n", "<leader>D", vim.lsp.buf.declaration)
    keymap("n", "<leader>d", vim.lsp.buf.definition)
    keymap("n", "<leader>h", vim.lsp.buf.hover)
    keymap("n", "<leader>I", vim.lsp.buf.implementation)
    keymap("n", "<leader>R", vim.lsp.buf.rename)
    keymap("n", "<leader>r", vim.lsp.buf.references)
    keymap("n", "<leader>a", vim.lsp.buf.code_action)
    keymap("n", "<leader>f", vim.lsp.buf.format)
    keymap("v", "<leader>f", vim.lsp.buf.range_formatting)
  end
}
