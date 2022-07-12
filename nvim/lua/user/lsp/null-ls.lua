return function()
  local status_ok, null_ls = pcall(require, "null-ls")
  if not status_ok then
    return
  end

  -- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
  -- local formatting = null_ls.builtins.formatting
  -- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
  local diagnostics = null_ls.builtins.diagnostics
  -- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
  local code_actions = null_ls.builtins.code_actions

  null_ls.setup({
    sources = {
      diagnostics.shellcheck,
      code_actions.shellcheck,
    },
  })
end
