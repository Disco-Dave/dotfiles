return {
  setup = function()
    local status_ok, null_ls = pcall(require, "null-ls")
    if not status_ok then
      return
    end

    local formatting = null_ls.builtins.formatting
    local diagnostics = null_ls.builtins.diagnostics
    local code_actions = null_ls.builtins.code_actions

    local sources = {
      formatting.prettier.with({
        filetypes = {
          "css",
          "html",
          "javascript",
          "javascriptreact",
          "typescript",
          "typescriptreact",
          "yaml",
        },
      }),
      formatting.shfmt,
      diagnostics.shellcheck,
      code_actions.shellcheck,
    }

    null_ls.setup({ sources = sources })
  end
}
