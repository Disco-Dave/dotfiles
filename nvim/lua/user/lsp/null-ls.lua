return {
  setup = function()
    local status_ok, null_ls = pcall(require, "null-ls")
    if not status_ok then
      return
    end

    local formatting = null_ls.builtins.formatting
    local diagnostics = null_ls.builtins.diagnostics
    local code_actions = null_ls.builtins.code_actions

    null_ls.setup({
      sources = {
        formatting.prettier,
        formatting.shfmt,
        diagnostics.shellcheck,
        code_actions.shellcheck,
      },
    })
  end
}
