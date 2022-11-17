return {
  setup = function()
    local status_ok, null_ls = pcall(require, "null-ls")
    if not status_ok then
      return
    end

    local formatting = null_ls.builtins.formatting
    local diagnostics = null_ls.builtins.diagnostics
    local code_actions = null_ls.builtins.code_actions

    local h = require("null-ls.helpers")
    local methods = require("null-ls.methods")

    local FORMATTING = methods.internal.FORMATTING

    local purstidy = h.make_builtin({
      name = "tidy",
      method = { FORMATTING },
      filetypes = { "purescript" },
      generator_opts = {
        command = "purs-tidy",
        args = {
          "format",
        },
        to_stdin = true,
      },
      factory = h.formatter_factory,
    })

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
      purstidy,
      -- formatting.rubocop,
      -- diagnostics.rubocop,
    }

    null_ls.setup({ sources = sources })
  end
}
