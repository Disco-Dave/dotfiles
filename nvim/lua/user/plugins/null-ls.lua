-- https://github.com/jose-elias-alvarez/null-ls.nvim
local null_ls_status_ok, null_ls = pcall(require, "null-ls")
if not null_ls_status_ok then
  return
end


local opts = {
  sources = {
    null_ls.builtins.formatting.prettier.with({
      filetypes = {
        "css",
        "html",
        "javascript",
        "javascriptreact",
        "typescript",
        "typescriptreact",
        "yaml",
        "json",
      },
    }),
    null_ls.builtins.formatting.shfmt,
    null_ls.builtins.diagnostics.shellcheck,
    null_ls.builtins.code_actions.shellcheck,
  }
}


null_ls.setup(opts)
