-- https://github.com/williamboman/mason-lspconfig.nvim
-- :help mason-lspconfig

local mason_status_ok, mason = pcall(require, "mason")
if not mason_status_ok then
  return
end


local mason_lspconfig_status_ok, mason_lspconfig = pcall(require, "mason-lspconfig")
if not mason_lspconfig_status_ok then
  return
end


mason.setup()

mason_lspconfig.setup({
  -- https://github.com/williamboman/mason-lspconfig.nvim#available-lsp-servers
  ensure_installed = {
    "ansiblels",
    "cssls",
    "docker_compose_language_service",
    "dockerls",
    "html",
    "jsonls",
    "lua_ls",
    "stylelint_lsp",
    "terraformls",
    "tflint",
    "tsserver",
    "yamlls",
  }
})
