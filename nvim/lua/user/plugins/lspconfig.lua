-- https://github.com/neovim/nvim-lspconfig
-- :help lspconfig
local lspconfig_status_ok, lspconfig = pcall(require, "lspconfig")
if not lspconfig_status_ok then
  return
end


-- https://github.com/hrsh7th/cmp-nvim-lsp
local cmp_nvim_lsp_status_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
if not cmp_nvim_lsp_status_ok then
  return
end

local capabilities = cmp_nvim_lsp.default_capabilities()


-- :lua =vim.lsp.get_active_clients()[1].server_capabilities
-- Useful for when I want formatting to come from null-ls.
local function disable_formatting(client)
  client.server_capabilities.documentFormattingProvider = false
  client.server_capabilities.documentRangeFormattingProvider = false
end


-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
-- :help lspconfig-all


-- Lua
lspconfig.lua_ls.setup({
  capabilities = capabilities,
  settings = {
    Lua = {
      diagnostics = {
        globals = { 'vim' },
      },
      telemetry = {
        enable = false,
      },
    },
  },
})


-- Haskell
lspconfig.hls.setup({
  capabilities = capabilities,
  settings = {
    haskell = {
      formattingProvider = "fourmolu",
    },
  },
})


-- Elm
lspconfig.elmls.setup({
  capabilities = capabilities,
})


-- Rust
lspconfig.rust_analyzer.setup({
  capabilities = capabilities,
})


-- Typescript
lspconfig.tsserver.setup({
  capabilities = capabilities,
  on_attach = disable_formatting,
})


-- CSS
lspconfig.cssls.setup({
  capabilities = capabilities,
  on_attach = disable_formatting,
})

lspconfig.stylelint_lsp.setup({
  capabilities = capabilities,
  on_attach = disable_formatting,
})


-- HTML
lspconfig.html.setup({
  capabilities = capabilities,
  on_attach = disable_formatting,
})


-- JSON
lspconfig.jsonls.setup({
  capabilities = capabilities,

})


-- YAML
lspconfig.yamlls.setup({
  capabilities = capabilities,
  settings = {
    redhat = {
      telemetry = {
        enabled = false,
      },
    },
    yaml = {
      keyOrdering = false,
    },
  },
})


-- Docker
lspconfig.dockerls.setup({
  capabilities = capabilities,
})

lspconfig.docker_compose_language_service.setup({
  capabilities = capabilities,
})


-- Terraform
lspconfig.terraformls.setup({
  capabilities = capabilities,
})

lspconfig.tflint.setup({
  capabilities = capabilities,
})


-- Ansible
lspconfig.ansiblels.setup({
  capabilities = capabilities,
})
