-- "name": name of server from :help lspconfig-all
-- "install": boolean value indicating if the server should be installed by lsp-installer
return {
  cssls = { install = true, },
  elmls = {},
  hls = { disable_formatting = false }, -- TODO Get native fourmolu via null-ls working
  html = { install = true },
  sumneko_lua = { install = true },
  tsserver = { install = true },
}
