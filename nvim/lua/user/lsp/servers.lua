local function combine(...)
  local on_attaches = { ... }
  return function(client, buffnr)
    for _, on_attach in pairs(on_attaches) do
      on_attach(client, buffnr)
    end
  end
end

-- :lua =vim.lsp.get_active_clients()[1].server_capabilities

local function disable_formatting(client, _)
  client.server_capabilities.documentFormattingProvider = false
  client.server_capabilities.documentRangeFormattingProvider = false
end

return {
  ansiblels = { install = true, mason_name = "ansible-language-server" },
  cssls = { install = true, on_attach = disable_formatting },
  dhall_lsp_server = { install = true },
  dockerls = { install = true },
  elmls = {},
  hls = {},
  html = { install = true, on_attach = disable_formatting },
  jsonls = { install = true },
  purescriptls = { install = true, on_attach = disable_formatting },
  -- rubocop = { install = true },
  rust_analyzer = { install = true },
  solargraph = { install = true },
  sorbet = { install = true },
  sumneko_lua = { install = true },
  terraformls = { install = true },
  tflint = { install = true },
  tsserver = { install = true, on_attach = disable_formatting },
  vimls = { install = true },
  yamlls = { install = true },
}
