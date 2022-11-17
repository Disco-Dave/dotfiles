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
  ansiblels = { install = true },
  cssls = { install = true, on_attach = disable_formatting },
  dhall_lsp_server = { install = true },
  dockerls = { install = true },
  elmls = {},
  hls = {},
  html = { install = true, on_attach = disable_formatting },
  jsonls = { install = true },
  purescriptls = { install = true, on_attach = disable_formatting },
  rust_analyzer = { install = true },
  solargraph = { install = true },
  sumneko_lua = { install = true },
  terraformls = { install = true },
  tflint = { install = true },
  tsserver = { install = true, on_attach = disable_formatting },
  vimls = { install = true },
  yamlls = { install = true },
}
