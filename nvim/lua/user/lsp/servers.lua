local function combine(...)
  local on_attaches = { ... }
  return function(client, buffnr)
    for _, on_attach in pairs(on_attaches) do
      on_attach(client, buffnr)
    end
  end
end

local function disable_formatting(client, _)
  client.resolved_capabilities.document_formatting = false
  client.resolved_capabilities.document_range_formatting = false
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
