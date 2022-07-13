local function combine(on_attaches)
  return function(client, buffnr)
    for _, on_attach in ipairs(on_attaches) do
      on_attach(client, buffnr)
    end
  end
end

local function disable_formatting(client, _)
  client.resolved_capabilities.document_formatting = false
  client.resolved_capabilities.document_range_formatting = false
end

return {
  cssls = { install = true, on_attach = disable_formatting },
  elmls = {},
  hls = {},
  html = { install = true, on_attach = disable_formatting },
  sumneko_lua = { install = true },
  tsserver = { install = true, on_attach = disable_formatting },
  rust_analyzer = { install = true },
}
