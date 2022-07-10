local status_ok, lsp_installer = pcall(require, "nvim-lsp-installer")
if not status_ok then
	return
end

-- Register a handler that will be called for all installed servers.
-- Alternatively, you may also register handlers on specific server instances instead (see example below).
lsp_installer.on_server_ready(function(server)
	local opts = {
		on_attach = require("user.lsp.handlers").on_attach,
		capabilities = require("user.lsp.handlers").capabilities,
		ensure_installed = {
			"sumneko_lua",
			"hls",
		},
	}

	local additional_settings = {
		"sumneko_lua",
	}

	for _, lsp_name in pairs(additional_settings) do
		if server.name == lsp_name then
			local extra_opts = require("user.lsp.settings." .. lsp_name)
			opts = vim.tbl_deep_extend("force", extra_opts, opts)
		end
	end

	-- This setup() function is exactly the same as lspconfig's setup function.
	-- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
	server:setup(opts)
end)
