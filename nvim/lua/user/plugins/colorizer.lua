local colorizer_status_ok, colorizer = pcall(require, "colorizer")

if colorizer_status_ok then
  colorizer.setup({
    filetypes = {
      "html",
      "css",
    },
    user_default_options = {
      mode = "virtualtext",
    },
  })
end
