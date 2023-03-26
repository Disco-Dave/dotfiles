local theme = require("user.colorscheme.nord")

local status_ok, _ = pcall(theme)
if not status_ok then
  return
end
