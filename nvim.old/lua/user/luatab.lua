local status_ok, luatab = pcall(require, "luatab")
if not status_ok then
  return
end

luatab.setup({})
