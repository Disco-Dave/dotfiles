local colorscheme = "nord" -- name of the colorscheme to use

local function set_colorscheme()
  vim.opt.cursorline = true -- highlight the current line
  vim.g.nord_cursor_line_number_background = 1 -- extend cursorline highlight into the number column
  vim.g.nord_italic = 1 -- enable italic fonts
  vim.g.nord_bold = 1 -- enable bold fonts
  vim.g.nord_underline = 1 -- enable underlined fonts
  vim.g.nord_uniform_diff_background = 1 -- highlight the foreground instead of background when diffing

  vim.cmd("colorscheme " .. colorscheme) -- it was important that we set all those vim.g variables before calling this
end

local status_ok, _ = pcall(set_colorscheme)
if not status_ok then
  return
end
