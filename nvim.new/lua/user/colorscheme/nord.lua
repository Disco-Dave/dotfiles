return function()
  vim.opt.cursorline = true -- highlight the current line
  vim.g.nord_cursor_line_number_background = 1 -- extend cursorline highlight into the number column
  vim.g.nord_italic = 1 -- enable italic fonts
  vim.g.nord_bold = 1 -- enable bold fonts
  vim.g.nord_underline = 1 -- enable underlined fonts
  vim.g.nord_uniform_diff_background = 1 -- highlight the foreground instead of background when diffing

  vim.cmd("colorscheme " .. "nord") -- it was important that we set all those vim.g variables before calling this
end
