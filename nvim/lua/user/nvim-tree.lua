local status_ok, nvim_tree = pcall(require, "nvim-tree")
if not status_ok then
  return
end

nvim_tree.setup({
  sort_by = "case_sensitive",
  view = {
    adaptive_size = true,
    mappings = {
      list = {
        { key = "u", action = "dir_up" },
        { key = "<C-x>", action = "" },
        { key = "s", action = "split" },
        { key = "<C-v>", action = "" },
        { key = "v", action = "vsplit" },
        { key = "<C-t>", action = "" },
        { key = "t", action = "tabnew" },
      },
    },
  },
  renderer = {
    group_empty = true,
  },
})

local opts = { noremap = true, silent = true } -- options for all key bindings
local keymap = vim.api.nvim_set_keymap -- shorten vim.api.nvim_set_keymap to keymap

keymap("n", "<leader>;", ":NvimTreeToggle<CR>", opts) -- toggle nvim-tree
