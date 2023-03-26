-- https://github.com/jpalardy/vim-slime
-- :help slime

vim.g.slime_target = "neovim" -- send selected text to a neovim terminal
vim.g.slime_paste_file = vim.env.XDG_RUNTIME_DIR .. "/slime" -- store runtime file for the slime in a temp directory

vim.cmd [[command JobID :echo b:terminal_job_id]] -- get job id for terminal window with :JobID

vim.keymap.set("v", "<leader>i", "<Plug>SlimeRegionSend", {
  noremap = true,
  silent = true,
})
