### testing setup

```
vim.lsp.set_log_level 'debug'

vim.filetype.add {
  extension = {
    jazz = 'jazz',
  },
}

local client = vim.lsp.start_client {
  name = 'jasmin-lsp',
  cmd = { '/home/user/jasmin-lsp/_build/install/default/bin/jasmin_lsp' },
  on_attach = vim.notify 'hey',
}

if not client then
  vim.notify 'client error'
end

vim.api.nvim_create_autocmd('FileType', {
  pattern = 'jazz',
  callback = function()
    vim.lsp.buf_attach_client(0, client)
  end,
})
```
