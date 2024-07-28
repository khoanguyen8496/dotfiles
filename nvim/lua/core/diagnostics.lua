-- Diagnostics
vim.diagnostic.config({
    float = {
        source = "always",
        border = "rounded",
        severity_sort = true,
    },
    virtual_text = {
        severity = { min = vim.diagnostic.severity.W },
        source = "if_many",
    },
    severity_sort = true,
})
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Previous diagnostic" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Next diagnostic" })
-- local icons = {
--     Error = "󰅚 ",
--     Warn = "󰀪 ",
--     Info = "•",
--     Hint = "•",
-- }
-- for _, lvl in ipairs({ "Error", "Warn", "Info", "Hint" }) do
--     local hl = "DiagnosticSign" .. lvl
--     local highlight_lnum = lvl == "Error" or lvl == "Warn"
--     vim.fn.sign_define(hl, { text = icons[lvl], texthl = hl, linehl = "", numhl = highlight_lnum and hl or "" })
-- end
