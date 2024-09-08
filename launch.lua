local dap = require("dap")

dap.configurations.ocaml = {
    {
        name = "aback earlybird",
        type = "ocaml",
        request = "launch",
        program = "${workspaceFolder}/_build/default/bin/main.bc",
        args = { "com", "-i", "core/io.ab" },
        stopOnEntry = true,
        cwd = "${workspaceFolder}",
    },
    {
        name = "aback gdb",
        type = "gdb",
        request = "launch",
        program = "${workspaceFolder}/_build/default/bin/main.exe",
        args = { "com", "-i", "core/io.ab" },
        stopOnEntry = true,
        cwd = "${workspaceFolder}",
    }
}
