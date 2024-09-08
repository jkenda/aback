local dap = require("dap")

dap.configurations.ocaml = {
    {
        type = "ocaml",
        request = "launch",
        name = "aback",
        program = "${workspaceFolder}/_build/default/bin/main.bc",
        args = { "com", "-i", "core/io.ab" },
        stopOnEntry = true,
        cwd = "${workspaceFolder}",
    }
}
