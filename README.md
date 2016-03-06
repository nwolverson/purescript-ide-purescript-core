# purescript-ide-purescript-core

Common PureScript code for the core implementation of PureScript editor plugins for JavaScript-based editors,
i.e. [atom-ide-purescript](https://github.com/nwolverson/atom-ide-purescript) and 
[vscode-ide-purescript](https://github.com/nwolverson/vscode-ide-purescript).

Makes use of [purescript-ide-psc](https://github.com/kRITZCREEK/purescript-psc-ide) to launch and 
connect to `psc-ide-server`, which provides editor services (based on PureScript compiler API and 
particularly compiler "externs" files) supporting completion, type info, search, pursuit, etc. 

Also wraps the PureScript compiler JSON output.
