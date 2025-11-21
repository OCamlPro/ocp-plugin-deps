
[![Actions Status](https://github.com/ocamlpro/ocp-plugin-deps/workflows/Main%20Workflow/badge.svg)](https://github.com/ocamlpro/ocp-plugin-deps/actions)
[![Release](https://img.shields.io/github/release/ocamlpro/ocp-plugin-deps.svg)](https://github.com/ocamlpro/ocp-plugin-deps/releases)

# ocp-plugin-deps

ocp-plugin-deps is a basic tool that helps finding dependencies of
plugins, using META files.

It is called as follows:

```
ocp-plugin-deps PLUGIN [STATIC-LIBRARIES] [-I search-dir]
```

The `PLUGIN` parameter is the name of plugin you are interested in.
`STATIC-LIBRARIES` list some of the top libraries that are already in
the executable and won't be loaded.

`ocp-plugin-deps` will then scan the META files, sort the
dependencies, and list the dependencies that should be loaded prior to
loading PLUGIN.

For every dependency, `ocp-plugin-deps` will display where to find the
corresponding .cmxs file (if not displayed, then it does not exist).

* Website: https://ocamlpro.github.io/ocp-plugin-deps
* General Documentation: https://ocamlpro.github.io/ocp-plugin-deps/sphinx
* API Documentation: https://ocamlpro.github.io/ocp-plugin-deps/doc
* Sources: https://github.com/ocamlpro/ocp-plugin-deps


