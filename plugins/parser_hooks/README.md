# How to build a plugin

```
$ make plugin PLUGIN=parser_hooks
```
This command builds plugin for current OS only. Be sure to include `-f Makefile.win` on Windows.

# How to use your plugins in a Haxe project

The `parser_hooks` plugin is meant to be added at initialization using the `--load-plugin` argument.
`$SystemName` is replaced with the operating system name.
```
$ haxe -cp hx --interp --load-plugin cmxs/$SystemName/plugin.cmxs
```
Once it's installed, the compiler changes will be available.
```haxe
static public function main() {
	"Hello, World!" |> trace;
}
```

# How to start a new plugin

To create an initialization or parser plugin, copy this "parser_hooks" directory and replace all occurrences of "parser_hooks" with your own plugin name.

Visit the "example" plugin directory for an example of a plugin that allow for native compiler code to be called in Haxe macros.