# How to build a plugin

```
$ make plugin PLUGIN=example
```
This command builds plugin for current OS only. Be sure to include `-f Makefile.win` on Windows.

# How to use your plugins in a Haxe project

Setup your plugin as a haxe library:
```
$ haxelib dev example path/to/haxe/plugins/example
```
And then access it inside of a macro:
```haxe
macro static public function testPlugin() {
	Example.plugin.hello();
	return macro {}
}
```

# How to start a new plugin

Just make a copy of an "example" plugin directory and replace all occurrences of "example" word with your own plugin name.