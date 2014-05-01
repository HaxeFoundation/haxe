package python.lib;

import python.lib.Dict;

@:native("object")
extern class Object {
	public var __dict__:Dict<String, Dynamic>;
}