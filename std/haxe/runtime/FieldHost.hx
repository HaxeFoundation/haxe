package haxe.runtime;

@:transitive
abstract FieldHost(Dynamic) from {}
	from Dynamic<Dynamic> // #if (jvm || neko || js || lua) // can't do this because of docgen
	from Class<Dynamic> // #end
{}
