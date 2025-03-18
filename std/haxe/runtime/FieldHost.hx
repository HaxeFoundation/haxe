package haxe.runtime;

@:transitive
abstract FieldHost(Dynamic) from {}
	from Dynamic<Dynamic>
	#if (jvm || neko || js) from Class<Dynamic>#end {}
