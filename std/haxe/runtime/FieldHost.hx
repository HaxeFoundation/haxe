package haxe.runtime;

@:transitive
abstract FieldHost(Dynamic) from {}
	from Dynamic<Dynamic>
	from Class<Dynamic>
{
	public inline function asArrayAccess():ArrayAccess<Dynamic> {
		return cast this;
	}

	public inline function asStructure():{} {
		return cast this;
	}

	public inline function asDynamic():Dynamic {
		return cast this;
	}

	#if (neko || js || flash || python || lua)
	@:from static public inline function fromEnum<T>(en:Enum<T>):FieldHost {
		return cast en;
	}
	#end
}
