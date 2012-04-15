package haxe.lang;

/**
 * ...
 * @author waneck
 */

@:struct @:nativegen @:native("haxe.lang.Null") private class Nullable<T>
{
	
	@:readonly public var value:T;
	@:readonly public var hasValue:Bool;
	
	@:functionBody('
			if (! (v is System.ValueType))
			{
				if (v.Equals(default(T)))
				{
					hasValue = false;
				}
			}
			
			if (!v.Equals(default(T)))
			{
				hasValue = true;
			}
			
			this.@value = v;
			this.hasValue = hasValue;
	')
	public function new(v:T, hasValue:Bool)
	{
		this.value = v;
		this.hasValue = hasValue;
	}
	
	public static function ofDynamic<T>(obj:Dynamic):Nullable<T>
	{
		if (obj == null)
		{
			return new Nullable<T>(null, false);
		} else {
			return new Nullable<T>(obj, true);
		}
	}
	
	public function toDynamic():Dynamic
	{
		if (hasValue) 
			return value;
		return null;
	}
}