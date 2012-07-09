package cs.internal;

@:classContents('
	//This function is here to be used with Reflection, when the haxe.lang.Null type is known
	public static haxe.lang.Null<T> _ofDynamic(object obj)
	{
		if (obj == null)
		{
			return new haxe.lang.Null<T>(default(T), false);
		} else if (typeof(T).Equals(typeof(double))) {
			return new haxe.lang.Null<T>((T) (object) haxe.lang.Runtime.toDouble(obj), true);
		} else if (typeof(T).Equals(typeof(int))) {
			return new haxe.lang.Null<T>((T) (object) haxe.lang.Runtime.toInt(obj), true);
		} else {
			return new haxe.lang.Null<T>((T) obj, true);
		}
	}
')
@:keep @:struct @:nativegen @:native("haxe.lang.Null") private class Nullable<T>
{
	
	@:readonly public var value:T;
	@:readonly public var hasValue:Bool;
	
	@:functionBody('
			if ( !(v is System.ValueType) && System.Object.ReferenceEquals(v, default(T)))
			{
				hasValue = false;
			}
			
			this.@value = v;
			this.hasValue = hasValue;
	')
	public function new(v:T, hasValue:Bool)
	{
		this.value = v;
		this.hasValue = hasValue;
	}
	
	@:functionBody('
		if (obj == null)
		{
			return new haxe.lang.Null<D>(default(D), false);
		} else if (typeof(D).Equals(typeof(double))) {
			return new haxe.lang.Null<D>((D) (object) haxe.lang.Runtime.toDouble(obj), true);
		} else if (typeof(D).Equals(typeof(int))) {
			return new haxe.lang.Null<D>((D) (object) haxe.lang.Runtime.toInt(obj), true);
		} else {
			return new haxe.lang.Null<D>((D) obj, true);
		}
	')
	public static function ofDynamic<D>(obj:Dynamic):Nullable<D>
	{
		return null;
	}
	
	@:functionBody('
		if (this.hasValue)
			return value;
		return null;
	')
	public function toDynamic():Dynamic
	{
		return null;
	}
}