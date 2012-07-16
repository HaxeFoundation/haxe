package java.internal;

@:native('haxe.lang.FieldLookup')
@:keep
@:static private class FieldLookup 
{
	
	@:functionBody('
		return s.hashCode();
	')
	public static function hash(s:String):Int
	{
		return 0;
	}
	
	public static function findHash(hash:String, hashs:Array<String>):Int
	{
		var min = 0;
		var max = hashs.length;
		
		while (min < max)
		{
			var mid = Std.int((max + min) / 2); //overflow safe
			var classify = untyped hash.compareTo(hashs[mid]);
			if (classify < 0)
			{
				max = mid;
			} else if (classify > 0) {
				min = mid + 1;
			} else {
				return mid;
			}
		}
		//if not found, return a negative value of where it should be inserted
		return ~min;
	}
	
}