package haxe.lang;

@:native('haxe.lang.FieldLookup')
@:static private class FieldLookup 
{

	@:private private static var fieldIds:Array<Int>;
	@:private private static var fields:Array<String>;
	
	//s cannot be null here
	private static inline function doHash(s:String):Int
	{
		var acc = 0; //alloc_int
		for (i in 0...s.length)
		{
			acc = (( 223 * (acc >> 1) + s.charCodeAt(i) ) << 1);
		}
		
		return acc >>> 1; //always positive
	}
	
	public static function lookupHash(key:Int):String
	{
		//start of binary search algorithm
		var ids = fieldIds;
		var min = 0;
		var max = ids.length;
		
		while (min < max)
		{
			var mid = Std.int(min + (max - min) / 2); //overflow safe
			var imid = ids[mid];
			if (key < imid)
			{
				max = mid;
			} else if (key > imid) {
				min = mid + 1;
			} else {
				return fields[mid];
			}
		}
		//if not found, it's definately an error
		throw "Field not found for hash " + key;
	}
	
	public static function hash(s:String):Int
	{
		if (s == null) return 0;
		
		var key = doHash(s);
		
		//start of binary search algorithm
		var ids = fieldIds;
		var min = 0;
		var max = ids.length;
		
		while (min < max)
		{
			var mid = Std.int(min + (max - min) / 2); //overflow safe
			var imid = ids[mid];
			if (key < imid)
			{
				max = mid;
			} else if (key > imid) {
				min = mid + 1;
			} else {
				var field = fields[mid];
				if (field != s)
					return -(key + 1); //special case
				return key;
			}
		}
		//if not found, min holds the value where we should insert the key
		ids.insert(min, key);
		fields.insert(min, s);
		return key;
	}
	
	public static function findHash(hash:Int, hashs:Array<Int>):Int
	{
		var min = 0;
		var max = hashs.length;
		
		while (min < max)
		{
			var mid = Std.int((max + min) / 2); //overflow safe
			var imid = hashs[mid];
			if (hash < imid)
			{
				max = mid;
			} else if (hash > imid) {
				min = mid + 1;
			} else {
				return min;
			}
		}
		//if not found, return a negative value of where it should be inserted
		return ~min;
	}
	
}