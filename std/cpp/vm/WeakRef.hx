package cpp.vm;


class WeakRef<T>
{
   var ref:Dynamic;
   var hardRef:Bool;


	public function new(inObject:T, inHard:Bool = false)
	{
		hardRef = inHard;
		if (hardRef)
         ref = inObject;
      else
			ref = untyped __global__.__hxcpp_weak_ref_create(inObject);
	}
	
	
	public function get():T
	{
		if (hardRef)
			return ref;
		
		return  untyped __global__.__hxcpp_weak_ref_get(ref);
	}
	
	
	public function toString():String
	{
		if (hardRef)
			return "" + hardRef;
		
		return "WeakRef(" + ref + ")";
	}
}

