class Hash<T> {

	public function new() : Void {
		#neko
		h = untyped __dollar__hnew(0);
		#end
	}

	public function set( key : String, value : T ) : Void {
		#flash
		untyped this[key] = value;
		#else neko
		untyped __dollar__hset(h,key.__s,value,null);
		#end
	}

	public function get( key : String ) : T {
		#flash
		return untyped this[key];
		#else neko
		return untyped __dollar__hget(h,key.__s,null);
		#end
	}

	public function exists( key : String ) : Bool {
		#flash
		return untyped this[key] != null;
		#else neko
		return untyped __dollar__hmem(h,key.__s,null);
		#end
	}

	public function keys() : Iterator<String> {
		#flash
		return untyped (__keys__(this)).iterator();
		#else neko
		var l = new List<String>();
		untyped __dollar__hiter(h,function(k,_) { l.push(new String(k)); });
		return l.iterator();
		#end
	}

	public function iterator() : Iterator<T> {
		#flash
		return untyped({
			ref : this,
			it : keys(),
			hasNext : function() { return this.it.hasNext(); },
			next : function() { var i = this.it.next(); return this.ref[i]; }
		});
		#else neko
		var l = new List<T>();
		untyped __dollar__hiter(h,function(_,v) { l.push(v); });
		return l.iterator();
		#end
	}

	public function toString() {
		var s = new StringBuf();
		s.add("{");
		var it = keys();
		for i in it {
			s.add(i);
			s.add(" => ");
			s.add(get(i));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}

	#flash
	static var __init : Dynamic = untyped _global["ASSetPropFlags"](Hash.prototype,null,1);
	#else neko
	private var h : Dynamic;
	#else error
	#end
}