class Hash<T> {

	public function new() : Void {
	}

	public function set( key : String, value : T ) : Void {
		#flash
		untyped this[key] = value;
		#end
	}

	public function get( key : String ) : T {
		#flash
		return untyped this[key];
		#end
	}

	public function exists( key : String ) : Bool {
		#flash
		return untyped this[key] != null;
		#end
	}

	public function keys() : Iterator<String> {
		#flash
		return untyped (__keys__(this)).iterator();
		#end
	}

	public function iterator() : Iterator<T> {
		return untyped({
			ref : this,
			it : keys(),
			hasNext : function() { return this.it.hasNext(); },
			next : function() { var i = this.it.next(); return this.ref[i]; }
		});
	}

	#flash
	static var __init : Dynamic = untyped _global["ASSetPropFlags"](Hash.prototype,null,1);
	#end
}