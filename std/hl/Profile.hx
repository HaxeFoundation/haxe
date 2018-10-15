package hl;

typedef Symbol = hl.Abstract<"hl_symbol">;

class Allocation {
	public var t : hl.Type;
	public var count : Int;
	public var size : Int;
	public var stack : Array<String>;
	public function new(t, count, size) {
		this.t = t;
		this.count = count;
		this.size = size;
	}
	@:keep public function toString() {
		return t + "(" + count + ")";
	}
}

@:hlNative("std")
class Profile {

	/**
		Enable allocation tracking per thread. All threads are enabled by default.
	**/
	public static var enable(get, set) : Bool;

	public static function getData( sortBySize = false ) {
		var old = enable;
		enable = false;
		track_lock(true);
		var maxDepth = 0;
		var count = track_count(maxDepth);
		var arr = new hl.NativeArray<Symbol>(maxDepth);
		var out = [];
		for( i in 0...count ) {
			var t : hl.Type = null, count = 0, size = 0;
			track_entry(i, t, count, size, arr);
			if( count == 0 ) continue;
			var a = new Allocation(t, count, size);
			a.stack = [for( a in arr ) resolveSymbol(a)];
			out.push(a);
		}
		if( sortBySize )
			out.sort(function(a1, a2) return a2.size - a1.size);
		else
			out.sort(function(a1, a2) return a2.count - a1.count);
		track_lock(false);
		enable = old;
		return out;
	}

	public static function dump( fileName = "alloc.dump", sortBySize = false ) {
		var old = enable;
		enable = false;
		var f = sys.io.File.write(fileName);
		var data = getData(sortBySize);
		var count = 0, size = 0;
		for( o in data ) {
			count += o.count;
			size += o.size;
		}
		f.writeString(count +" total allocs (" + size+" bytes)\n");
		for( o in data ) {
			f.writeString(o.count+" "+o.t + " (" + o.size + " bytes)\n");
			for( s in o.stack )
				f.writeString("\t" + s + "\n");
		}
		f.close();
		enable = old;
	}

	/**
		Reset accumulated tracked data.
	**/
	@:hlNative("std","track_reset") public static function reset() {
	}

	/**
		Start tracking. Enabled by default.
	**/
	@:hlNative("std","track_init") public static function start() {
	}

	/**
		Stop tracking for all threads.
	**/
	@:hlNative("std","track_stop") public static function stop() {
	}

	static var BUFSIZE = 512;
	static var buf = new hl.Bytes(BUFSIZE*2);
	static function resolveSymbol( s : Symbol ) {
		var size = BUFSIZE;
		var bytes = resolve_symbol(s, buf, size);
		if( bytes == null ) return "<???>";
		return @:privateAccess String.fromUCS2(bytes.sub(0,(size+1)*2));
	}

	static function get_enable() return track_enabled();
	static function set_enable(v) {
		track_enable(v);
		return v;
	}

	static function resolve_symbol( s : Symbol, buf : hl.Bytes, bufSize : hl.Ref<Int> ) : hl.Bytes { return null; }
	static function track_count( maxDepth : hl.Ref<Int> ) : Int { return 0; }
	static function track_entry( id : Int, type : hl.Ref<hl.Type>, count : hl.Ref<Int>, size : hl.Ref<Int>, stack : NativeArray<Symbol> ) : Void {}
	static function track_enable(b:Bool) : Void {}
	static function track_lock(b:Bool) : Void {}
	static function track_enabled() : Bool { return false; }
	static function __init__() { start(); track_enable(true); }

}
