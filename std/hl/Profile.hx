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

	public static var enable(get, set) : Bool;

	public static function getData() {
		var old = enable;
		enable = false;
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
		out.sort(function(a1, a2) return a2.count - a1.count);
		enable = old;
		return out;
	}

	public static function dump( fileName = "alloc.dump" ) {
		var d = getData();
		var old = enable;
		enable = false;
		var f = sys.io.File.write(fileName);
		var data = getData();
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

	static var BUFSIZE = 512;
	static var buf = new hl.Bytes(BUFSIZE*2);
	static function resolveSymbol( s : Symbol ) {
		var size = BUFSIZE;
		var bytes = resolve_symbol(s, buf, size);
		if( bytes == null ) return "<???>";
		return @:privateAccess String.fromUCS2(bytes.sub(0,(size+1)*2));
	}

	static function get_enable() return Gc.flags.has(Track);
	static function set_enable(v) {
		var flags = Gc.flags;
		if( v ) flags.set(Track) else flags.unset(Track);
		Gc.flags = flags;
		return v;
	}

	static function resolve_symbol( s : Symbol, buf : hl.Bytes, bufSize : hl.Ref<Int> ) : hl.Bytes { return null; }
	static function track_count( maxDepth : hl.Ref<Int> ) : Int { return 0; }
	static function track_entry( id : Int, type : hl.Ref<hl.Type>, count : hl.Ref<Int>, size : hl.Ref<Int>, stack : NativeArray<Symbol> ) : Void {}
	static function track_init() : Void {}
	static function __init__() track_init();

}
