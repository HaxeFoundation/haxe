package hl;

typedef Symbol = hl.Abstract<"hl_symbol">;

enum TrackKind {
	Alloc;
	Cast;
	DynField;
	DynCall;
}

class Result {
	public var t : hl.Type;
	public var kind : TrackKind;
	public var count : Int;
	public var info : Int;
	public var stack : Array<String>;
	public function new(t, count, info) {
		this.t = t;
		this.count = count;
		this.info = info;
	}
	@:keep public function toString() {
		return t + "(" + count + ")";
	}
}


@:hlNative("std")
class Profile {

	public static var threadBits(get,set) : haxe.EnumFlags<TrackKind>;
	public static var globalBits(get,set) : haxe.EnumFlags<TrackKind>;

	static var KINDS = null;
	public static function getData( sortBySize = false, reset = true ) {
		var old = globalBits;
		globalBits = new haxe.EnumFlags();
		if( buf == null ) buf = new hl.Bytes(BUFSIZE*2);
		track_lock(true);
		var maxDepth = 0;
		var count = track_count(maxDepth);
		var arr = new hl.NativeArray<Symbol>(maxDepth);
		var out = [];
		if( KINDS == null ) KINDS = TrackKind.createAll();
		for( i in 0...count ) {
			var t : hl.Type = null, count = 0, info = 0;
			var k = track_entry(i, t, count, info, arr);
			if( count == 0 ) continue;
			var a = new Result(t, count, info);
			a.kind = KINDS[k];
			a.stack = [for( a in arr ) resolveSymbol(a)];
			out.push(a);
		}
		out.sort(function(a1,a2) {
			if( a1.kind != a2.kind )
				return a1.kind.getIndex() - a2.kind.getIndex();
			if( sortBySize && a1.kind == Alloc )
				return a2.info - a1.info;
			return a2.count - a1.count;
		});
		track_lock(false);
		if( reset ) Profile.reset();
		globalBits = old;
		return out;
	}

	public static function dump( fileName = "track.dump", sortBySize = false, reset = true ) {
		var old = globalBits;
		globalBits = new haxe.EnumFlags();
		var f = sys.io.File.write(fileName);
		var data = getData(sortBySize, reset);
		var allocCount = 0, allocSize = 0, castCount = 0, dynCount = 0;
		for( o in data ) {
			switch( o.kind ) {
			case Alloc:
				allocCount += o.count;
				allocSize += o.info;
			case Cast:
				castCount += o.count;
			case DynCall, DynField:
				dynCount += o.count;
			}
		}
		if( data.length == 0 )
			f.writeString("Nothing\n");
		if( allocCount > 0 )
			f.writeString(allocCount +" total allocs (" + allocSize+" bytes)\n");
		if( castCount > 0 )
			f.writeString(castCount+" total casts\n");
		if( dynCount > 0 )
			f.writeString(dynCount+" total dynamic accesses/calls\n");
		for( o in data ) {
			var pcount = StringTools.lpad(""+o.count," ",5);
			switch( o.kind ) {
			case Alloc:
				f.writeString("alloc    "+pcount+" "+o.t + " (" + o.info + " bytes)\n");
			case Cast:
				f.writeString("cast     "+pcount+" "+o.t + "\n");
			case DynCall:
				f.writeString("dyncall  "+pcount+" "+o.t+"."+getFieldName(o.info)+"()\n");
			case DynField:
				f.writeString("dynfield "+pcount+" "+o.t+"."+getFieldName(o.info)+"\n");
			}
			for( s in o.stack )
				f.writeString("\t\t\t\t" + s + "\n");
		}
		f.close();
		globalBits = old;
	}

	/**
		Reset accumulated tracked data.
	**/
	@:hlNative("std","track_reset") public static function reset() {
	}

	/**
		Restart tracking after being stopped.
	**/
	@:hlNative("std","track_init") public static function restart() {
	}

	/**
		Stop tracking for all threads.
	**/
	public static function stop() {
		globalBits = new haxe.EnumFlags();
	}

	/**
		Set maximum stack depth for reports (default = 10)
	**/
	@:hlNative("std","track_set_depth") public static function setMaxDepth( v : Int ) {
	}

	static function get_threadBits() return new haxe.EnumFlags(track_get_bits(true));
	static function set_threadBits(v:haxe.EnumFlags<TrackKind>) { track_set_bits(v.toInt(),true); return v; }
	static function get_globalBits() return new haxe.EnumFlags(track_get_bits(false));
	static function set_globalBits(v:haxe.EnumFlags<TrackKind>) { track_set_bits(v.toInt(),false); return v; }

	static var BUFSIZE = 512;
	static var buf : hl.Bytes;
	static function resolveSymbol( s : Symbol ) {
		var size = BUFSIZE;
		if( buf == null ) buf = new hl.Bytes(BUFSIZE*2);
		var bytes = resolve_symbol(s, buf, size);
		if( bytes == null ) return "<???>";
		return @:privateAccess String.fromUCS2(bytes.sub(0,(size+1)*2));
	}

	static function resolve_symbol( s : Symbol, buf : hl.Bytes, bufSize : hl.Ref<Int> ) : hl.Bytes { return null; }
	static function track_count( maxDepth : hl.Ref<Int> ) : Int { return 0; }
	static function track_entry( id : Int, type : hl.Ref<hl.Type>, count : hl.Ref<Int>, info : hl.Ref<Int>, stack : NativeArray<Symbol> ) : Int { return 0; }
	static function track_init() : Void {}
	static function track_lock(b:Bool) : Void {}
	static function track_get_bits(thread:Bool) : Int { return 0; }
	static function track_set_bits(bits:Int,thread:Bool) : Void {}
	static function __init__() track_init();

	// todo : move later to hl.Bytes
	@:hlNative("std","field_name")
	public static function getFieldName( hash : Int ) : Bytes {
		return null;
	}

}
