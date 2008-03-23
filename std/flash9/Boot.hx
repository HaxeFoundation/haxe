package flash;

class Boot extends flash.display.MovieClip {

	#if (!flash9doc)

	static var init : Void -> Void;
	static var tf : flash.text.TextField;
	static var lines : Array<String>;

	#if !as3gen
	static var __res : flash.utils.Dictionary;
	#end

	public static var skip_constructor = false;

	public function new(?mc:flash.display.MovieClip) {
		super();
		untyped {
			var aproto = Array.prototype;
			aproto.copy = function() {
				return this.slice();
			};
			aproto.insert = function(i,x) {
				this.splice(i,0,x);
			};
			aproto.remove = function(obj) {
				for( i in 0...this.length )
					if( this[i] == obj ) {
						this.splice(i,1);
						return true;
					}
				return false;
			}
			aproto.iterator = function() {
				return {
					cur : 0,
					arr : this,
					hasNext : function() {
						return this.cur < this.arr.length;
					},
					next : function() {
						return this.arr[this.cur++];
					}
				}
			};
			aproto.setPropertyIsEnumerable("copy", false);
			aproto.setPropertyIsEnumerable("insert", false);
			aproto.setPropertyIsEnumerable("remove", false);
			aproto.setPropertyIsEnumerable("iterator", false);
			#if !as3gen
			Bool = __global__["Boolean"];
			Int = __global__["int"];
			Float = __global__["Number"];
			Dynamic = { toString : function(){ return "Dynamic"; } };
			#end
			var cca = String.prototype.charCodeAt;
			String.prototype.charCodeAt = function(i) {
				var x = cca.call(this,i);
				if( __global__["isNaN"](x) )
					return null;
				return x;
			};
		}
		lines = new Array();
		var c = if( mc == null ) this else mc;
		flash.Lib.current = c;
		try {
			untyped if( c.stage != null && c.stage.align == "" )
				c.stage.align = "TOP_LEFT";
		} catch( e : Dynamic ) {
			// security error when loading from different domain
		}
		if( init != null )
			init();
	}

	public static function enum_to_string( e ) {
		if( e.params == null )
			return e.tag;
		return e.tag+"("+e.params.join(",")+")";
	}

	public static function __instanceof( v : Dynamic, t : Dynamic ) {
		try {
			if( t === untyped __global__["Dynamic"] )
				return true;
			return untyped __is__(v,t);
		} catch( e : Dynamic ) {
		}
		return false;
	}

	public static function __clear_trace() {
		if( tf == null )
			return;
		flash.Lib.current.removeChild(tf);
		tf = null;
		lines = new Array();
	}

	public static function __set_trace_color(rgb) {
		getTrace().textColor = rgb;
	}

	public static function getTrace() {
		var mc = flash.Lib.current;
		if( tf == null ) {
			tf = new flash.text.TextField();
			var format = tf.getTextFormat();
			format.font = "_sans";
			tf.defaultTextFormat = format;
			tf.selectable = false;
			tf.width = if( mc.stage == null ) 800 else mc.stage.stageWidth;
			tf.autoSize = flash.text.TextFieldAutoSize.LEFT;
			tf.mouseEnabled = false;
		}
		mc.addChild(tf); // on top
		return tf;
	}

	public static function __trace( v : Dynamic, pos : haxe.PosInfos ) {
		var tf = getTrace();
		var pstr = if( pos == null ) "(null)" else pos.fileName+":"+pos.lineNumber;
		lines = lines.concat((pstr +": "+__string_rec(v,"")).split("\n"));
		tf.text = lines.join("\n");
		var stage = flash.Lib.current.stage;
		if( stage == null )
			return;
		while( tf.height > stage.stageHeight ) {
			lines.shift();
			tf.text = lines.join("\n");
		}
	}

	public static function __string_rec( v : Dynamic, str : String ) {
		var cname = untyped __global__["flash.utils.getQualifiedClassName"](v);
		switch( cname ) {
		case "Object":
			var k : Array<String> = untyped __keys__(v);
			var s = "{";
			var first = true;
			for( i in 0...k.length ) {
				var key = k[i];
				if( first )
					first = false;
				else
					s += ",";
				s += " "+key+" : "+__string_rec(v[untyped key],str);
			}
			if( !first )
				s += " ";
			s += "}";
			return s;
		case "Array":
			var s = "[";
			var i;
			var first = true;
			for( i in 0...v.length ) {
				if( first )
					first = false;
				else
					s += ",";
				s += __string_rec(v[i],str);
			}
			return s+"]";
		default:
			switch( untyped __typeof__(v) ) {
			case "function": return "<function>";
			}
		}
		return new String(v);
	}

	#end

}
