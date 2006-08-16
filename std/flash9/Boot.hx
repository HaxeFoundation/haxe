package flash;

class Boot extends flash.display.MovieClip {

	static var init : Void -> Void;
	static var tf : flash.text.TextField;
	static var lines : Array<String>;

	function new() {
		super();
		untyped {
			var aproto = Array.prototype;
			aproto.copy = aproto.slice;
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
			Bool = __global__["Boolean"];
			Int = __global__["int"];
			Float = __global__["Number"];
			Dynamic = { toString : function(){ return "Dynamic"; } };
			var cca = String.prototype.charCodeAt;
			String.prototype.charCodeAt = function(i) {
				var x = cca.call(this,i);
				if( __global__["isNaN"](x) )
					return null;
				return x;
			};
		}
		lines = new Array();
		flash.Lib.current = this;
		init();
	}

	static function enum_to_string( e ) {
		if( e.params == null )
			return e.tag;
		return e.tag+"("+e.params.join(",")+")";
	}

	static function __instanceof( v : Dynamic, t : Dynamic ) {
		try {
			if( t === untyped __global__["Dynamic"] )
				return true;
			return untyped __is__(v,t);
		} catch( e : Dynamic ) {
			return false;
		}
	}

	static function __clear_trace() {
		flash.Lib.current.removeChild(tf);
		tf = null;
		lines = new Array();
	}

	static function __trace( v : Dynamic, pos : haxe.PosInfos ) {
		var mc = flash.Lib.current;
		if( tf == null ) {
			tf = new flash.text.TextField();
			tf.selectable = false;
			tf.width = mc.stage.stageWidth;
			tf.autoSize = flash.text.TextFieldAutoSize.LEFT;
		}
		mc.addChild(tf); // on top
		var pstr = if( pos == null ) "(null)" else pos.fileName+":"+pos.lineNumber;
		lines = lines.concat((pstr +": "+__string_rec(v,"")).split("\n"));
		tf.text = lines.join("\n");
		while( tf.height > mc.stage.stageHeight ) {
			lines.shift();
			tf.text = lines.join("\n");
		}
	}

	static function __string_rec( v : Dynamic, str : String ) {
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

}