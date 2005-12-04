class Boot {

	public static var _global : Dynamic;
	public static var _root : MovieClip;
	public static var current : MovieClip;

	private static function __closure(f,o) {
		untyped {
			var f2 = function() {
				var me = __arguments__.callee;
				return me.f.apply(me.o,__arguments__);
			};
			f2.f = o[f];
			f2.o = o;
			return f2;
		}
	}

	private static function __instanceof(o,cl) {
		untyped {
			if( __instanceof__(o,cl) )
				return true;
			switch( cl ) {
			case Int:
				return Math.ceil(o) == o; // error with NaN
			case Float:
				return __typeof__(o) == "number";
			case Bool:
				return (o == true || o == false);
			case String:
				return __typeof__(o) == "string";
			default:
				return false;
			}
		}
	}

	private static function __init() {
		untyped {
			var obj = _global["Object"];
			if( flash == null )
				flash = __new__(obj);
			else if( flash.text == null )
				flash.text = __new__(obj);
			flash.text.StyleSheet = TextField["StyleSheet"];
			flash.system = __new__(obj);
			flash.system.Capabilities = System.capabilities;
			flash.system.Security = System.security;
			Math.pi = Math["PI"];

			#use_ime
			flash.system.IME = System["IME"];
			flash.system.IME._ALPHANUMERIC_FULL = System["IME"]["ALPHANUMERIC_FULL"];
			flash.system.IME._ALPHANUMERIC_HALF = System["IME"]["ALPHANUMERIC_HALF"];
			flash.system.IME._CHINESE = System["IME"]["CHINESE"];
			flash.system.IME._JAPANESE_HIRAGANA = System["IME"]["JAPANESE_HIRAGANA"];
			flash.system.IME._JAPANESE_KATAKANA_FULL = System["IME"]["JAPANESE_KATAKANA_FULL"];
			flash.system.IME._JAPANESE_KATAKANA_HALF = System["IME"]["JAPANESE_KATAKANA_HALF"];
			flash.system.IME._KOREAN = System["IME"]["KOREAN"];
			flash.system.IME._UNKNOWN = System["IME"]["UNKNOWN"];
			#end

			Node = _global["XMLNode"];
			Node.element_node = 1;
			Node.text_node = 3;
			Node.prototype.removeChild = Node.prototype.removeNode;
			Node.prototype.replaceChild = function(cnew,cold) {
				this.insertBefore(cnew,cold);
				this.removeChild(cold);
			};

			Array.prototype.copy = Array.prototype.slice;
			Array.prototype.insert = function(i,x) {
				this.splice(i,0,x);
			};
			Array.prototype.remove = function(obj) {
				var i = 0;
				var l = this.length;
				while( i < l ) {
					if( this[i] == obj ) {
						this.splice(i,1);
						return true;
					}
					i++;
				}
				return false;
			}
			Array.prototype.iterator = function() {
				return {
					cur : 0,
					max : this.length,
					arr : this,
					hasNext : function() {
						return this.cur < this.max;
					},
					next : function() {
						return this.arr[this.cur++];
					}
				}
			};
			Array.prototype.indexes = function() {
				return {
					cur : 0,
					max : this.length,
					hasNext : function() {
						return this.cur < this.max;
					},
					next : function() {
						return this.cur++;
					}
				}
			};

			Int = __new__(obj);
			Float = _global["Number"];
			// prevent closure creation by setting untyped
			current["@instanceof"] = untyped __instanceof;
			current["@closure"] = untyped __closure;
		}
	}

}