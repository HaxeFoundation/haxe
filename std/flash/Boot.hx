class Boot {

	public static var _global : Dynamic;
	public static var _root : MovieClip;
	public static var current : MovieClip;

	public static var newObject : Dynamic -> Array<Dynamic> -> Dynamic;

	private static function __init() {
		untyped {
			if( flash == null )
				flash = newObject(null,[]);
			else if( flash.text == null )
				flash.text = newObject(null,[]);
			flash.text.StyleSheet = TextField["StyleSheet"];
			flash.system = newObject(null,[]);
			flash.system.Capabilities = System.capabilities;
			flash.system.Security = System.security;
			flash.system.IME = System["IME"];
			Math.pi = Math["PI"];
			String.prototype.sub = String.prototype.substr;
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
		}
	}

}