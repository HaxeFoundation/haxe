
class StringBuf {

	public function new() {
		#neko
		b = __make();
		#else flash
		b = "";
		#else error
		#end
	}

	public function add( x : Dynamic ) {
		#neko
		__add(b,x);
		#else flash
		b += x;
		#else error
		#end
	}

	public function addSub( s : String, pos : Int, len : Int ) {
		#neko
		__add_sub(b,untyped s.__s,pos,len);
		#else flash
		b += s.substr(pos,len);
		#else error
		#end
	}

	public function addChar( c : Int ) {
		#neko
		__add_char(b,c);
		#else flash
		b += untyped String.fromCharCode(c);
		#else error
		#end
	}

	public function toString() : String {
		#neko
		return new String(__string(b));
		#else flash
		return b;
		#else error
		#end
	}

	private var b : Dynamic;

#neko
	static var __make : Dynamic = Neko.load("std","buffer_new",0);
	static var __add : Dynamic = Neko.load("std","buffer_add",2);
	static var __add_char : Dynamic = Neko.load("std","buffer_add_char",2);
	static var __add_sub : Dynamic = Neko.load("std","buffer_add_sub",4);
	static var __string : Dynamic = Neko.load("std","buffer_string",1);
#end

}