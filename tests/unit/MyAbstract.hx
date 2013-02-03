package unit;

abstract MyAbstract(Int) {

    public inline function new(x) {
        this = x;
    }

    public inline function incr() {
        return ++this;
    }

    public inline function toInt() : Int {
        return this;
    }

}

abstract TemplateWrap(haxe.Template) {
	public inline function new(x) {
		this = new haxe.Template(x);
	}

	public inline function get()
		return this

	@:from static inline public function fromString(s:String) {
		return new TemplateWrap(s);
	}

	@:to inline function toString() {
		return this.execute( { t: "really works!"});
	}
}

abstract Meter(Float) from Float to Float {
	public inline function new(f)
		this = f

	public inline function get()
		return this

	@:to public inline function toString()
		return this + "m"
}

abstract Kilometer(Float) from Float to Float {
	public inline function new(f)
		this = f

	@:to public inline function toString()
		return this + "km"

	@:from static public inline function fromMeter(m:Meter)
		return new Kilometer(m.get() / 1000.)
}

abstract MyHash(Hash<V>)<V> {
	private inline function new() {
		this = new Hash<V>();
	}
	public inline function set(k:String, v:V)
		this.set(k, v)
	public inline function get(k:String)
		return this.get(k)
	public inline function toString()
		return this.toString()

	@:from static public function fromStringArray(arr:Array<String>) {
		var hash = new MyHash();
		var i = 0;
		while (i < arr.length) {
			var k = arr[i++];
			var v = arr[i++];
			hash.set(k, v);
		}
		return hash;
	}

	@:from static public function fromArray<K>(arr:Array<K>) {
		var hash = new MyHash();
		var i = 0;
		while (i < arr.length) {
			var k = arr[i++];
			var v = arr[i++];
			hash.set(Std.string('_s$k'), v);
		}
		return hash;
	}
}

class AbstractBase<T> {
	public var value:T;
	public function new(value:T) {
		this.value = value;
	}
}

abstract AbstractZ(AbstractBase<T>)<T> from AbstractBase<T> {
	@:to public static function toFoo(a:AbstractBase<Int>):Int {
		return a.value;
	}

	@:to public static function toString(a:AbstractBase<String>):String {
		return a.value;
	}
}

class MyPoint3 {
	public var x:Float;
	public var y:Float;
	public var z:Float;
	public function new(x, y, z) {
		this.x = x;
		this.y = y;
		this.z = z;
	}
}

abstract MyVector(MyPoint3) from MyPoint3 to MyPoint3 {
	@:op(A + B) static public inline function add(lhs:MyVector, rhs:MyVector):MyVector {
		return new MyPoint3(lhs.x + rhs.x, lhs.y + rhs.y, lhs.z + rhs.z);
	}
	
	@:op(A *= B) static public inline function scalarAssign(lhs:MyVector, rhs:Float):MyVector {
		lhs.x *= rhs;
		lhs.y *= rhs;
		lhs.z *= rhs;
		return lhs;
	}
	
	@:op(A * B) static public inline function scalar(lhs:MyVector, rhs:Float):MyVector {
		return new MyPoint3(lhs.x * rhs, lhs.y * rhs, lhs.z * rhs);
	}
					
	public inline function get():MyPoint3
		return this
		
	@:to public inline function toString():String
		return untyped '(${this.x},${this.y},${this.z})'
}

abstract MyInt(Int) from Int to Int {
	// MyInt + MyInt can be used as is, and returns a MyInt
	@:op(A + B) static public function add(lhs:MyInt, rhs:MyInt):MyInt;
	
	@:commutative @:op(A * B) static public function repeat(lhs:MyInt, rhs:String):String {
		var s:StringBuf = new StringBuf();
		for (i in 0...lhs)
			s.add(rhs);
		return s.toString();
	}
}

class ClassWithHashCode {
	var i:Int;
	public function new(i) { this.i = i; }
	public function hashCode() return i
}