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

#if !flash8
abstract TemplateWrap(haxe.Template) {
	public inline function new(x) {
		this = new haxe.Template(x);
	}

	public inline function get()
		return this;

	@:from static inline public function fromString(s:String) {
		return new TemplateWrap(s);
	}

	@:to inline function toString() {
		return this.execute( { t: "really works!"});
	}
}
#end

abstract Meter(Float) from Float to Float {
	public inline function new(f)
		this = f;

	public inline function get()
		return this;

	@:to public inline function toString()
		return this + "m";
}

abstract Kilometer(Float) from Float to Float {
	public inline function new(f)
		this = f;

	@:to public inline function toString()
		return this + "km";

	@:from static public inline function fromMeter(m:Meter)
		return new Kilometer(m.get() / 1000.);
}


class MyClassWithAbstractArgCtor {
	public var km:Kilometer;
	public function new(km:Kilometer) {
		this.km = km;
	}
}

abstract MyHash<V>(haxe.ds.StringMap<V>) {
	private inline function new() {
		this = new haxe.ds.StringMap<V>();
	}
	public inline function set(k:String, v:V)
		this.set(k, v);
	public inline function get(k:String)
		return this.get(k);
	public inline function toString()
		return this.toString();

	@:from static public function fromStringArray(arr:Array<String>):MyHash<String> {
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

abstract AbstractZ<T>(AbstractBase<T>) from AbstractBase<T> {
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
	var x(get, set):Float;
	var y(get, set):Float;
	var z(get, set):Float;

	public function get_x() return this.x;
	public function get_y() return this.y;
	public function get_z() return this.z;
	public function set_x(x) return this.x = x;
	public function set_y(y) return this.y = y;
	public function set_z(z) return this.z = z;
	
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
	
	@:op(-A) static public inline function invert(t:MyVector):MyVector {
		return new MyPoint3( -t.x, -t.y, -t.z);
	}
	
	public inline function get():MyPoint3
		return this;

	@:to public inline function toString():String
		return untyped '(${this.x},${this.y},${this.z})';
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
	
	@:op(A / B) static public function cut(lhs:String, rhs:MyInt) {
		return lhs.substr(0, rhs);
	}
}

abstract MyInt2(Int){
	public inline function new(v) {
		this = v;
	}
	
	public function get():Int {
		return this;
	}
	
	@:op(-x) public inline function invert():MyInt2 {
		return new MyInt2(-this);
	}
	
	@:op(++x) public inline function incr() {
		++this;
	}
}

abstract MyString(String) from String to String {
	@:op(A + B) static public function add(lhs:MyString, rhs:MyString):MyString;
	@:op(A + B) static public function addInt(lhs:MyString, rhs:Int):MyString;
	@:op(A + B) static public function addBool(lhs:MyString, rhs:Bool):Bool;
	@:op(A - B) static public function sub(lhs:MyString, rhs:MyString):MyString;
}

class ClassWithHashCode {
	var i:Int;
	public function new(i) { this.i = i; }
	public function hashCode() return i;
}

class ClassWithoutHashCode {
	public var i:Int;
	public function new(i) { this.i = i; }
}

abstract MyReflect({}) from {} {
	@:arrayAccess public inline function arrayAccess(key:String):Dynamic {
		return Reflect.field(this, key);
	}
	
	@:arrayAccess public inline function arrayWrite<T>(key:String, value:T):T {
		Reflect.setField(this, key, value);
		return value;
	}
}

abstract MyAbstractClosure(String){
	public function new(value:String) {
		this = value;
	}
	
	public function test() {
		var fn = function(){
			return this;
		}
		return fn;
	}
	
	public inline function setVal(v) {
		this = v;
	}
}

abstract MyAbstractSetter(Dynamic) {
	
	public var value(get,set):String;
	
	public inline function new() {
		this = {};
	}
	
	inline function get_value() {
		return this.value;
	}
	
	inline function set_value(s:String) {
		this.value = s;
		return s;
	}
}

abstract MyAbstractCounter(Int) {
	public static var counter = 0;
	inline function new(v:Int) {
		this = v;
		counter++;
	}

	@:from inline static public function fromInt(v:Int) {
		return new MyAbstractCounter(v);
	}
	
	inline public function getValue():Int return this + 1;
}

abstract MyAbstractThatCallsAMember(Int) to Int {
	public function new(i) {
		this = i;
		bar();
	}

	inline function bar() this++;
}

abstract MyDebugString(String) to String {
	public inline function new(s:String) {
		this = s;
	}
	
	public inline function substr(i:Int, ?len:Null<Int>) {
		return this.substr(i);
	}
}

@:multiType abstract MySpecialString(String) {
	public function new(value:String);	

	public inline function substr(i:Int, ?len:Int) {
		return len == null ? this.substr(i) : this.substr(i, len);
	}
	
	@:to static inline public function toNormal(t:String, value:String) {
		return new MyDebugString(value);
	}	
}