import cs.Constraints;
import haxe.Constraints.Constructible;

@:classCode("
	public static void testClass<T>(T t) where T : class {}
	public static void testStruct<T>(T t) where T : struct {}
	public static void testConstructible<T>(T t) where T : new() {}
	public static void testConstructibleClass<T>(T t) where T : class, new() {}
")
class TestCs {
    extern public static function testClass<T:CsClass>(t:T):Void;
    extern public static function testStruct<T:CsStruct>(t:T):Void;
    extern public static function testConstructible<T:Constructible<()->Void>>(t:T):Void;
    extern public static function testConstructibleClass<T:Constructible<()->Void> & CsClass>(t:T):Void;
}

@:nativeGen
class Main {
    public static function main() {
        testClass(new Array<String>());
        TestCs.testClass(new Class_(new Array<String>()).value);

        testStruct(42);
        TestCs.testStruct(new Struct(42).value);

        testConstructible(new haxe.Serializer());
        TestCs.testConstructible(new Constructible_(new haxe.Serializer()).value);

        testConstructibleClass(new haxe.Serializer());
        TestCs.testConstructibleClass(new ConstructibleClass(new haxe.Serializer()).value);
    }

    static function testClass<T:CsClass>(value:T) TestCs.testClass(value);
    static function testStruct<T:CsStruct>(value:T) TestCs.testStruct(value);
    static function testConstructible<T:Constructible<()->Void>>(value:T) TestCs.testConstructible(value);
    static function testConstructibleClass<T:Constructible<()->Void> & CsClass>(value:T) TestCs.testConstructibleClass(value);
}

@:nativeGen
class Class_<T:CsClass> {
    public var value:T;
    public function new(value:T) this.value = value;
}

@:nativeGen
class Struct<T:CsStruct> {
    public var value:T;
    public function new(value:T) this.value = value;
}

@:nativeGen
class Constructible_<T:Constructible<()->Void>> {
    public var value:T;
    public function new(value:T) this.value = value;
}

@:nativeGen
class ConstructibleClass<T:Constructible<()->Void> & CsClass> {
    public var value:T;
    public function new(value:T) this.value = value;
}

@:nativeGen
class StructT<T, T1:T & CsStruct> {}

#if (cs_ver >= "7.3")
@:nativeGen
class Unmanaged<T:CsUnmanaged> {}

@:nativeGen
class UnmanagedClass<T:CsUnmanaged & CsClass> {}
#end