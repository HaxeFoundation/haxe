import cs.Constraints;
import haxe.Constraints.Constructible;

@:nativeGen
class StructAndConstructible<T:CsStruct & Constructible<Void->Void>> {}

@:nativeGen
class ConstructibleAndStruct<T:Constructible<Void->Void> & CsStruct> {}

@:nativeGen
class StructAndClass<T:CsStruct & CsClass> {}

@:nativeGen
class ClassAndStruct<T:CsClass & CsStruct> {}

#if (cs_ver >= "7.3")
@:nativeGen
class UnmanagedAndStruct<T:CsUnmanaged & CsStruct> {}

@:nativeGen
class StructAndUnmanaged<T:CsStruct & CsUnmanaged> {}

@:nativeGen
class UnmanagedAndConstructible<T:CsUnmanaged & Constructible<Void->Void>> {}

@:nativeGen
class ConstructibleAndUnmanaged<T:Constructible<Void->Void> & CsUnmanaged> {}
#end