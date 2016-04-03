package cpp;

using cpp.NativeString;

@:native("hx::StdString const &")
@:include("hx/StdString.h")
@:structAccess
extern class StdStringRef
{
   public function c_str() : ConstPointer<Char>;
   public function size() : Int;
   public function find(s:String):Int;
   public function substr(pos:Int, len:Int):StdString;
   public function toString():String;
   public function toStdString():StdString;
}

