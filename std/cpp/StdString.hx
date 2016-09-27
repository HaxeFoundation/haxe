package cpp;

using cpp.NativeString;

@:native("hx::StdString")
@:include("hx/StdString.h")
@:stackOnly
@:structAccess
extern class StdString extends StdStringRef
{
   @:extern @:native("std::string::npos")
   public static var npos(default,null):Int;

   //public function new(inData:StdStringData);

   @:native("hx::StdString")
   static public function ofString(s:String) : StdString;
   //public function toString():String;
   //public function find(s:String):Int;
   //public function substr(pos:Int, len:Int):StdString;
}

