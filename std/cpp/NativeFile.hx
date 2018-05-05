package cpp;

@:buildXml('<include name="${HXCPP}/src/hx/libs/std/Build.xml"/>')
extern class NativeFile
{
   @:native("_hx_std_file_open")
   extern public static function file_open(fname:String,r:String) : Dynamic return null;

   @:native("_hx_std_file_close")
   extern public static function file_close(handle:Dynamic) : Void { }


   @:native("_hx_std_file_write")
   extern public static function file_write(handle:Dynamic,s:haxe.io.BytesData,p:Int,n:Int) : Int return 0;


   @:native("_hx_std_file_write_char")
   extern public static function file_write_char(handle:Dynamic,c:Int) : Void { }


   @:native("_hx_std_file_read")
   extern public static function file_read(handle:Dynamic,s:haxe.io.BytesData,p:Int,n:Int) : Int return 0;


   @:native("_hx_std_file_read_char")
   extern public static function file_read_char(handle:Dynamic) : Int return 0;


   @:native("_hx_std_file_seek")
   extern public static function file_seek(handle:Dynamic,pos:Int,kind:Int) : Void { }


   @:native("_hx_std_file_tell")
   extern public static function file_tell(handle:Dynamic) : Int return 0;


   @:native("_hx_std_file_eof")
   extern public static function file_eof(handle:Dynamic) : Bool return false;


   @:native("_hx_std_file_flush")
   extern public static function file_flush(handle:Dynamic) : Void return null;


   @:native("_hx_std_file_contents_string")
   extern public static function file_contents_string(name:String) : String return null;


   @:native("_hx_std_file_contents_bytes")
   extern public static function file_contents_bytes(name:String) : haxe.io.BytesData return null;


   @:native("_hx_std_file_stdin")
   extern public static function file_stdin() : Dynamic return null;


   @:native("_hx_std_file_stdout")
   extern public static function file_stdout() : Dynamic return null;


   @:native("_hx_std_file_stderr")
   extern public static function file_stderr() : Dynamic return null;

}
