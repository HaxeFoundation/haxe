package cpp;

@:buildXml('<include name="${HXCPP}/src/hx/libs/std/Build.xml"/>')
extern class NativeProcess
{

   @:extern @:native("_hx_std_process_run")
   public static function process_run(cmd:String,vargs:Array<String>) : Dynamic return null;

   @:extern @:native("_hx_std_process_run")
   public static function process_run_with_show(cmd:String,vargs:Array<String>,inShow:Int) : Dynamic return null;

   @:extern @:native("_hx_std_process_stdout_read")
   public static function process_stdout_read(handle:Dynamic,buf:haxe.io.BytesData,pos:Int,len:Int) : Int return 0;


   @:extern @:native("_hx_std_process_stderr_read")
   public static function process_stderr_read(handle:Dynamic,buf:haxe.io.BytesData,pos:Int,len:Int) : Int return 0;


   @:extern @:native("_hx_std_process_stdin_write")
   public static function process_stdin_write(handle:Dynamic,buf:haxe.io.BytesData,pos:Int,len:Int) : Int return 0;


   @:extern @:native("_hx_std_process_stdin_close")
   public static function process_stdin_close(handle:Dynamic) : Void { }


   @:extern @:native("_hx_std_process_exit")
   public static function process_exit(handle:Dynamic) : Int return 0;


   @:extern @:native("_hx_std_process_pid")
   public static function process_pid(handle:Dynamic) : Int return 0;


   @:extern @:native("_hx_std_process_close")
   public static function process_close(handle:Dynamic) : Void { };

}
