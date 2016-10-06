package lua.lib.luv;

@:luaRequire("luv")
extern class Misc {
  public static function chdir(path : String) : Bool;

  public static function os_homedir() : String;
  public static function os_tmpdir() : String;
  public static function os_get_passwd() : String;
  public static function cpu_info() : Table<Int,CpuInfo>;

  public static function cwd() : String;
  public static function exepath() : String;
  public static function get_process_title() : String;
  public static function get_total_memory() : Int;
  public static function get_free_memory() : Int;
  public static function getpid() : Int;

  // TODO Windows only?
  public static function getuid() : Int;
  public static function setuid(from : Int, to : Int) : String;
  public static function getgid() : Int;
  public static function setgid(from : Int, to : Int) : Void;

  public static function getrusage() : ResourceUsage;
  public static function guess_handle(handle : Int) : String;
  public static function hrtime() : Float;

  // TODO: implement this
  // public static function interface_addresses() : String;

  public static function loadavg() : Float;
  public static function resident_set_memory() : Int;
  public static function set_process_title(title : String) : Bool;
  public static function uptime() : Int;
  public static function version() : Int;
  public static function version_string() : String;

  // TODO : Windows only
  public static function print_all_handles() : Table<Int,String>;
  public static function print_active_handles() : Table<Int,String>;

}

typedef CpuInfo = {
  model : String,
  times : CpuTimes,
  speed : Int
}

typedef CpuTimes = {
  sys  : Int,
  idle : Int,
  irq  : Int,
  user : Int
}

typedef ResourceUsage = {
  nivcsw   : Int,
  maxrss   : Int,
  msgrcv   : Int,
  isrss    : Int,
  inblock  : Int,
  ixrss    : Int,
  nvcsw    : Int,
  nsignals : Int,
  minflt   : Int,
  nswap    : Int,
  msgsnd   : Int,
  oublock  : Int,
  majflt   : Int,
  stime    : MicroTimeStamp,
  idrss    : Int,
  utime    : MicroTimeStamp
}

typedef MicroTimeStamp = {
  usec : Int,
  sec : Int
}
