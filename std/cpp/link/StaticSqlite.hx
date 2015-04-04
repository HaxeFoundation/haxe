package cpp.link;

@:cppFileCode( 'extern "C" int sqlite_register_prims();')
@:buildXml("
<target id='haxe'>
  <lib name='${HXCPP}/lib/${BINDIR}/libsqlite${LIBEXTRA}${LIBEXT}'/>
  <lib name='-lpthread' if='linux'/>
</target>
")
@:keep class StaticSqlite
{
   static function __init__()
   {
     untyped __cpp__("sqlite_register_prims();");
   }
}

