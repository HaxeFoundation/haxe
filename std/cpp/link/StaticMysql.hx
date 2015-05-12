package cpp.link;

@:cppFileCode( 'extern "C" int mysql_register_prims();')
@:buildXml("
<target id='haxe'>
  <lib name='${HXCPP}/lib/${BINDIR}/libmysql5${LIBEXTRA}${LIBEXT}'/>
  <lib name='ws2_32.lib' if='windows'/>
</target>
")
@:keep class StaticMysql
{
   static function __init__()
   {
     untyped __cpp__("mysql_register_prims();");
   }
}

