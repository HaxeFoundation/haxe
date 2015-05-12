package cpp.link;

@:cppFileCode( 'extern "C" int std_register_prims();')
@:buildXml("
<target id='haxe'>
  <lib name='${HXCPP}/lib/${BINDIR}/libstd${LIBEXTRA}${LIBEXT}'/>
   <lib name='ws2_32.lib' if='windows'/>
</target>
")
@:keep class StaticStd
{
   static function __init__()
   {
     untyped __cpp__("std_register_prims();");
   }
}

