package pack;

import cs.system.reflection.AssemblyDelaySignAttribute;

@:cs.assemblyMeta(System.Reflection.AssemblyDefaultAliasAttribute("test"))
@:cs.assemblyStrict(cs.system.reflection.AssemblyDelaySignAttribute(true))
class Main {}
