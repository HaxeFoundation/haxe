import cs.system.reflection.AssemblyDelaySignAttribute;

@:cs.assemblyMeta(Test)
class Main {}

@:cs.assemblyStrict(cs.system.reflection.AssemblyDelaySignAttribute(true))
class Main2 {}
