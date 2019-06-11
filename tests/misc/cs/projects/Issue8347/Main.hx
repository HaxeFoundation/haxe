import cs.system.reflection.AssemblyDelaySignAttribute;

@:assemblyMeta(Test)
class Main {}

@:assemblyStrict(cs.system.reflection.AssemblyDelaySignAttribute(true))
class Main2 {}
