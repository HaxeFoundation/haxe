package fail;

import cs.system.reflection.AssemblyDelaySignAttribute;

enum SomeEnum {}

@:assemblyStrict(cs.system.reflection.AssemblyDelaySignAttribute(true))
class NotFirstType {}

