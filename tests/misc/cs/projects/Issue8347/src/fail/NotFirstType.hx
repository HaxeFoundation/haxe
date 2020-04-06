package fail;

import cs.system.reflection.AssemblyDelaySignAttribute;

enum SomeEnum {}

@:cs.assemblyStrict(cs.system.reflection.AssemblyDelaySignAttribute(true))
class NotFirstType {}

