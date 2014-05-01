
package python.internal;

import python.Syntax;

@:keep
@:nativeGen
@:native("_hx_AnonObject")
class AnonObject {
    public function new(fields) {
        Syntax.assign(Internal.fieldDict(this), fields);
    }
}
