package cpp;

@:unreflective
extern class RawPointer<T> extends RawConstPointer<T>
{
    @:extern inline static function nullptr<T>():RawPointer<T> return untyped __cpp__("NULL");
    @:extern inline static function of<T>(v:T):RawPointer<T> return untyped __cpp__("&{0}", v);
}
