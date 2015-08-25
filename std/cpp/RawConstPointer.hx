package cpp;

@:unreflective
extern class RawConstPointer<T>
{
    @:extern inline static function nullptr<T>():RawConstPointer<T> return untyped __cpp__("NULL");
    @:extern inline static function of<T>(v:T):RawConstPointer<T> return untyped __cpp__("&{0}", v);
}
