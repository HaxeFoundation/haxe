mod BaseIter;
mod Type;
mod EnumValueTools;
mod String;
mod ArrayAccess;
mod vec;
mod EnumTools;
pub trait HxObject {
	pub fn __get_field(&self, field:@str) -> Option<HxObject>;
	pub fn __set_field(&mut self, field:@str, value:Option<@HxObject>) -> Option<@HxObject>;
	pub fn toString(&self) -> @str;
}
impl ToStr for HxObject {
	pub fn to_str(&self) -> ~str {
		return self.toString();
	}
}
impl HxObject for BaseIter<T> {
}
impl HxObject for vec<T> {
}
impl HxObject for ArrayAccess<T> {
}
impl HxObject for String {
}
impl HxObject for Type {
}
impl HxObject for haxe::EnumTools {
}
impl HxObject for haxe::EnumValueTools {
}
