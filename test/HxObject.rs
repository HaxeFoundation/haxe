mod HxObject;
mod HxEnum;
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
impl HxObject for Option<@BaseIter> {
}
impl HxObject for Option<@vec> {
}
impl HxObject for Option<@ArrayAccess> {
}
impl HxObject for Option<@str> {
}
impl HxObject for Option<@Type> {
}
impl HxObject for Option<@haxe::EnumTools> {
}
impl HxObject for Option<@haxe::EnumValueTools> {
}
