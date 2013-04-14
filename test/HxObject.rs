mod BaseIter;
mod Type;
mod EnumValueTools;
mod String;
mod ArrayAccess;
mod vec;
mod EnumTools;
pub trait HxObject {
	pub fn toString(&self) -> @str;
}
impl ToStr for HxObject {
	pub fn to_str(&self) -> ~str {
		return self.toString();
	}
}
impl HxObject for BaseIter<T> {
	pub fn toString() -> Option<@str> {
		return Self.__name();
	}
}
impl HxObject for vec<T> {
	pub fn toString() -> Option<@str> {
		return Self.__name();
	}
}
impl HxObject for ArrayAccess<T> {
	pub fn toString() -> Option<@str> {
		return Self.__name();
	}
}
impl HxObject for String {
	pub fn toString() -> Option<@str> {
		return Self.__name();
	}
}
impl HxObject for Type {
	pub fn toString() -> Option<@str> {
		return Self.__name();
	}
}
impl HxObject for haxe::EnumTools {
	pub fn toString() -> Option<@str> {
		return Self.__name();
	}
}
impl HxObject for haxe::EnumValueTools {
	pub fn toString() -> Option<@str> {
		return Self.__name();
	}
}
