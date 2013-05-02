pub trait HxEnum {
	}
pub trait HxObject {
	pub fn toString(&self) -> Option<@str>;
}impl<T> HxObject for BaseIter<T> {
	pub fn toString(&self) -> Option<@str> {
		return Some(@"BaseIter");
	}
}
impl HxObject for i32 {
	pub fn toString(&self) -> Option<@str> {
		return Some(self.to_str().to_managed());
	}
}
impl HxObject for i64 {
	pub fn toString(&self) -> Option<@str> {
		return Some(self.to_str().to_managed());
	}
}
impl HxObject for f32 {
	pub fn toString(&self) -> Option<@str> {
		return Some(self.to_str().to_managed());
	}
}
impl HxObject for f64 {
	pub fn toString(&self) -> Option<@str> {
		return Some(self.to_str().to_managed());
	}
}
