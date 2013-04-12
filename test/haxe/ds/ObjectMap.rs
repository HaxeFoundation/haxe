mod HxObject;
mod HxEnum;
pub struct ObjectMap {
	z_: @hashmap::linear::LinearMap;
}
pub impl ObjectMap {
	pub fn toString(&self) -> Option<@str> {
		return Some(@"Map");
	}
	pub fn iterator(&self) -> Option<@HxObject> {
		return None;
	}
	pub fn keys(&self) -> Option<@HxObject> {
		return None;
	}
	pub fn remove(&self, key: Option<@HxObject>) -> bool {
		return self.z_.remove(key);
	}
	pub fn exists(&self, key: Option<@HxObject>) -> bool {
		return self.z_.contains_key(key);
	}
	pub fn get(&self, key: Option<@HxObject>) -> Option<@HxObject> {
		return self.z_.find(key);
	}
	pub fn set(&self, key: Option<@HxObject>, value: Option<@HxObject>) -> () {
		self.z_.insert(key,value);
	}
	pub fn _new(weakKeys: bool) -> () {
		self.z_ = hashmap::linear::LinearMap.new();
	}
}
pub trait ObjectMapi {
	pub fn toString(&self) -> Option<@str> ;
	pub fn iterator(&self) -> Option<@HxObject> ;
	pub fn keys(&self) -> Option<@HxObject> ;
	pub fn remove(&self, key: Option<@HxObject>) -> bool ;
	pub fn exists(&self, key: Option<@HxObject>) -> bool ;
	pub fn get(&self, key: Option<@HxObject>) -> Option<@HxObject> ;
	pub fn set(&self, key: Option<@HxObject>, value: Option<@HxObject>) -> () ;
}
impl HxObject for Option<@haxe::ds::ObjectMap> {
	pub fn __get_field(&self, &field:str) {
		return match(field) {
			"_" => Some(self._),
			_ => None
		}
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		match(field) {
			"_" => self._ = value,
			_ => None
		}
	}
}
