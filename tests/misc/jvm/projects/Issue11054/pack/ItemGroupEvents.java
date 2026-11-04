package pack;

public final class ItemGroupEvents {
	@FunctionalInterface
	public interface ModifyEntries {
		void modifyEntries(int entries);
	}
}