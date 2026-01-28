namespace haxe.lang
{
    /// <summary>
    /// Base class for all Haxe enum types.
    /// Provides AOT-safe access to enum index and parameters.
    /// </summary>
    public abstract class HaxeEnum
    {
        private static readonly object[] EmptyParams = new object[0];

        public int _hx_index;

        protected HaxeEnum(int index)
        {
            this._hx_index = index;
        }

        public int _hx_getIndex()
        {
            return this._hx_index;
        }

        /// <summary>
        /// Returns parameters for this enum constructor.
        /// Override in subclasses that have parameters.
        /// </summary>
        public virtual object[] _hx_getParameters()
        {
            return EmptyParams;
        }
    }
}
