/*
 * typedarray.idl
 *
 * TypedArray IDL definitions scraped from the Khronos specification.
 *
 * Original Khronos Working Draft:
 *
 *   https://www.khronos.org/registry/typedarray/specs/latest/
 */

[ Constructor(unsigned long length) ]
interface ArrayBuffer_ {
    readonly attribute unsigned long byteLength;
    ArrayBuffer_ slice(long begin, optional long end);
    static boolean isView(any value);
};

/* ArrayBuffer_ implements Transferable; */

[NoInterfaceObject]
interface ArrayBufferView_ {
    readonly attribute ArrayBuffer_ buffer;
    readonly attribute unsigned long byteOffset;
    readonly attribute unsigned long byteLength;
};


// The 'byte' type does not currently exist in Web IDL.
// In this IDL, it should be a signed 8 bit type.
[
    Constructor(unsigned long length),
    Constructor(Int8Array_ array),
    Constructor(byte[] array),
    Constructor(ArrayBuffer_ buffer, 
                optional unsigned long byteOffset, optional unsigned long length)
]
interface Int8Array_ : ArrayBufferView_ {
    const long BYTES_PER_ELEMENT = 1;

    readonly attribute unsigned long length;
    
    getter byte get(unsigned long index);
    setter void set_(unsigned long index, byte value);
    void set(Int8Array_ array, optional unsigned long offset);
    void set(byte[] array, optional unsigned long offset);
    Int8Array_ subarray(long start, long end);
};


// The 'unsigned byte' type does not currently exist in Web IDL, though
// 'octet' is equivalent.
[
    Constructor(unsigned long length),
    Constructor(Uint8Array_ array),
    Constructor(octet[] array),
    Constructor(ArrayBuffer_ buffer, 
                optional unsigned long byteOffset, optional unsigned long length)
]
interface Uint8Array_ : ArrayBufferView_ {
    const long BYTES_PER_ELEMENT = 1;
    
    readonly attribute unsigned long length;
    
    getter octet get(unsigned long index);
    setter void set_(unsigned long index, octet value);
    void set(Uint8Array_ array, optional unsigned long offset);
    void set(octet[] array, optional unsigned long offset);
    Uint8Array_ subarray(long start, long end);
};


[
    Constructor(unsigned long length),
    Constructor(Uint8ClampedArray_ array),
    Constructor(octet[] array),
    Constructor(ArrayBuffer_ buffer, 
                optional unsigned long byteOffset, optional unsigned long length)
]
interface Uint8ClampedArray_ : ArrayBufferView_ {
    const long BYTES_PER_ELEMENT = 1;
    
    readonly attribute unsigned long length;

    getter octet get(unsigned long index);
    setter void set_(unsigned long index, [Clamp] octet value);
    void set(Uint8ClampedArray_ array, optional unsigned long offset);
    void set(octet[] array, optional unsigned long offset);
    Uint8ClampedArray_ subarray(long start, long end);
};


[
    Constructor(unsigned long length),
    Constructor(Int16Array_ array),
    Constructor(short[] array),
    Constructor(ArrayBuffer_ buffer, 
                optional unsigned long byteOffset, optional unsigned long length)
]
interface Int16Array_ : ArrayBufferView_ {
    const long BYTES_PER_ELEMENT = 2;
    
    readonly attribute unsigned long length;
    
    getter short get(unsigned long index);
    setter void set_(unsigned long index, short value);
    void set(Int16Array_ array, optional unsigned long offset);
    void set(short[] array, optional unsigned long offset);
    Int16Array_ subarray(long start, long end);
};


[
    Constructor(unsigned long length),
    Constructor(Uint16Array_ array),
    Constructor(unsigned short[] array),
    Constructor(ArrayBuffer_ buffer, 
                optional unsigned long byteOffset, optional unsigned long length)
]
interface Uint16Array_ : ArrayBufferView_ {
    const long BYTES_PER_ELEMENT = 2;
    
    readonly attribute unsigned long length;
    
    getter unsigned short get(unsigned long index);
    setter void set_(unsigned long index, unsigned short value);
    void set(Uint16Array_ array, optional unsigned long offset);
    void set(unsigned short[] array, optional unsigned long offset);
    Uint16Array_ subarray(long start, long end);
};


[
    Constructor(unsigned long length),
    Constructor(Int32Array_ array),
    Constructor(long[] array),
    Constructor(ArrayBuffer_ buffer, 
                optional unsigned long byteOffset, optional unsigned long length)
]
interface Int32Array_ : ArrayBufferView_ {
    const long BYTES_PER_ELEMENT = 4;
    
    readonly attribute unsigned long length;
    
    getter long get(unsigned long index);
    setter void set_(unsigned long index, long value);
    void set(Int32Array_ array, optional unsigned long offset);
    void set(long[] array, optional unsigned long offset);
    Int32Array_ subarray(long start, long end);
};


[
    Constructor(unsigned long length),
    Constructor(Uint32Array_ array),
    Constructor(unsigned long[] array),
    Constructor(ArrayBuffer_ buffer, 
                optional unsigned long byteOffset, optional unsigned long length)
]
interface Uint32Array_ : ArrayBufferView_ {
    const long BYTES_PER_ELEMENT = 4;
    
    readonly attribute unsigned long length;
    
    getter unsigned long get(unsigned long index);
    setter void set_(unsigned long index, unsigned long value);
    void set(Uint32Array_ array, optional unsigned long offset);
    void set(unsigned long[] array, optional unsigned long offset);
    Uint32Array_ subarray(long start, long end);
};


[
    Constructor(unsigned long length),
    Constructor(Float32Array_ array),
    Constructor(unrestricted float[] array),
    Constructor(ArrayBuffer_ buffer, 
                optional unsigned long byteOffset, optional unsigned long length)
]
interface Float32Array_  : ArrayBufferView_ {
    const long BYTES_PER_ELEMENT = 4;
    
    readonly attribute unsigned long length;
    
    getter unrestricted float get(unsigned long index);
    setter void set_(unsigned long index, unrestricted float value);
    void set(Float32Array_ array, optional unsigned long offset);
    void set(unrestricted float[] array, optional unsigned long offset);
    Float32Array_ subarray(long start, long end);
};


[
    Constructor(unsigned long length),
    Constructor(Float64Array_ array),
    Constructor(unrestricted double[] array),
    Constructor(ArrayBuffer_ buffer, 
                optional unsigned long byteOffset, optional unsigned long length)
]
interface Float64Array_ : ArrayBufferView_ {
    const long BYTES_PER_ELEMENT = 8;
    
    readonly attribute unsigned long length;
    
    getter unrestricted double get(unsigned long index);
    setter void set_(unsigned long index, unrestricted double value);
    void set(Float64Array_ array, optional unsigned long offset);
    void set(unrestricted double[] array, optional unsigned long offset);
    Float64Array_ subarray(long start, long end);
};


[
  Constructor(ArrayBuffer_ buffer,
              optional unsigned long byteOffset,
              optional unsigned long byteLength)
]
interface DataView_ : ArrayBufferView_ {
    // Gets the value of the given type at the specified byte offset
    // from the start of the view. There is no alignment constraint;
    // multi-byte values may be fetched from any offset.
    //
    // For multi-byte values, the optional littleEndian argument
    // indicates whether a big-endian or little-endian value should be
    // read. If false or undefined, a big-endian value is read.
    //
    // These methods raise an INDEX_SIZE_ERR exception if they would read
    // beyond the end of the view.
    byte getInt8(unsigned long byteOffset);
    octet getUint8(unsigned long byteOffset);
    short getInt16(unsigned long byteOffset,
                   optional boolean littleEndian);
    unsigned short getUint16(unsigned long byteOffset,
                             optional boolean littleEndian);
    long getInt32(unsigned long byteOffset,
                  optional boolean littleEndian);
    unsigned long getUint32(unsigned long byteOffset,
                            optional boolean littleEndian);
    unrestricted float getFloat32(unsigned long byteOffset,
                                  optional boolean littleEndian);
    unrestricted double getFloat64(unsigned long byteOffset,
                                   optional boolean littleEndian);

    // Stores a value of the given type at the specified byte offset
    // from the start of the view. There is no alignment constraint;
    // multi-byte values may be stored at any offset.
    //
    // For multi-byte values, the optional littleEndian argument
    // indicates whether the value should be stored in big-endian or
    // little-endian byte order. If false or undefined, the value is
    // stored in big-endian byte order.
    //
    // These methods throw exceptions if they would write beyond the end
    // of the view.
    void setInt8(unsigned long byteOffset,
                 byte value);
    void setUint8(unsigned long byteOffset,
                  octet value);
    void setInt16(unsigned long byteOffset,
                  short value,
                  optional boolean littleEndian);
    void setUint16(unsigned long byteOffset,
                   unsigned short value,
                   optional boolean littleEndian);
    void setInt32(unsigned long byteOffset,
                  long value,
                  optional boolean littleEndian);
    void setUint32(unsigned long byteOffset,
                   unsigned long value,
                   optional boolean littleEndian);
    void setFloat32(unsigned long byteOffset,
                    unrestricted float value,
                    optional boolean littleEndian);
    void setFloat64(unsigned long byteOffset,
                    unrestricted double value,
                    optional boolean littleEndian);
};
