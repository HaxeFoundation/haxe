package cs.system.numerics;

/** Represents a 4x4 matrix. */
@:native("System.Numerics.Matrix4x4")
extern class Matrix4x4 extends cs.system.ValueType {
	/**
	 * Gets the multiplicative identity matrix.
	 * @return Gets the multiplicative identity matrix.
	 */
	static var Identity(default, never):cs.system.numerics.Matrix4x4;
	/** The first element of the first row. */
	var M11:Single;
	/** The second element of the first row. */
	var M12:Single;
	/** The third element of the first row. */
	var M13:Single;
	/** The fourth element of the first row. */
	var M14:Single;
	/** The first element of the second row. */
	var M21:Single;
	/** The second element of the second row. */
	var M22:Single;
	/** The third element of the second row. */
	var M23:Single;
	/** The fourth element of the second row. */
	var M24:Single;
	/** The first element of the third row. */
	var M31:Single;
	/** The second element of the third row. */
	var M32:Single;
	/** The third element of the third row. */
	var M33:Single;
	/** The fourth element of the third row. */
	var M34:Single;
	/** The first element of the fourth row. */
	var M41:Single;
	/** The second element of the fourth row. */
	var M42:Single;
	/** The third element of the fourth row. */
	var M43:Single;
	/** The fourth element of the fourth row. */
	var M44:Single;
	/**
	 * Indicates whether the current matrix is the identity matrix.
	 * @return if the current matrix is the identity matrix; otherwise, .
	 */
	var IsIdentity(default, never):Bool;
	/**
	 * Gets or sets the translation component of this matrix.
	 * @return The translation component of the current instance.
	 */
	var Translation(default, default):cs.system.numerics.Vector3;
	@:overload(function(value:cs.system.numerics.Matrix3x2):Void {})
	function new(m11:Single, m12:Single, m13:Single, m14:Single, m21:Single, m22:Single, m23:Single, m24:Single, m31:Single, m32:Single, m33:Single, m34:Single, m41:Single, m42:Single, m43:Single, m44:Single):Void;
	/**
	 * Adds each element in one matrix with its corresponding element in a second
	 * matrix.
	 * @param value1 The first matrix.
	 * @param value2 The second matrix.
	 * @return The matrix that contains the summed values of  and .
	 */
	static function Add(value1:cs.system.numerics.Matrix4x4, value2:cs.system.numerics.Matrix4x4):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a spherical billboard that rotates around a specified object position.
	 * @param objectPosition The position of the object that the billboard will rotate
	 * around.
	 * @param cameraPosition The position of the camera.
	 * @param cameraUpVector The up vector of the camera.
	 * @param cameraForwardVector The forward vector of the camera.
	 * @return The created billboard.
	 */
	static function CreateBillboard(objectPosition:cs.system.numerics.Vector3, cameraPosition:cs.system.numerics.Vector3, cameraUpVector:cs.system.numerics.Vector3, cameraForwardVector:cs.system.numerics.Vector3):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a cylindrical billboard that rotates around a specified axis.
	 * @param objectPosition The position of the object that the billboard will rotate
	 * around.
	 * @param cameraPosition The position of the camera.
	 * @param rotateAxis The axis to rotate the billboard around.
	 * @param cameraForwardVector The forward vector of the camera.
	 * @param objectForwardVector The forward vector of the object.
	 * @return The billboard matrix.
	 */
	static function CreateConstrainedBillboard(objectPosition:cs.system.numerics.Vector3, cameraPosition:cs.system.numerics.Vector3, rotateAxis:cs.system.numerics.Vector3, cameraForwardVector:cs.system.numerics.Vector3, objectForwardVector:cs.system.numerics.Vector3):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a matrix that rotates around an arbitrary vector.
	 * @param axis The axis to rotate around.
	 * @param angle The angle to rotate around , in radians.
	 * @return The rotation matrix.
	 */
	static function CreateFromAxisAngle(axis:cs.system.numerics.Vector3, angle:Single):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a rotation matrix from the specified Quaternion rotation value.
	 * @param quaternion The source Quaternion.
	 * @return The rotation matrix.
	 */
	static function CreateFromQuaternion(quaternion:cs.system.numerics.Quaternion):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a rotation matrix from the specified yaw, pitch, and roll.
	 * @param yaw The angle of rotation, in radians, around the Y axis.
	 * @param pitch The angle of rotation, in radians, around the X axis.
	 * @param roll The angle of rotation, in radians, around the Z axis.
	 * @return The rotation matrix.
	 */
	static function CreateFromYawPitchRoll(yaw:Single, pitch:Single, roll:Single):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a view matrix.
	 * @param cameraPosition The position of the camera.
	 * @param cameraTarget The target towards which the camera is pointing.
	 * @param cameraUpVector The direction that is "up" from the camera's point of
	 * view.
	 * @return The view matrix.
	 */
	static function CreateLookAt(cameraPosition:cs.system.numerics.Vector3, cameraTarget:cs.system.numerics.Vector3, cameraUpVector:cs.system.numerics.Vector3):cs.system.numerics.Matrix4x4;
	/**
	 * Creates an orthographic perspective matrix from the given view volume
	 * dimensions.
	 * @param width The width of the view volume.
	 * @param height The height of the view volume.
	 * @param zNearPlane The minimum Z-value of the view volume.
	 * @param zFarPlane The maximum Z-value of the view volume.
	 * @return The orthographic projection matrix.
	 */
	static function CreateOrthographic(width:Single, height:Single, zNearPlane:Single, zFarPlane:Single):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a customized orthographic projection matrix.
	 * @param left The minimum X-value of the view volume.
	 * @param right The maximum X-value of the view volume.
	 * @param bottom The minimum Y-value of the view volume.
	 * @param top The maximum Y-value of the view volume.
	 * @param zNearPlane The minimum Z-value of the view volume.
	 * @param zFarPlane The maximum Z-value of the view volume.
	 * @return The orthographic projection matrix.
	 */
	static function CreateOrthographicOffCenter(left:Single, right:Single, bottom:Single, top:Single, zNearPlane:Single, zFarPlane:Single):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a perspective projection matrix from the given view volume dimensions.
	 * @param width The width of the view volume at the near view plane.
	 * @param height The height of the view volume at the near view plane.
	 * @param nearPlaneDistance The distance to the near view plane.
	 * @param farPlaneDistance The distance to the far view plane.
	 * @return The perspective projection matrix.
	 */
	static function CreatePerspective(width:Single, height:Single, nearPlaneDistance:Single, farPlaneDistance:Single):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a perspective projection matrix based on a field of view, aspect ratio,
	 * and near and far view plane distances.
	 * @param fieldOfView The field of view in the y direction, in radians.
	 * @param aspectRatio The aspect ratio, defined as view space width divided by
	 * height.
	 * @param nearPlaneDistance The distance to the near view plane.
	 * @param farPlaneDistance The distance to the far view plane.
	 * @return The perspective projection matrix.
	 */
	static function CreatePerspectiveFieldOfView(fieldOfView:Single, aspectRatio:Single, nearPlaneDistance:Single, farPlaneDistance:Single):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a customized perspective projection matrix.
	 * @param left The minimum x-value of the view volume at the near view plane.
	 * @param right The maximum x-value of the view volume at the near view plane.
	 * @param bottom The minimum y-value of the view volume at the near view plane.
	 * @param top The maximum y-value of the view volume at the near view plane.
	 * @param nearPlaneDistance The distance to the near view plane.
	 * @param farPlaneDistance The distance to the far view plane.
	 * @return The perspective projection matrix.
	 */
	static function CreatePerspectiveOffCenter(left:Single, right:Single, bottom:Single, top:Single, nearPlaneDistance:Single, farPlaneDistance:Single):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a matrix that reflects the coordinate system about a specified plane.
	 * @param value The plane about which to create a reflection.
	 * @return A new matrix expressing the reflection.
	 */
	static function CreateReflection(value:cs.system.numerics.Plane):cs.system.numerics.Matrix4x4;
	@:overload(function(radians:Single):cs.system.numerics.Matrix4x4 {})
	/**
	 * Creates a matrix for rotating points around the X axis.
	 * @param radians The amount, in radians, by which to rotate around the X axis.
	 * @return The rotation matrix.
	 */
	static function CreateRotationX(radians:Single, centerPoint:cs.system.numerics.Vector3):cs.system.numerics.Matrix4x4;
	@:overload(function(radians:Single):cs.system.numerics.Matrix4x4 {})
	/**
	 * Creates a matrix for rotating points around the Y axis.
	 * @param radians The amount, in radians, by which to rotate around the Y-axis.
	 * @return The rotation matrix.
	 */
	static function CreateRotationY(radians:Single, centerPoint:cs.system.numerics.Vector3):cs.system.numerics.Matrix4x4;
	@:overload(function(radians:Single):cs.system.numerics.Matrix4x4 {})
	/**
	 * Creates a matrix for rotating points around the Z axis.
	 * @param radians The amount, in radians, by which to rotate around the Z-axis.
	 * @return The rotation matrix.
	 */
	static function CreateRotationZ(radians:Single, centerPoint:cs.system.numerics.Vector3):cs.system.numerics.Matrix4x4;
	@:overload(function(scales:cs.system.numerics.Vector3):cs.system.numerics.Matrix4x4 {})
	@:overload(function(scale:Single):cs.system.numerics.Matrix4x4 {})
	@:overload(function(scales:cs.system.numerics.Vector3, centerPoint:cs.system.numerics.Vector3):cs.system.numerics.Matrix4x4 {})
	@:overload(function(scale:Single, centerPoint:cs.system.numerics.Vector3):cs.system.numerics.Matrix4x4 {})
	@:overload(function(xScale:Single, yScale:Single, zScale:Single):cs.system.numerics.Matrix4x4 {})
	/**
	 * Creates a scaling matrix from the specified vector scale.
	 * @param scales The scale to use.
	 * @return The scaling matrix.
	 */
	static function CreateScale(xScale:Single, yScale:Single, zScale:Single, centerPoint:cs.system.numerics.Vector3):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a matrix that flattens geometry into a specified plane as if casting a
	 * shadow from a specified light source.
	 * @param lightDirection The direction from which the light that will cast the
	 * shadow is coming.
	 * @param plane The plane onto which the new matrix should flatten geometry so as
	 * to cast a shadow.
	 * @return A new matrix that can be used to flatten geometry onto the specified
	 * plane from the specified direction.
	 */
	static function CreateShadow(lightDirection:cs.system.numerics.Vector3, plane:cs.system.numerics.Plane):cs.system.numerics.Matrix4x4;
	@:overload(function(position:cs.system.numerics.Vector3):cs.system.numerics.Matrix4x4 {})
	/**
	 * Creates a translation matrix from the specified 3-dimensional vector.
	 * @param position The amount to translate in each axis.
	 * @return The translation matrix.
	 */
	static function CreateTranslation(xPosition:Single, yPosition:Single, zPosition:Single):cs.system.numerics.Matrix4x4;
	/**
	 * Creates a world matrix with the specified parameters.
	 * @param position The position of the object.
	 * @param forward The forward direction of the object.
	 * @param up The upward direction of the object. Its value is usually [0, 1, 0].
	 * @return The world matrix.
	 */
	static function CreateWorld(position:cs.system.numerics.Vector3, forward:cs.system.numerics.Vector3, up:cs.system.numerics.Vector3):cs.system.numerics.Matrix4x4;
	/**
	 * Attempts to extract the scale, translation, and rotation components from the
	 * given scale, rotation, or translation matrix. The return value indicates whether
	 * the operation succeeded.
	 * @param matrix The source matrix.
	 * @param scale When this method returns, contains the scaling component of the
	 * transformation matrix if the operation succeeded.
	 * @param rotation When this method returns, contains the rotation component of the
	 * transformation matrix if the operation succeeded.
	 * @param translation When the method returns, contains the translation component
	 * of the transformation matrix if the operation succeeded.
	 * @return if  was decomposed successfully; otherwise,  .
	 */
	static function Decompose(matrix:cs.system.numerics.Matrix4x4, scale:cs.Ref<cs.system.numerics.Vector3>, rotation:cs.Ref<cs.system.numerics.Quaternion>, translation:cs.Ref<cs.system.numerics.Vector3>):Bool;
	/**
	 * Inverts the specified matrix. The return value indicates whether the operation
	 * succeeded.
	 * @param matrix The matrix to invert.
	 * @param result When this method returns, contains the inverted matrix if the
	 * operation succeeded.
	 * @return if  was converted successfully; otherwise,  .
	 */
	static function Invert(matrix:cs.system.numerics.Matrix4x4, result:cs.Ref<cs.system.numerics.Matrix4x4>):Bool;
	/**
	 * Performs a linear interpolation from one matrix to a second matrix based on a
	 * value that specifies the weighting of the second matrix.
	 * @param matrix1 The first matrix.
	 * @param matrix2 The second matrix.
	 * @param amount The relative weighting of .
	 * @return The interpolated matrix.
	 */
	static function Lerp(matrix1:cs.system.numerics.Matrix4x4, matrix2:cs.system.numerics.Matrix4x4, amount:Single):cs.system.numerics.Matrix4x4;
	@:overload(function(value1:cs.system.numerics.Matrix4x4, value2:cs.system.numerics.Matrix4x4):cs.system.numerics.Matrix4x4 {})
	/**
	 * Returns the matrix that results from multiplying two matrices together.
	 * @param value1 The first matrix.
	 * @param value2 The second matrix.
	 * @return The product matrix.
	 */
	static function Multiply(value1:cs.system.numerics.Matrix4x4, value2:Single):cs.system.numerics.Matrix4x4;
	/**
	 * Negates the specified matrix by multiplying all its values by -1.
	 * @param value The matrix to negate.
	 * @return The negated matrix.
	 */
	static function Negate(value:cs.system.numerics.Matrix4x4):cs.system.numerics.Matrix4x4;
	/**
	 * Adds each element in one matrix with its corresponding element in a second
	 * matrix.
	 * @param value1 The first matrix.
	 * @param value2 The second matrix.
	 * @return The matrix that contains the summed values.
	 */
	static function op_Addition(value1:cs.system.numerics.Matrix4x4, value2:cs.system.numerics.Matrix4x4):cs.system.numerics.Matrix4x4;
	/**
	 * Returns a value that indicates whether the specified matrices are equal.
	 * @param value1 The first matrix to compare.
	 * @param value2 The second matrix to care
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(value1:cs.system.numerics.Matrix4x4, value2:cs.system.numerics.Matrix4x4):Bool;
	/**
	 * Returns a value that indicates whether the specified matrices are not equal.
	 * @param value1 The first matrix to compare.
	 * @param value2 The second matrix to compare.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(value1:cs.system.numerics.Matrix4x4, value2:cs.system.numerics.Matrix4x4):Bool;
	@:overload(function(value1:cs.system.numerics.Matrix4x4, value2:cs.system.numerics.Matrix4x4):cs.system.numerics.Matrix4x4 {})
	/**
	 * Returns the matrix that results from multiplying two matrices together.
	 * @param value1 The first matrix.
	 * @param value2 The second matrix.
	 * @return The product matrix.
	 */
	static function op_Multiply(value1:cs.system.numerics.Matrix4x4, value2:Single):cs.system.numerics.Matrix4x4;
	/**
	 * Subtracts each element in a second matrix from its corresponding element in a
	 * first matrix.
	 * @param value1 The first matrix.
	 * @param value2 The second matrix.
	 * @return The matrix containing the values that result from subtracting each
	 * element in  from its corresponding element in .
	 */
	static function op_Subtraction(value1:cs.system.numerics.Matrix4x4, value2:cs.system.numerics.Matrix4x4):cs.system.numerics.Matrix4x4;
	/**
	 * Negates the specified matrix by multiplying all its values by -1.
	 * @param value The matrix to negate.
	 * @return The negated matrix.
	 */
	static function op_UnaryNegation(value:cs.system.numerics.Matrix4x4):cs.system.numerics.Matrix4x4;
	/**
	 * Subtracts each element in a second matrix from its corresponding element in a
	 * first matrix.
	 * @param value1 The first matrix.
	 * @param value2 The second matrix.
	 * @return The matrix containing the values that result from subtracting each
	 * element in  from its corresponding element in .
	 */
	static function Subtract(value1:cs.system.numerics.Matrix4x4, value2:cs.system.numerics.Matrix4x4):cs.system.numerics.Matrix4x4;
	/**
	 * Transforms the specified matrix by applying the specified Quaternion rotation.
	 * @param value The matrix to transform.
	 * @param rotation The rotation t apply.
	 * @return The transformed matrix.
	 */
	static function Transform(value:cs.system.numerics.Matrix4x4, rotation:cs.system.numerics.Quaternion):cs.system.numerics.Matrix4x4;
	/**
	 * Transposes the rows and columns of a matrix.
	 * @param matrix The matrix to transpose.
	 * @return The transposed matrix.
	 */
	static function Transpose(matrix:cs.system.numerics.Matrix4x4):cs.system.numerics.Matrix4x4;
	@:overload(function(other:cs.system.numerics.Matrix4x4):Bool {})
	/**
	 * Returns a value that indicates whether this instance and another 4x4 matrix are
	 * equal.
	 * @param other The other matrix.
	 * @return if the two matrices are equal; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Calculates the determinant of the current 4x4 matrix.
	 * @return The determinant.
	 */
	function GetDeterminant():Single;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string that represents this matrix.
	 * @return The string representation of this matrix.
	 */
	function ToString():String;
}
