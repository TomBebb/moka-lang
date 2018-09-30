class Main {
	@LinkName("llvm.sqrt.f32")
	static extern func sqrt(v: float): float
	static func main(): int {
		val vec = new Vector2(3.0, 4.0)
		2
	}

}

struct Vector2 {
	var x: float
	var y: float
	func new(x: float, y: float) {
		this.x = x
		this.y = y
	}
	func length(): float {
		Main.sqrt((x * x) + (y * y))
	}
}