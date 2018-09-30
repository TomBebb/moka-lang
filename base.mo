class Main {
	static val PI: float = 3.14
	@LinkName("llvm.sqrt.f32")
	static extern func sqrt(v: float): float
	var x: float
	var y: float
	func new() {
		x = 1.0
		y = 2.0
		x += 1.0
	}
	func length(): float {
		sqrt((x * x) + (y * y))
	}
	static func main(): int {
		var main = new Main()
		2
	}

}