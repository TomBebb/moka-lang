class Main {
	@LinkName("llvm.sqrt.f32")
	static extern func sqrt(v: float): float
	var x: float
	var y: float
	func new() {
		x = 1.0
		y = 2.0
	}
	static func main(): int {
		var main = new Main()
		2
	}
}