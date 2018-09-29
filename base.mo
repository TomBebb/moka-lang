class Main {
	@LinkName("llvm.sqrt.f32")
	static extern func sqrt(v: float): float

	static func main(): int {
		var a = sqrt(5.0)
		0
	}
}