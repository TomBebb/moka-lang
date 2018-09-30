class Main {
	@LinkName("llvm.sqrt.f32")
	static extern func sqrt(v: float): float
	static func main(): int {
		printf("Hello, world!")
		2
	}
	@LinkName("printf")
	static extern func printf(v: String): void
}