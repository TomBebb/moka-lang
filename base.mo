class Main {
	@LinkName("llvm.sqrt.f32")
	static extern func sqrt(v: float): float
	static func main(): int {
		printf("square root of 4=%f\n", 4.0)
		0
	}
	@LinkName("printf") @CallConv("vararg")	
	static extern func printf(v: String): void
}