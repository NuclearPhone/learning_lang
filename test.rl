test(c: number) -> void
{
	@PRINT(c)
}

main() -> void
{
	string_variable: string = "hello, world"

	@PRINT(string_variable)

	test_variable: number = 25

	test_variable = 3

	(string_variable == "abc") ?
		@PRINT(test_variable)
	! @PRINT(test_variable * 4)

	test_variable < 5 -> {
		@PRINT(test_variable)
		test_variable = test_variable + 1
	}
}
