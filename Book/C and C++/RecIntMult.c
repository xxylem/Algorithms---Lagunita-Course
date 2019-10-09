#include <stdio.h>
#include <stdbool.h>

struct IntPair {
	int left;
	int right;
};

int int_pow(base, power) {
	/** Input: two ints b and p.
		Output: b to the power p. */

	if (power == 0) {
		return 1;
	}

	if (power == 1) {
		return base;
	}

	else {

		int ans = int_pow(base * base, power / 2);

		if (power % 2 == 1) {
			return base * ans;
		}

		else {
			return ans;
		}
	}
}

int number_of_digits(x) {
	/** Input: n-digit number x
        Output: n */

	if (x == 0) {
		return 1;
	}

	int num_digits = 0;
	while (x > 0) {
		x = x / 10;
		num_digits++;
	}

	return num_digits;
}

struct IntPair split_int_in_two(x, n) {
	/* Input: x is an n-digit number
	   Output: An IntPair with x split in two halves based on its
				decimal representation.
	   Caveat: In the special case of n=1, returns a pair with 0, x.
	   Example: 1234 -> 12, 34
	   ASSUMES: n is even, or n is 1. */
	
	int left_half;
	int right_half;

	if (n == 1) {
		/* If there is an attempt to split a single digit,
		   simply copy the digit to the right half and put a zero on the right. 
		   EXAMPLE: 2 -> 0, 2 */
		left_half = 0;
		right_half = x;
	}
	else {
		/* Otherwise, just split the int in the middle. */
		int modn2 = int_pow(10, (n / 2));
		left_half = x / modn2;
		right_half = x % modn2;
	}

	struct IntPair x_halves;
	x_halves.left = left_half;
	x_halves.right = right_half;

	return x_halves;
}

int rec_int_mult(x, y) {
	/** Input: two n-digit positive integers x and y.
        Output: the product x · y.
        Assumption: n is a power of 2. */

	int nx = number_of_digits(x);
	int ny = number_of_digits(y);

	if (nx == 1 && ny == 1) {
		return x * y;
	}

	else {
		// Get the halves of x and y.
		struct IntPair ab = split_int_in_two(x, nx);
		struct IntPair cd = split_int_in_two(y, ny);

		int a = ab.left;
		int b = ab.right;
		int c = cd.left;
		int d = cd.right;

		// Recursively compute the products of the parts.
		int ac = rec_int_mult(a, c);
		int ad = rec_int_mult(a, d);
		int bc = rec_int_mult(b, c);
		int bd = rec_int_mult(b, d);

		return int_pow(10, nx) * ac + int_pow(10, nx / 2) * (ad + bc) + bd;
	}
}

void main() {
	
	for (int i = 0; i < 10; i++) {
		for (int j = 0; j < 10; j++) {
			if (rec_int_mult(i, j) != i * j) {
				printf("Error with numbers %d and %d.\n", i, j);
				printf("Rec_int_mult result: %d\n", rec_int_mult(i, j));
				printf("Actual value: %d\n", i * j);
				return -1;
			}
		}
	}
	for (int i = 10; i < 100; i++) {
		for (int j = 10; j < 100; j++) {
			if (rec_int_mult(i, j) != i * j) {
				printf("Error with numbers %d and %d.\n", i, j);
				printf("Rec_int_mult result: %d\n", rec_int_mult(i, j));
				printf("Actual value: %d\n", i * j);
				return -1;
			}
		}
	}
	for (int i = 1000; i < 10000; i++) {
		for (int j = 1000; j < 10000; j++) {
			if (rec_int_mult(i, j) != i * j) {
				printf("Error with numbers %d and %d.\n", i, j);
				printf("Rec_int_mult result: %d\n", rec_int_mult(i, j));
				printf("Actual value: %d\n", i * j);
				return -1;
			}
		}
	}
	printf("Everything is fine!");
}