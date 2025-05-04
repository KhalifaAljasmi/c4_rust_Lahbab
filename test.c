// Test file with all supported features

// Function with integer parameters
int add(int a, int b) {
    return a + b;
}

int main() {
    // Integer variables - declare first, then initialize
    int x = 10;
    int y = 20;
    
    // Direct while loop
    int count = 1;
    while (count <= 3) {
        printf("looping: %d\n", count);
        count = count + 1;
    }
    
    // Test if-else directly
    if (x < y) {
        printf("x is less than y\n");
    } else {
        printf("x is greater than or equal to y\n");
    }
    
    int result = add(x, y);
    printf("x + y = %d\n", result);

    return 0;
}